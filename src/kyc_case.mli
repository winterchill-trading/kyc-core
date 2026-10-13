(** A customer's due-diligence case: the aggregate that events are applied to. It covers
    customer due diligence (identity steps, tier), the risk-based rating and enhanced due
    diligence (EDD), screening, ongoing monitoring, periodic review and record retention.

    The case is derived state. Persist the {!Event.t} stream and rebuild with
    {!of_events}; that is why this type has no [Stable] module.

    [apply] never raises. An illegal event returns an error and leaves the case unchanged
    (the event is not recorded as applied, so the same event keeps failing the same way).
    An event whose id was already applied is a no-op; the same id with different content
    is an error. Every applied event adds one {!Audit} entry with its actor, time and
    note. *)

open! Core

module Policy : sig
  module Review_intervals : sig
    type t =
      { low : Time_ns.Span.t
      ; medium : Time_ns.Span.t
      ; high : Time_ns.Span.t
      }
    [@@deriving compare, equal, sexp]

    val for_rating : t -> Risk.Level.t -> Time_ns.Span.t
  end

  type t =
    { rules : Tier.Rules.t
    ; approval_tier : Tier.t (** verified tier needed before [Approve] *)
    ; edd_tier : Tier.t (** verified tier needed before [Approve] when EDD applies *)
    ; require_screening : bool (** at least one screening run before [Approve] *)
    ; require_risk_assessment : bool (** a risk rating before [Approve] *)
    ; edd_requires_senior_approval : bool
    ; review_intervals : Review_intervals.t (** periodic review, by rating *)
    ; retention : Time_ns.Span.t (** keep records this long after the relationship ends *)
    }
  [@@deriving compare, equal, sexp]

  (** Default rules; approval at [T2], EDD at [T3] with senior approval; screening and a
      risk rating required; reviews every 5 years (low), 3 years (medium) and 1 year
      (high); retention 5 years (FATF R.11's minimum; local law may require longer). Years
      are 365 days. *)
  val default : t
end

module Follow_up : sig
  (** Work the case needs now, for schedulers and operator queues. *)
  type t =
    | Risk_assessment_needed (** none yet, or screening found what the factors miss *)
    | Edd_outstanding of
        { triggers : Risk.Edd_trigger.t list
        ; missing_steps : Verification.Step.Set.t
        ; senior_approval : bool (** still needed *)
        }
    | Screening_review of Screening_id.t list
    | Open_alerts of Monitoring.Alert_id.t list
    | Periodic_review_due of Timestamp.t
    | Steps_lapsed of Verification.Step.t list
    (** verified before, expired or failed now *)
    | Retention_ended of Timestamp.t (** records may now be deleted *)
  [@@deriving compare, equal, sexp]
end

type t [@@deriving compare, equal, sexp_of]

val create : ?policy:Policy.t -> Customer_id.t -> t
val apply : t -> Event.t -> t Or_error.t

(** Applies in list order and stops at the first error, naming the event. *)
val of_events : ?policy:Policy.t -> Customer_id.t -> Event.t list -> t Or_error.t

val customer_id : t -> Customer_id.t
val policy : t -> Policy.t

(** [None] until the [Opened] event. *)
val person : t -> Person.t option

val status : t -> Status.t
val step : t -> Verification.Step.t -> Verification.State.t
val steps : t -> (Verification.Step.t * Verification.State.t) list
val verified_steps : t -> Verification.Step.Set.t
val screenings : t -> Screening.t list
val alerts : t -> Monitoring.Alert.t list
val audit : t -> Audit.t
val has_applied : t -> Event_id.t -> bool

(** The latest risk assessment. *)
val risk : t -> Risk.t option

(** The tier the verified steps reach, whatever the status. *)
val verified_tier : t -> Tier.t

(** The tier the customer may use now: [T0] when rejected, blocked or closed, the verified
    tier when approved, and at most [T1] while the case is open (contact details need no
    review, identity does). *)
val tier : t -> Tier.t

(** Steps the policy requires for a tier. *)
val required_for : t -> Tier.t -> Verification.Step.Set.t

(** Required steps for the tier that are not verified yet. *)
val missing_for : t -> Tier.t -> Verification.Step.Set.t

(** Missing steps for the next tier above {!verified_tier}; empty at the top tier. *)
val missing_steps : t -> Verification.Step.Set.t

(** EDD triggers from the latest risk assessment plus any open high-severity monitoring
    alert. Empty means standard due diligence. *)
val edd_triggers : t -> Risk.Edd_trigger.t list

(** Why [Approve] would fail now (status aside), or [Ok ()]. *)
val can_approve : t -> unit Or_error.t

(** Last approval or completed periodic review. *)
val last_review_at : t -> Timestamp.t option

(** [last_review_at] plus the interval for the current rating, while the case is approved
    and rated. *)
val next_review_due : t -> Timestamp.t option

(** When the relationship was closed. *)
val closed_at : t -> Timestamp.t option

(** [closed_at] plus the policy's retention period. *)
val retain_until : t -> Timestamp.t option

val follow_ups : t -> now:Timestamp.t -> Follow_up.t list

(** The product's limits for the case's current {!tier}. *)
val limits : (module Tier.Limits with type limits = 'l) -> t -> 'l
