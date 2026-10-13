(** A customer's due-diligence case: the aggregate that events are applied
    to. It covers customer due diligence (identity steps, tier), the
    risk-based rating and CDD level, enhanced due diligence (EDD),
    screening with four-eyes discounting, ongoing monitoring, investigations
    and FIU reporting, periodic review, and record retention.

    The case is derived state. Persist the {!Event.t} stream and rebuild with
    {!of_events}; that is why this type has no [Stable] module.

    [apply] never raises. An illegal event returns an error and leaves the
    case unchanged (the event is not recorded as applied, so the same event
    keeps failing the same way). An event whose id was already applied is a
    no-op; the same id with different content is an error. Every applied
    event adds at least one {!Audit} entry with its actor, time and note; a
    confirmed sanctions hit adds a second one for the automatic block. *)

open! Core

module Policy : sig
  module Review_intervals : sig
    type t =
      { low : Time_ns.Span.t
      ; medium : Time_ns.Span.t
      ; high : Time_ns.Span.t
      }
    [@@deriving compare, equal, sexp]

    val for_band : t -> Risk.Band.t -> Time_ns.Span.t
  end

  type t =
    { rules : Tier.Rules.t
    ; approval_tier : Tier.t (** verified tier needed before [Approve] *)
    ; edd_tier : Tier.t (** verified tier needed before [Approve] under EDD *)
    ; require_screening : bool (** at least one screening run before [Approve] *)
    ; require_risk_assessment : bool (** a risk band before [Approve] *)
    ; edd_requires_senior_approval : bool
    ; allow_simplified : bool (** [Cdd_level.Simplified] for low-risk fiat customers *)
    ; review_intervals : Review_intervals.t (** periodic review, by band *)
    ; retention : Time_ns.Span.t (** after the relationship ends or the refusal *)
    }
  [@@deriving compare, equal, sexp]

  (** Default rules; approval at [T2], EDD at [T3] with senior approval;
      screening and a risk band required; simplified DD allowed; reviews
      every 5 / 3 / 1 years for Low / Medium / High (AMLR Art. 26 caps are 5
      and 1); retention 5 years (FATF R.11, AMLR Art. 77). Years are 365
      days. *)
  val default : t
end

module Follow_up : sig
  (** Work the case needs now, for schedulers and operator queues. Most of
      it is restricted information; see {!Visibility}. *)
  type t =
    | Risk_assessment_needed (** none yet, or screening found what the inputs miss *)
    | Edd_outstanding of
        { triggers : Risk.Edd_trigger.t list
        ; missing_steps : Verification.Step.Set.t
        ; senior_approval : bool (** still needed *)
        }
    | Screening_review of Screening_id.t list
    | Open_alerts of Monitoring.Alert_id.t list
    | Open_investigations of Investigation.Investigation_id.t list
    | Fiu_request_due of (Investigation.Request_id.t * Date.t) list
    | Periodic_review_due of Timestamp.t
    (** raise an [Overdue_review] alert when this is past *)
    | Steps_lapsed of Verification.Step.t list
    | Retention_ended of Timestamp.t (** PII may now be erased *)
  [@@deriving compare, equal, sexp]
end

module Customer_view : sig
  (** What the customer (and customer-facing APIs) may be shown. It carries
      no risk band, screening, alert, investigation or report information:
      a blocked case shows [Under_review] and nothing else (tipping-off). *)

  type status =
    | In_progress
    | Action_required
    | Under_review
    | Active
    | Declined
    | Closed
  [@@deriving compare, equal, sexp]

  type t =
    { status : status
    ; tier : Tier.t
    ; steps_to_complete : Verification.Step.t list
    ; fields_to_resubmit : Person.Field.t list
    }
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
val investigations : t -> Investigation.t list
val fiu_requests : t -> Investigation.Fiu_request.t list
val audit : t -> Audit.t
val has_applied : t -> Event_id.t -> bool

(** The latest risk assessment and its band (after any override). *)
val risk : t -> Risk.Assessment.t option

val band : t -> Risk.Band.t option

(** The tier the verified steps reach, whatever the status. *)
val verified_tier : t -> Tier.t

(** The tier the customer may use now: [T0] when rejected, blocked or
    closed, the verified tier when approved, and at most [T1] while the case
    is open (contact details need no review, identity does). *)
val tier : t -> Tier.t

val required_for : t -> Tier.t -> Verification.Step.Set.t
val missing_for : t -> Tier.t -> Verification.Step.Set.t

(** Missing steps for the next tier above {!verified_tier}. *)
val missing_steps : t -> Verification.Step.Set.t

(** EDD triggers on a date: from the latest risk inputs (PEP within its EDD
    period, high-risk third country, self-hosted exposure, confirmed adverse
    media, manual reasons) plus [Unusual_transaction] while a
    transaction-monitoring alert is escalated. *)
val edd_triggers : t -> now:Timestamp.t -> Risk.Edd_trigger.t list

(** [Enhanced] for any trigger or a [High] band; see {!Cdd_level.decide}.
    [Standard] before any assessment. *)
val cdd_level : t -> now:Timestamp.t -> Cdd_level.t

(** Why [Approve] would fail at [now] (status aside), or [Ok ()]. *)
val can_approve : t -> now:Timestamp.t -> unit Or_error.t

(** Last approval or completed periodic review. *)
val last_review_at : t -> Timestamp.t option

(** [last_review_at] plus the interval for the current band, while the case
    is approved and rated; a band change moves it. *)
val next_review_due : t -> Timestamp.t option

val closed_at : t -> Timestamp.t option
val refused_at : t -> Timestamp.t option
val legal_hold : t -> Retention.Legal_hold.t option

(** See {!Retention.retain_until}. *)
val retain_until : t -> Timestamp.t option

val follow_ups : t -> now:Timestamp.t -> Follow_up.t list
val customer_view : t -> Customer_view.t

(** The product's limits for the case's current {!tier}. *)
val limits : (module Tier.Limits with type limits = 'l) -> t -> 'l
