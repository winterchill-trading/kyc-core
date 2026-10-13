(** A customer's verification case: the aggregate that events are applied to.

    The case is derived state. Persist the {!Event.t} stream and rebuild with
    {!of_events}; that is why this type has no [Stable] module.

    [apply] never raises. An illegal event returns an error and leaves the case unchanged
    (the event is not recorded as applied, so the same event keeps failing the same way).
    An event whose id was already applied is a no-op; the same id with different content
    is an error. *)

open! Core

module Policy : sig
  type t =
    { rules : Tier.Rules.t
    ; approval_tier : Tier.t (** verified tier needed before [Approve] *)
    ; require_screening : bool (** at least one screening run before [Approve] *)
    }
  [@@deriving compare, equal, sexp]

  (** Default rules, approval at [T2], screening required. *)
  val default : t
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
val audit : t -> Audit.t
val has_applied : t -> Event_id.t -> bool

(** The tier the verified steps reach, whatever the status. *)
val verified_tier : t -> Tier.t

(** The tier the customer may use now: [T0] when rejected or blocked, the verified tier
    when approved, and at most [T1] while the case is open (contact details need no
    review, identity does). *)
val tier : t -> Tier.t

(** Steps the policy requires for a tier. *)
val required_for : t -> Tier.t -> Verification.Step.Set.t

(** Required steps for the tier that are not verified yet. *)
val missing_for : t -> Tier.t -> Verification.Step.Set.t

(** Missing steps for the next tier above {!verified_tier}; empty at the top tier. *)
val missing_steps : t -> Verification.Step.Set.t

(** Why [Approve] would fail now (status aside), or [Ok ()]. *)
val can_approve : t -> unit Or_error.t

(** The product's limits for the case's current {!tier}. *)
val limits : (module Tier.Limits with type limits = 'l) -> t -> 'l
