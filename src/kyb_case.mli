(** A business applicant's due-diligence case (KYB): the legal entity, the people linked
    to it (each with their own {!Kyc_case}), beneficial ownership, required documents, the
    source-of-funds declaration, submission rounds with requests for more information,
    decisions, risk rating, EDD sign-off, periodic review and retention.

    The case is derived state: persist the {!Kyb_event.t} stream and rebuild with
    {!of_events}. It follows the same rules as {!Kyc_case}: [apply] never raises, an
    illegal event returns an error and leaves the case unchanged, an event id already
    applied is a no-op, and the same id with different content is an error.

    Rounds: the first [Submit] opens round 1 and every resubmission after a request for
    more information opens the next. A provider decision that names an older round is
    refused, so a late event about a superseded submission cannot overturn the current
    one. *)

open! Core

module Policy : sig
  type t =
    { requirements : Kyb_requirements.t
    ; owner_threshold_bps : int (** beneficial ownership threshold, 2500 *)
    ; require_risk_assessment : bool (** a risk band before [Approve] *)
    ; edd_requires_senior_approval : bool
    ; review_intervals : Kyc_case.Policy.Review_intervals.t
    ; retention : Time_ns.Span.t
    }
  [@@deriving compare, equal, sexp]

  (** {!Kyb_requirements.default}, 25 % (AMLR Art. 52), a risk band required, senior
      approval under EDD, the review intervals and retention of
      {!Kyc_case.Policy.default}. *)
  val default : t
end

module Decision : sig
  (** A reviewer's or provider's outcome for a round. *)
  type outcome =
    | Approved
    | More_information of Kyb_item.t list
    | Rejected of string
  [@@deriving compare, equal, sexp]

  type t =
    { round : int
    ; outcome : outcome
    ; at : Timestamp.t
    ; actor : Actor.t
    ; note : string option (** the reviewer's words *)
    }
  [@@deriving compare, equal, sexp]
end

module Follow_up : sig
  (** Work the case needs now, for schedulers and operator queues. *)
  type t =
    | Missing of Kyb_item.t list
    (** while editable: what submission still needs, then any requested items *)
    | Awaiting_decision of
        { round : int
        ; since : Timestamp.t
        }
    | Party_cases_pending of Customer_id.t list (** their KYC cases are not approved *)
    | Risk_assessment_needed
    | Edd_outstanding of
        { triggers : Kyb_risk.Edd_trigger.t list
        ; senior_approval : bool (** still needed *)
        }
    | Periodic_review_due of Timestamp.t
    | Retention_ended of Timestamp.t
  [@@deriving compare, equal, sexp]
end

module Applicant_view : sig
  (** What the applicant business (and applicant-facing APIs) may be shown. No risk band,
      no block reason, and no linked person whose own case is blocked: a blocked case
      shows [Under_review] and nothing else (tipping-off). *)
  type t =
    { status : Kyc_case.Customer_view.status
    ; round : int
    ; requested : Kyb_item.t list (** asked for in the latest request *)
    ; missing : Kyb_item.t list (** still needed before submission *)
    ; parties_pending : Customer_id.t list
    (** people who still have to complete verification *)
    ; reason : string option (** the rejection reason code *)
    }
  [@@deriving compare, equal, sexp]
end

type t [@@deriving compare, equal, sexp_of]

(** An empty case in [Draft]; the first event must be [Opened]. *)
val create : ?policy:Policy.t -> Entity_id.t -> t

(** Applies one event; see the rules above. *)
val apply : t -> Kyb_event.t -> t Or_error.t

(** Applies in list order and stops at the first error, naming the event. *)
val of_events : ?policy:Policy.t -> Entity_id.t -> Kyb_event.t list -> t Or_error.t

(** The entity the case is about. *)
val entity_id : t -> Entity_id.t

(** The policy the case was created with. *)
val policy : t -> Policy.t

(** [None] until the [Opened] event. *)
val entity : t -> Legal_entity.t option

(** The linked people, as last set. *)
val parties : t -> Kyb_party.t list

(** The latest reported status of each linked person's own case. *)
val party_status : t -> Customer_id.t -> Status.t option

(** Document metadata, by id. *)
val documents : t -> Kyb_document.t list

(** The latest declaration. *)
val source_of_funds : t -> Source_of_funds.t option

(** The current status. *)
val status : t -> Kyb_status.t

(** 0 before the first submission. *)
val round : t -> int

(** Time of the latest submission. *)
val submitted_at : t -> Timestamp.t option

(** Oldest first. *)
val decisions : t -> Decision.t list

(** The audit trail. *)
val audit : t -> Kyb_audit.t

(** Whether an event with this id was applied. *)
val has_applied : t -> Event_id.t -> bool

(** The latest risk assessment. *)
val risk : t -> Kyb_risk.Assessment.t option

(** Its band, after any override. *)
val band : t -> Risk.Band.t option

(** Beneficial owners under the policy's threshold; see {!Kyb_party.beneficial_owners}. *)
val beneficial_owners : t -> (Kyb_party.t * Kyb_party.Basis.t list) list

(** Documents the entity and its parties need under the policy. *)
val required_documents : t -> Kyb_document.Requirement.t list

(** What [Submit] still needs: entity fields, the parties (at least one director, or a
    trustee for a trust), beneficial owners, documents and the source-of-funds
    declaration. *)
val missing_for_submission : t -> Kyb_item.t list

(** People whose own KYC case must be approved and is not. *)
val parties_pending : t -> Customer_id.t list

(** EDD triggers of the latest assessment on [now]; none before one. *)
val edd_triggers : t -> now:Timestamp.t -> Kyb_risk.Edd_trigger.t list

(** [Enhanced] for any EDD trigger or a [High] band, else [Standard]; simplified due
    diligence is not offered to businesses here. *)
val cdd_level : t -> now:Timestamp.t -> Cdd_level.t

(** Why [Approve] would fail at [now] (status aside), or [Ok ()]: anything missing for
    submission, linked people not approved, no risk assessment, or EDD outstanding. *)
val can_approve : t -> now:Timestamp.t -> unit Or_error.t

(** Last approval or completed periodic review. *)
val last_review_at : t -> Timestamp.t option

(** [last_review_at] plus the interval for the current band, while approved and rated. *)
val next_review_due : t -> Timestamp.t option

(** When the relationship ended. *)
val closed_at : t -> Timestamp.t option

(** When the application was rejected. *)
val refused_at : t -> Timestamp.t option

(** See {!Retention.retain_until}; business cases carry no legal hold here. *)
val retain_until : t -> Timestamp.t option

(** Work the case needs at [now]. *)
val follow_ups : t -> now:Timestamp.t -> Follow_up.t list

(** The applicant's view; see {!Applicant_view}. *)
val applicant_view : t -> Applicant_view.t
