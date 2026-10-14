(** Something that happened to a business verification case. Events are the persisted
    record (bin_io via [Stable]); a {!Kyb_case} is rebuilt by applying them in order.

    [id] makes application idempotent: for provider events use the provider's event or
    delivery id, so a redelivered webhook is a no-op. *)

open! Core

module Kind : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Opened of Legal_entity.Stable.V1.t
        | Entity_updated of Legal_entity.Stable.V1.t
        | Parties_set of Kyb_party.Stable.V1.t list
        | Party_case_changed of
            { party : Customer_id.Stable.V1.t
            ; status : Status.Stable.V1.t
            }
        | Document_added of Kyb_document.Stable.V1.t
        | Document_removed of Document_id.Stable.V1.t
        | Source_of_funds_declared of Source_of_funds.Stable.V1.t
        | Status_changed of
            { transition : Kyb_status.Transition.Stable.V1.t
            ; round : int option
            }
        | Risk_assessed of Kyb_risk.Assessment.Stable.V1.t
        | Senior_approval
        | Periodic_review_completed
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Opened of Legal_entity.t
    | Entity_updated of Legal_entity.t (** the whole entity again *)
    | Parties_set of Kyb_party.t list (** replaces the list *)
    | Party_case_changed of
        { party : Customer_id.t
        ; status : Status.t
        }
    (** the linked person's own case moved; reported by the platform *)
    | Document_added of Kyb_document.t
    | Document_removed of Document_id.t
    | Source_of_funds_declared of Source_of_funds.t
    | Status_changed of
        { transition : Kyb_status.Transition.t
        ; round : int option
        (** the submission round a provider decision refers to; a decision about an
            older round is refused. [Submit] may name the round it opens. *)
        }
    | Risk_assessed of Kyb_risk.Assessment.t
    | Senior_approval (** EDD sign-off; the note is the rationale *)
    | Periodic_review_completed
  [@@deriving compare, equal, hash, sexp]
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Event_id.Stable.V1.t
      ; entity_id : Entity_id.Stable.V1.t
      ; at : Timestamp.Stable.V1.t
      ; actor : Actor.Stable.V1.t
      ; note : string option
      ; kind : Kind.Stable.V1.t
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : Event_id.t
  ; entity_id : Entity_id.t
  ; at : Timestamp.t
  ; actor : Actor.t
  ; note : string option (** why, in the actor's words; reviewers' notes go here *)
  ; kind : Kind.t
  }
[@@deriving compare, equal, hash, sexp]
