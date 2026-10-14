open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
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
    | Entity_updated of Legal_entity.t
    | Parties_set of Kyb_party.t list
    | Party_case_changed of
        { party : Customer_id.t
        ; status : Status.t
        }
    | Document_added of Kyb_document.t
    | Document_removed of Document_id.t
    | Source_of_funds_declared of Source_of_funds.t
    | Status_changed of
        { transition : Kyb_status.Transition.t
        ; round : int option
        }
    | Risk_assessed of Kyb_risk.Assessment.t
    | Senior_approval
    | Periodic_review_completed
  [@@deriving compare, equal, hash, sexp]
end

module Stable = struct
  module V1 = struct
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
  ; note : string option
  ; kind : Kind.t
  }
[@@deriving compare, equal, hash, sexp]
