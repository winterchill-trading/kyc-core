open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Opened of Person.Stable.V1.t
        | Person_updated of Person.Stable.V1.t
        | Step_changed of
            { step : Verification.Step.Stable.V1.t
            ; state : Verification.State.Stable.V1.t
            }
        | Status_changed of Status.Transition.Stable.V1.t
        | Screened of
            { screening_id : Screening_id.Stable.V1.t
            ; outcome : Screening.Outcome.Stable.V1.t
            }
        | Screening_reviewed of
            { screening_id : Screening_id.Stable.V1.t
            ; decision : Screening.Decision.Stable.V1.t
            }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Opened of Person.t
    | Person_updated of Person.t
    | Step_changed of
        { step : Verification.Step.t
        ; state : Verification.State.t
        }
    | Status_changed of Status.Transition.t
    | Screened of
        { screening_id : Screening_id.t
        ; outcome : Screening.Outcome.t
        }
    | Screening_reviewed of
        { screening_id : Screening_id.t
        ; decision : Screening.Decision.t
        }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Event_id.Stable.V1.t
      ; customer_id : Customer_id.Stable.V1.t
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
  ; customer_id : Customer_id.t
  ; at : Timestamp.t
  ; actor : Actor.t
  ; note : string option
  ; kind : Kind.t
  }
[@@deriving compare, equal, hash, sexp, quickcheck]
