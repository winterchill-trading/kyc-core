open! Core

module Change = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Opened
        | Person_updated of Person.Field.Stable.V1.t list
        | Step of
            { step : Verification.Step.Stable.V1.t
            ; from : Verification.State.Stable.V1.t
            ; to_ : Verification.State.Stable.V1.t
            }
        | Status of
            { from : Status.Stable.V1.t
            ; to_ : Status.Stable.V1.t
            }
        | Screened of
            { screening_id : Screening_id.Stable.V1.t
            ; hits : int
            }
        | Screening_reviewed of
            { screening_id : Screening_id.Stable.V1.t
            ; decision : Screening.Decision.Stable.V1.t
            }
        | Risk_assessed of
            { computed : Risk.Level.Stable.V1.t
            ; rating : Risk.Level.Stable.V1.t
            }
        | Senior_approval
        | Periodic_review_completed
        | Alert_raised of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; severity : Risk.Level.Stable.V1.t
            }
        | Alert_closed of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; resolution : Monitoring.Resolution.Stable.V1.t
            }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Opened
    | Person_updated of Person.Field.t list
    | Step of
        { step : Verification.Step.t
        ; from : Verification.State.t
        ; to_ : Verification.State.t
        }
    | Status of
        { from : Status.t
        ; to_ : Status.t
        }
    | Screened of
        { screening_id : Screening_id.t
        ; hits : int
        }
    | Screening_reviewed of
        { screening_id : Screening_id.t
        ; decision : Screening.Decision.t
        }
    | Risk_assessed of
        { computed : Risk.Level.t
        ; rating : Risk.Level.t
        }
    | Senior_approval
    | Periodic_review_completed
    | Alert_raised of
        { alert_id : Monitoring.Alert_id.t
        ; severity : Risk.Level.t
        }
    | Alert_closed of
        { alert_id : Monitoring.Alert_id.t
        ; resolution : Monitoring.Resolution.t
        }
  [@@deriving compare, equal, hash, sexp]
end

module Entry = struct
  module Stable = struct
    module V1 = struct
      type t =
        { at : Timestamp.Stable.V1.t
        ; actor : Actor.Stable.V1.t
        ; event_id : Event_id.Stable.V1.t
        ; change : Change.Stable.V1.t
        ; why : string option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { at : Timestamp.t
    ; actor : Actor.t
    ; event_id : Event_id.t
    ; change : Change.t
    ; why : string option
    }
  [@@deriving compare, equal, hash, sexp]
end

(* Newest first, so [add] is O(1); [entries] reverses. *)
type t = Entry.t list [@@deriving compare, equal, sexp_of]

let empty = []
let add t entry = entry :: t
let entries t = List.rev t
let length = List.length
