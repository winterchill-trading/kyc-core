(** The audit trail of a case: one entry per change an event caused, with who, when and
    why. Entries hold no PII: a person update records which fields changed, never their
    values. *)

open! Core

module Change : sig
  module Stable : sig
    module V1 : sig
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

module Entry : sig
  module Stable : sig
    module V1 : sig
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

type t [@@deriving compare, equal, sexp_of]

val empty : t
val add : t -> Entry.t -> t

(** Oldest first. *)
val entries : t -> Entry.t list

val length : t -> int
