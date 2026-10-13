(** The audit trail of a case: one entry per change an event caused, with who, when and
    why. Entries hold no PII: a person update records which fields changed, never their
    values. Each change has a {!Visibility.Class.t}; {!for_audience} filters the trail so
    that, for example, a customer never sees monitoring or reporting entries. *)

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
            ; reason : Screening.Screen_reason.Stable.V1.t
            ; hits : int
            }
        | Hit_reviewed of
            { screening_id : Screening_id.Stable.V1.t
            ; entry_id : string
            ; action : Screening.Review_action.Stable.V1.t
            }
        | Risk_assessed of
            { score : int
            ; computed_band : Risk.Band.Stable.V1.t
            ; band : Risk.Band.Stable.V1.t
            ; model_version : string
            }
        | Senior_approval
        | Periodic_review_completed
        | Alert_raised of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; severity : Risk.Band.Stable.V1.t
            }
        | Alert_decided of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; decision : Monitoring.Decision.Stable.V1.t
            }
        | Investigation_opened of Investigation.Investigation_id.Stable.V1.t
        | Investigation_moved of
            { investigation_id : Investigation.Investigation_id.Stable.V1.t
            ; state : Investigation.Case_state.Stable.V1.t
            }
        | Report_changed of
            { investigation_id : Investigation.Investigation_id.Stable.V1.t
            ; state : Investigation.Report_state.Stable.V1.t
            }
        | Fiu_request_received of
            { request_id : Investigation.Request_id.Stable.V1.t
            ; due : Date.Stable.V1.t
            }
        | Fiu_request_answered of Investigation.Request_id.Stable.V1.t
        | Legal_hold_set of { until : Timestamp.Stable.V1.t }
        | Legal_hold_released
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
        ; reason : Screening.Screen_reason.t
        ; hits : int
        }
    | Hit_reviewed of
        { screening_id : Screening_id.t
        ; entry_id : string
        ; action : Screening.Review_action.t
        }
    | Risk_assessed of
        { score : int
        ; computed_band : Risk.Band.t
        ; band : Risk.Band.t
        ; model_version : string
        }
    | Senior_approval
    | Periodic_review_completed
    | Alert_raised of
        { alert_id : Monitoring.Alert_id.t
        ; severity : Risk.Band.t
        }
    | Alert_decided of
        { alert_id : Monitoring.Alert_id.t
        ; decision : Monitoring.Decision.t
        }
    | Investigation_opened of Investigation.Investigation_id.t
    | Investigation_moved of
        { investigation_id : Investigation.Investigation_id.t
        ; state : Investigation.Case_state.t
        }
    | Report_changed of
        { investigation_id : Investigation.Investigation_id.t
        ; state : Investigation.Report_state.t
        }
    | Fiu_request_received of
        { request_id : Investigation.Request_id.t
        ; due : Date.t
        }
    | Fiu_request_answered of Investigation.Request_id.t
    | Legal_hold_set of { until : Timestamp.t }
    | Legal_hold_released
  [@@deriving compare, equal, hash, sexp]

  val visibility : t -> Visibility.Class.t
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

(** Oldest first, only what the audience may see; the customer's copy has no [why] notes,
    and status changes into or out of [Blocked] are restricted. *)
val for_audience : t -> Visibility.Audience.t -> Entry.t list

val length : t -> int
