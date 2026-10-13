(** Something that happened to a case. Events are the persisted record (bin_io
    via [Stable]); a {!Kyc_case} is rebuilt by applying them in order.

    [id] makes application idempotent: for provider events use the
    provider's event or delivery id, so a redelivered webhook is a no-op. *)

open! Core

module Kind : sig
  module Stable : sig
    module V1 : sig
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
            ; reason : Screening.Screen_reason.Stable.V1.t
            ; hits : Screening.Hit.Stable.V1.t list
            }
        | Hit_reviewed of
            { screening_id : Screening_id.Stable.V1.t
            ; entry_id : string
            ; action : Screening.Review_action.Stable.V1.t
            }
        | Risk_assessed of Risk.Assessment.Stable.V1.t
        | Senior_approval
        | Periodic_review_completed
        | Alert_raised of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; source : Monitoring.Source.Stable.V1.t
            ; severity : Risk.Band.Stable.V1.t
            ; summary : string
            }
        | Alert_decided of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; decision : Monitoring.Decision.Stable.V1.t
            }
        | Investigation_opened of
            { investigation_id : Investigation.Investigation_id.Stable.V1.t
            ; alerts : Monitoring.Alert_id.Stable.V1.t list
            }
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
            ; received_on : Date.Stable.V1.t
            }
        | Fiu_request_answered of
            { request_id : Investigation.Request_id.Stable.V1.t
            ; answered_on : Date.Stable.V1.t
            }
        | Legal_hold_set of Retention.Legal_hold.Stable.V1.t
        | Legal_hold_released
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
        ; reason : Screening.Screen_reason.t
        ; hits : Screening.Hit.t list
        }
    | Hit_reviewed of
        { screening_id : Screening_id.t
        ; entry_id : string
        ; action : Screening.Review_action.t
        }
    | Risk_assessed of Risk.Assessment.t (** as computed by [Risk.assess] (and [Risk.override]) *)
    | Senior_approval (** EDD sign-off by senior management (FATF R.12); the note is the rationale *)
    | Periodic_review_completed (** needs a risk assessment since the last review *)
    | Alert_raised of
        { alert_id : Monitoring.Alert_id.t
        ; source : Monitoring.Source.t
        ; severity : Risk.Band.t
        ; summary : string
        }
    | Alert_decided of
        { alert_id : Monitoring.Alert_id.t
        ; decision : Monitoring.Decision.t
        }
    | Investigation_opened of
        { investigation_id : Investigation.Investigation_id.t
        ; alerts : Monitoring.Alert_id.t list
        }
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
        ; received_on : Date.t
        }
    | Fiu_request_answered of
        { request_id : Investigation.Request_id.t
        ; answered_on : Date.t
        }
    | Legal_hold_set of Retention.Legal_hold.t
    | Legal_hold_released
  [@@deriving compare, equal, hash, sexp]
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Event_id.Stable.V1.t
      ; customer_id : Customer_id.Stable.V1.t
      ; at : Timestamp.Stable.V1.t
      ; actor : Actor.Stable.V1.t
      ; note : string option (** why, in the actor's words *)
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
[@@deriving compare, equal, hash, sexp]
