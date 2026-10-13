(** Something that happened to a case. Events are the persisted record (bin_io via
    [Stable]); a {!Kyc_case} is rebuilt by applying them in order.

    [id] makes application idempotent: for provider events use the provider's event or
    delivery id, so a redelivered webhook is a no-op. *)

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
            ; outcome : Screening.Outcome.Stable.V1.t
            }
        | Screening_reviewed of
            { screening_id : Screening_id.Stable.V1.t
            ; decision : Screening.Decision.Stable.V1.t
            }
        | Risk_assessed of
            { factors : Risk.Factors.Stable.V1.t
            ; override : Risk.Override.Stable.V1.t option
            }
        | Senior_approval
        | Periodic_review_completed
        | Alert_raised of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; source : Monitoring.Source.Stable.V1.t
            ; severity : Risk.Level.Stable.V1.t
            ; summary : string
            }
        | Alert_closed of
            { alert_id : Monitoring.Alert_id.Stable.V1.t
            ; resolution : Monitoring.Resolution.Stable.V1.t
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
    | Risk_assessed of
        { factors : Risk.Factors.t
        ; override : Risk.Override.t option
        }
    | Senior_approval
    (** EDD sign-off by senior management (FATF R.12 for PEPs); the event's [note] carries
        the rationale *)
    | Periodic_review_completed (** needs a risk assessment since the last review *)
    | Alert_raised of
        { alert_id : Monitoring.Alert_id.t
        ; source : Monitoring.Source.t
        ; severity : Risk.Level.t
        ; summary : string
        }
    | Alert_closed of
        { alert_id : Monitoring.Alert_id.t
        ; resolution : Monitoring.Resolution.t
        }
  [@@deriving compare, equal, hash, sexp, quickcheck]
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
[@@deriving compare, equal, hash, sexp, quickcheck]
