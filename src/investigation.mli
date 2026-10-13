(** Investigations of escalated alerts and suspicious activity reports to the financial
    intelligence unit (FIU) (FATF R.20; AMLR Art. 69).

    {v
      case:   Open -> Investigating -> Pending_decision -> Closed_no_report
                                                       -> Closed_reported (report filed)
      report: Draft -> Approved -> Filed -> Acknowledged | Rejected_by_fiu
              Rejected_by_fiu -> Draft (corrected and resubmitted)
    v}

    Report approval needs an operator other than the one who drafted it. FIU information
    requests are due 5 working days after receipt (AMLR Art. 69(1)(b)). Everything here is
    restricted (tipping-off): see {!Visibility}. *)

open! Core
module Investigation_id : String_id.S
module Request_id : String_id.S

module Case_state : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Open
        | Investigating
        | Pending_decision
        | Closed_no_report
        | Closed_reported
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Open
    | Investigating
    | Pending_decision
    | Closed_no_report
    | Closed_reported
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Report_state : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Draft
        | Approved
        | Filed
        | Acknowledged of { fiu_ref : string }
        | Rejected_by_fiu of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Draft
    | Approved
    | Filed
    | Acknowledged of { fiu_ref : string }
    | Rejected_by_fiu of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

type t =
  { id : Investigation_id.t
  ; opened_at : Timestamp.t
  ; alerts : Monitoring.Alert_id.t list
  ; state : Case_state.t
  ; report : (Report_state.t * Operator_id.t) option (** state and drafter *)
  }
[@@deriving compare, equal, hash, sexp]

val open_
  :  id:Investigation_id.t
  -> at:Timestamp.t
  -> alerts:Monitoring.Alert_id.t list
  -> t

val move : t -> Case_state.t -> t Or_error.t
val report : t -> Report_state.t -> by:Operator_id.t -> t Or_error.t
val is_closed : t -> bool

module Fiu_request : sig
  type t =
    { id : Request_id.t
    ; received_on : Date.t
    ; due : Date.t
    ; answered_on : Date.t option
    }
  [@@deriving compare, equal, hash, sexp]

  (** [due] is 5 working days after [received_on], skipping weekends and [is_holiday]
      (default: none; pass the FIU country's calendar). *)
  val create
    :  ?is_holiday:(Date.t -> bool)
    -> id:Request_id.t
    -> received_on:Date.t
    -> unit
    -> t

  val overdue : t -> on:Date.t -> bool
end
