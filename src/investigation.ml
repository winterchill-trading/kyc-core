open! Core

(* See [Customer_id] for the caller identity. *)
module Investigation_id =
  (val String_id.make
         ~module_name:"Kyc_core.Investigation.Investigation_id"
         ~include_default_validation:true
         ~caller_identity:(Bin_prot.Shape.Uuid.of_string "2b6f9c3e-0d4a-4f8b-a1e7-6c5d3f9b8e21")
         ())

module Request_id =
  (val String_id.make
         ~module_name:"Kyc_core.Investigation.Request_id"
         ~include_default_validation:true
         ~caller_identity:(Bin_prot.Shape.Uuid.of_string "c4e1a7d2-9b3f-4e6a-8d0c-1f2e3a4b5c6d")
         ())

module Case_state = struct
  module Stable = struct
    module V1 = struct
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

module Report_state = struct
  module Stable = struct
    module V1 = struct
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
  ; report : (Report_state.t * Operator_id.t) option
  }
[@@deriving compare, equal, hash, sexp]

let open_ ~id ~at ~alerts = { id; opened_at = at; alerts; state = Open; report = None }

let is_closed t =
  match t.state with
  | Closed_no_report | Closed_reported -> true
  | Open | Investigating | Pending_decision -> false
;;

let filed t =
  match t.report with
  | Some ((Filed | Acknowledged _), _) -> true
  | Some ((Draft | Approved | Rejected_by_fiu _), _) | None -> false
;;

let move t (to_ : Case_state.t) =
  let illegal () =
    Or_error.error_s
      [%message
        "illegal investigation move" ~from:(t.state : Case_state.t) (to_ : Case_state.t)]
  in
  match t.state, to_ with
  | Open, Investigating | Investigating, Pending_decision -> Ok { t with state = to_ }
  | Pending_decision, Closed_no_report ->
    if Option.is_some t.report
    then Or_error.error_s [%message "a report exists; close as reported"]
    else Ok { t with state = to_ }
  | Pending_decision, Closed_reported ->
    if filed t
    then Ok { t with state = to_ }
    else Or_error.error_s [%message "the report must be filed before closing as reported"]
  | ( (Open | Investigating | Pending_decision | Closed_no_report | Closed_reported)
    , (Open | Investigating | Pending_decision | Closed_no_report | Closed_reported) ) ->
    illegal ()
;;

let report t (to_ : Report_state.t) ~by =
  let illegal () =
    Or_error.error_s
      [%message
        "illegal report change"
          ~from:(Option.map t.report ~f:fst : Report_state.t option)
          (to_ : Report_state.t)]
  in
  if is_closed t
  then Or_error.error_s [%message "investigation is closed"]
  else (
    match t.report, to_ with
    | None, Draft -> Ok { t with report = Some (Draft, by) }
    | Some (Draft, drafter), Approved ->
      if Operator_id.equal drafter by
      then Or_error.error_s [%message "a report is approved by someone other than its drafter"]
      else Ok { t with report = Some (Approved, drafter) }
    | Some (Approved, drafter), Filed -> Ok { t with report = Some (Filed, drafter) }
    | Some (Filed, drafter), ((Acknowledged _ | Rejected_by_fiu _) as s) ->
      Ok { t with report = Some (s, drafter) }
    | Some (Rejected_by_fiu _, _), Draft -> Ok { t with report = Some (Draft, by) }
    | _, _ -> illegal ())
;;

module Fiu_request = struct
  type t =
    { id : Request_id.t
    ; received_on : Date.t
    ; due : Date.t
    ; answered_on : Date.t option
    }
  [@@deriving compare, equal, hash, sexp]

  let create ?(is_holiday = fun _ -> false) ~id ~received_on () =
    { id
    ; received_on
    ; due = Date.add_business_days_rounding_forward received_on 5 ~is_holiday
    ; answered_on = None
    }
  ;;

  let overdue t ~on = Option.is_none t.answered_on && Date.( > ) on t.due
end
