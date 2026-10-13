open! Core

(* See [Customer_id] for the caller identity. *)
module Alert_id =
  (val String_id.make
         ~module_name:"Kyc_core.Monitoring.Alert_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "8f3d2c71-6a0e-4b9e-9d2a-51c7e4b0a6f3")
         ())

module Source = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Transaction_monitoring
        | Rescreening
        | Adverse_media
        | Document_expiry
        | Overdue_review
        | Other of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Transaction_monitoring
    | Rescreening
    | Adverse_media
    | Document_expiry
    | Overdue_review
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Decision = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Closed_no_action
        | Escalated
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Closed_no_action
    | Escalated
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Alert_state = struct
  type t =
    | Open
    | Closed_no_action of
        { by : Operator_id.t
        ; at : Timestamp.t
        }
    | Escalated of
        { by : Operator_id.t
        ; at : Timestamp.t
        }
  [@@deriving compare, equal, hash, sexp]
end

module Alert = struct
  type t =
    { id : Alert_id.t
    ; raised_at : Timestamp.t
    ; source : Source.t
    ; severity : Risk.Band.t
    ; summary : string
    ; state : Alert_state.t
    }
  [@@deriving compare, equal, hash, sexp]

  let is_open t =
    match t.state with
    | Open -> true
    | Closed_no_action _ | Escalated _ -> false
  ;;

  let is_active t =
    match t.state with
    | Open | Escalated _ -> true
    | Closed_no_action _ -> false
  ;;

  let decide t (decision : Decision.t) ~by ~at =
    match t.state, decision with
    | Open, Closed_no_action | Escalated _, Closed_no_action ->
      Ok { t with state = Closed_no_action { by; at } }
    | Open, Escalated -> Ok { t with state = Escalated { by; at } }
    | Escalated _, Escalated | Closed_no_action _, _ ->
      Or_error.error_s
        [%message
          "illegal alert decision"
            ~alert:(t.id : Alert_id.t)
            ~state:(t.state : Alert_state.t)
            (decision : Decision.t)]
  ;;
end
