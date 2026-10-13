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
        | Other of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Transaction_monitoring
    | Rescreening
    | Adverse_media
    | Document_expiry
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Resolution = struct
  module Stable = struct
    module V1 = struct
      type t =
        | No_action
        | Risk_rerated
        | Edd_performed
        | Reported
        | Exit_relationship
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | No_action
    | Risk_rerated
    | Edd_performed
    | Reported
    | Exit_relationship
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Alert = struct
  type t =
    { id : Alert_id.t
    ; raised_at : Timestamp.t
    ; source : Source.t
    ; severity : Risk.Level.t
    ; summary : string
    ; closed : (Resolution.t * Operator_id.t * Timestamp.t) option
    }
  [@@deriving compare, equal, hash, sexp]

  let is_open t = Option.is_none t.closed
end
