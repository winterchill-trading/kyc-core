(** Ongoing monitoring (FATF R.10, ongoing due diligence): alerts raised by transaction
    monitoring, periodic re-screening or document expiry, and how each was closed.
    Transaction monitoring itself lives elsewhere; it feeds alerts in as events, and
    {!Kyc_case.follow_ups} feeds work back out. *)

open! Core
module Alert_id : String_id.S

module Source : sig
  module Stable : sig
    module V1 : sig
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

module Resolution : sig
  module Stable : sig
    module V1 : sig
      type t =
        | No_action
        | Risk_rerated (** a new risk assessment follows *)
        | Edd_performed
        | Reported (** reported to the authority; detail stays out of this record *)
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

module Alert : sig
  type t =
    { id : Alert_id.t
    ; raised_at : Timestamp.t
    ; source : Source.t
    ; severity : Risk.Level.t
    ; summary : string
    ; closed : (Resolution.t * Operator_id.t * Timestamp.t) option
    }
  [@@deriving compare, equal, hash, sexp]

  val is_open : t -> bool
end
