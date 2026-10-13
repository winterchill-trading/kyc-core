(** Ongoing monitoring (FATF R.10 ongoing due diligence; AMLR Art. 26): alerts raised by
    transaction monitoring, re-screening, adverse media, document expiry or an overdue
    review.

    {v Open -> Closed_no_action | Escalated;  Escalated -> Closed_no_action v}

    An escalated alert goes to an {!Investigation}. Alerts are restricted information
    (tipping-off, FATF R.21): see {!Visibility}. *)

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

module Decision : sig
  module Stable : sig
    module V1 : sig
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

module Alert_state : sig
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

module Alert : sig
  type t =
    { id : Alert_id.t
    ; raised_at : Timestamp.t
    ; source : Source.t
    ; severity : Risk.Band.t
    ; summary : string
    ; state : Alert_state.t
    }
  [@@deriving compare, equal, hash, sexp]

  val is_open : t -> bool

  (** Open or escalated and not yet closed. *)
  val is_active : t -> bool

  val decide : t -> Decision.t -> by:Operator_id.t -> at:Timestamp.t -> t Or_error.t
end
