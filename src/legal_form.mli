(** The legal form of a business applicant, in generic terms that map onto national forms
    (a GmbH or an English Ltd is a [Private_limited_company], an AG or a plc a
    [Public_limited_company]). [Other] keeps the national name for forms that fit none.

    Trusts are legal arrangements rather than legal persons: their beneficial owners are
    the settlor, trustees, protector and beneficiaries whatever their share (FATF R.25,
    AMLR Art. 58), which {!Ownership} applies. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Sole_trader
      | General_partnership
      | Limited_partnership
      | Private_limited_company
      | Public_limited_company
      | Cooperative
      | Association
      | Foundation
      | Trust
      | Other of string (** the national name, e.g. ["Stiftung"] *)
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Sole_trader
  | General_partnership
  | Limited_partnership
  | Private_limited_company
  | Public_limited_company
  | Cooperative
  | Association
  | Foundation
  | Trust
  | Other of string
[@@deriving compare, equal, hash, sexp, quickcheck]

(** Every form except [Other]. *)
val standard : t list

(** [Trust]: ownership thresholds do not apply to its parties. *)
val is_legal_arrangement : t -> bool

(** Lower-case snake case for command lines and options lists, e.g.
    ["private_limited_company"]; [Other s] is ["other:" ^ s]. *)
val to_string : t -> string

(** Inverse of {!to_string}. *)
val of_string : string -> t Or_error.t
