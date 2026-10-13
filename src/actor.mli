(** Who caused a change: the platform itself, a verification provider (named by the
    integration, e.g. ["provider-a"]), or a compliance operator. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | System
      | Provider of string
      | Operator of Operator_id.Stable.V1.t
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | System
  | Provider of string
  | Operator of Operator_id.t
[@@deriving compare, equal, hash, sexp, quickcheck]

val is_operator : t -> bool
