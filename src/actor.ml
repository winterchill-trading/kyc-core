open! Core

module Stable = struct
  module V1 = struct
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

let is_operator = function
  | Operator _ -> true
  | System | Provider _ -> false
;;
