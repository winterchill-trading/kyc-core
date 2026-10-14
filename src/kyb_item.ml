open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Entity_field of Legal_entity.Field.Stable.V1.t
      | Document of Kyb_document.Requirement.Stable.V1.t
      | Parties
      | Party_case of Customer_id.Stable.V1.t
      | Beneficial_owners
      | Source_of_funds
      | Explanation of string
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

module T = struct
  type t = Stable.V1.t =
    | Entity_field of Legal_entity.Field.t
    | Document of Kyb_document.Requirement.t
    | Parties
    | Party_case of Customer_id.t
    | Beneficial_owners
    | Source_of_funds
    | Explanation of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

include T
include Comparable.Make (T)
