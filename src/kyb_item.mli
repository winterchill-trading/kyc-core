(** Something a business case still needs or a reviewer asks for: used both for the
    completeness check before submission and for the items of a request for more
    information. *)

open! Core

module Stable : sig
  module V1 : sig
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

type t = Stable.V1.t =
  | Entity_field of Legal_entity.Field.t
  | Document of Kyb_document.Requirement.t
  | Parties (** the list of directors, owners and representatives *)
  | Party_case of Customer_id.t (** that person's own verification is not approved *)
  | Beneficial_owners (** no beneficial owner (or senior managing official) identified *)
  | Source_of_funds
  | Explanation of string (** a reviewer's free-text question *)
[@@deriving compare, equal, hash, sexp, quickcheck]

include Comparable.S with type t := t
