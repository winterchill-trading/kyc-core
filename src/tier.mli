(** Verification tiers and the rule table that maps verified steps to a tier.

    - [T0]: unverified
    - [T1]: email and mobile
    - [T2]: identity (document and liveness)
    - [T3]: enhanced (address proof and source of funds)

    What a tier allows (limits, products) is not decided here: implement {!Limits} in the
    product layer and pass it to [Kyc_case.limits]. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | T0
      | T1
      | T2
      | T3
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | T0
  | T1
  | T2
  | T3
[@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

include Comparable.S with type t := t

module Rules : sig
  (** Required steps per tier. Each tier's set includes the one below it, so the tier of a
      customer is the highest tier whose set is fully verified. *)
  type t [@@deriving compare, equal, sexp]

  (** T1 = email + mobile; T2 = T1 + identity document + liveness; T3 = T2 + address
      proof + source of funds. *)
  val default : t

  (** Fails unless every tier is listed once, [T0] needs nothing, and each tier's steps
      include those of the tier below. *)
  val create : (Stable.V1.t * Verification.Step.t list) list -> t Or_error.t

  val required_for : t -> Stable.V1.t -> Verification.Step.Set.t

  (** The highest tier whose required steps are all in [verified]. *)
  val tier_of : t -> verified:Verification.Step.Set.t -> Stable.V1.t

  val to_list : t -> (Stable.V1.t * Verification.Step.Set.t) list
end

(** What a tier unlocks; supplied by the product, not by this library. *)
module type Limits = sig
  type limits [@@deriving sexp_of]

  val limits : t -> limits
end
