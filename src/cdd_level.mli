(** Depth of customer due diligence (FATF R.1/R.10; AMLR Arts. 33-34). *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Simplified
      | Standard
      | Enhanced
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Simplified
  | Standard
  | Enhanced
[@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

(** [Enhanced] when any EDD trigger applies or the band is [High];
    [Simplified] only for a [Low] band, when the policy allows it, and never
    for crypto or Lightning products; otherwise [Standard]. *)
val decide
  :  band:Risk.Band.t
  -> triggers:Risk.Edd_trigger.t list
  -> products:Risk.Product.t list
  -> allow_simplified:bool
  -> t
