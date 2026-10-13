open! Core

module Stable = struct
  module V1 = struct
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

let decide ~(band : Risk.Band.t) ~triggers ~products ~allow_simplified =
  let crypto =
    List.exists products ~f:(function
      | Risk.Product.Crypto | Lightning -> true
      | Fiat_account | Card -> false)
  in
  match band with
  | _ when not (List.is_empty triggers) -> Enhanced
  | High -> Enhanced
  | Low when allow_simplified && not crypto -> Simplified
  | Low | Medium -> Standard
;;
