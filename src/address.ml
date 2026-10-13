open! Core

module Stable = struct
  module V1 = struct
    type t =
      { line1 : string
      ; line2 : string option
      ; city : string
      ; region : string option
      ; postal_code : string option
      ; country :
          (Country.Stable.V1.t
          [@quickcheck.generator Country.quickcheck_generator]
          [@quickcheck.observer Country.quickcheck_observer]
          [@quickcheck.shrinker Country.quickcheck_shrinker])
      }
    [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
  end
end

include Stable.V1
