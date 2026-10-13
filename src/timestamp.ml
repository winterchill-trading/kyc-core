open! Core

module Stable = struct
  module V1 = Time_ns.Stable.Alternate_sexp.V1
end

include Stable.V1

let quickcheck_generator = Time_ns.quickcheck_generator
let quickcheck_observer = Time_ns.quickcheck_observer
let quickcheck_shrinker = Time_ns.quickcheck_shrinker
