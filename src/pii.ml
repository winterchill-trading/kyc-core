open! Core

module Stable = struct
  module V1 = struct
    type 'a t = 'a [@@deriving bin_io, compare, equal, hash, quickcheck]

    let redacted = "<redacted>"
    let sexp_of_t _ _ = Sexp.Atom redacted
    let t_of_sexp a_of_sexp sexp = a_of_sexp sexp
  end
end

include Stable.V1

let create x = x
let reveal x = x
let map x ~f = f x
let to_string _ = redacted
