(** Personally identifiable information.

    A ['a t] holds a value that must not leak into logs, error messages or expect output.
    [sexp_of_t] and [to_string] print ["<redacted>"]; the value is only reachable through
    {!reveal}. [t_of_sexp] reads the clear value, so fixtures and operator input can still
    be written as sexps.

    Because of that asymmetry a sexp round trip does not preserve the value. Persist PII
    with bin_io (the [Stable] types), which carries the clear value. *)

open! Core

type 'a t [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]

val create : 'a -> 'a t

(** The clear value. Call sites are the places PII is used on purpose (rendering a
    document for the customer, sending to a verification provider); grep for [Pii.reveal]
    to audit them. *)
val reveal : 'a t -> 'a

val map : 'a t -> f:('a -> 'b) -> 'b t

(** Always ["<redacted>"]. *)
val to_string : _ t -> string

val redacted : string

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
  end
end
