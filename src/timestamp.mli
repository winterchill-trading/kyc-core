(** [Time_ns.t] whose sexp is UTC ([2026-10-13 09:30:00Z]) whatever the machine's zone, so
    audit output and expect tests read the same on every host. bin_io is that of
    [Time_ns]. *)

open! Core

type t = Time_ns.t [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
  end
end
