(** Verification steps and the state of each.

    Every step starts [Pending]. Legal moves:

    {v
      Pending  -> Sent | Verified | Failed
      Sent     -> Sent (resend) | Verified | Failed | Expired
      Verified -> Expired (document or check lapsed) | Failed (revoked on review)
      Failed   -> Pending | Sent (retry)
      Expired  -> Pending | Sent (retry)
    v}

    Anything else is an error. A failed or expired step is never verified directly: the
    retry goes through [Sent] (or [Pending]) so the audit log shows a new attempt. *)

open! Core

module Step : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Email
        | Mobile
        | Identity_document
        | Liveness
        | Address_proof
        | Source_of_funds
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Email
    | Mobile
    | Identity_document
    | Liveness
    | Address_proof
    | Source_of_funds
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.S with type t := t
end

module State : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Pending
        | Sent (** code, link or capture session issued; waiting for the customer *)
        | Verified of Timestamp.Stable.V1.t
        | Failed of string (** reason, as given by the provider or operator *)
        | Expired
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Pending
    | Sent
    | Verified of Timestamp.t
    | Failed of string
    | Expired
  [@@deriving compare, equal, hash, sexp, quickcheck]

  val is_verified : t -> bool
end

(** [transition ~from ~to_] is [Ok to_] when the move is legal. *)
val transition : from:State.t -> to_:State.t -> State.t Or_error.t
