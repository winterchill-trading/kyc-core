(** The case status machine.

    {v
      Created ------------ Submit ----------> Verifying
      Needs_resubmission - Submit ----------> Verifying
      Verifying ---------- Approve ---------> Approved
      Verifying ---------- Reject ----------> Rejected
      Needs_resubmission - Reject ----------> Rejected   (customer never came back)
      Verifying ---------- Request_resubmission -> Needs_resubmission
      Approved ----------- Request_resubmission -> Needs_resubmission (periodic review)
      any but Blocked ---- Block -----------> Blocked (remembers the previous status)
      Blocked ------------ Unblock ---------> the previous status
    v}

    [Rejected] is final apart from [Block]. [Block] and [Unblock] are compliance decisions
    and need an [Operator] actor. Other guards (verified steps, open screening hits) are
    checked by {!Kyc_case}, which knows the whole case. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Created
      | Verifying
      | Approved
      | Rejected of string
      | Needs_resubmission of Person.Field.Stable.V1.t list
      | Blocked of
          { reason : string
          ; previous : t
          }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Created
  | Verifying
  | Approved
  | Rejected of string
  | Needs_resubmission of Person.Field.t list
  | Blocked of
      { reason : string
      ; previous : t
      }
[@@deriving compare, equal, hash, sexp, quickcheck]

module Transition : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Submit
        | Approve
        | Reject of string
        | Request_resubmission of Person.Field.Stable.V1.t list
        | Block of string
        | Unblock
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Submit
    | Approve
    | Reject of string
    | Request_resubmission of Person.Field.t list (** must not be empty *)
    | Block of string
    | Unblock
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

val apply : t -> Transition.t -> actor:Actor.t -> t Or_error.t
