(** The business case status machine.

    {v
      Draft ------------ Submit ---------------> Submitted (round 1)
      More_information - Submit ---------------> Submitted (next round)
      Submitted -------- Start_review ---------> In_review
      Submitted, In_review -- Approve ---------> Approved
      Submitted, In_review -- Request_information items -> More_information items
      Approved --------- Request_information --> More_information (periodic review)
      Submitted, In_review, More_information -- Reject -> Rejected (final)
      any but Blocked, Closed -- Block --------> Blocked (remembers the previous status)
      Blocked ---------- Unblock --------------> the previous status
      any but Closed --- Close ----------------> Closed (final)
    v}

    A decision may come straight from [Submitted]: a reviewer need not announce the
    review first. [Block] and [Unblock] need an [Operator]. Rounds and the guards that
    need the whole case (completeness, linked people, risk) are checked by {!Kyb_case}. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Draft
      | Submitted
      | In_review
      | More_information of Kyb_item.Stable.V1.t list
      | Approved
      | Rejected of string
      | Blocked of
          { reason : string
          ; previous : t
          }
      | Closed of string
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Draft (** being filled in; nothing sent yet *)
  | Submitted
  | In_review
  | More_information of Kyb_item.t list (** what the reviewer asked for *)
  | Approved
  | Rejected of string (** reason code *)
  | Blocked of
      { reason : string
      ; previous : t
      }
  | Closed of string (** relationship ended; starts the retention clock *)
[@@deriving compare, equal, hash, sexp, quickcheck]

module Transition : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Submit
        | Start_review
        | Request_information of Kyb_item.Stable.V1.t list
        | Approve
        | Reject of string
        | Block of string
        | Unblock
        | Close of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Submit
    | Start_review
    | Request_information of Kyb_item.t list (** must not be empty *)
    | Approve
    | Reject of string
    | Block of string
    | Unblock
    | Close of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

val apply : t -> Transition.t -> actor:Actor.t -> t Or_error.t

(** [Draft] or [More_information]: the applicant may change the case. *)
val is_editable : t -> bool

(** [Submitted] or [In_review]: waiting for a decision. *)
val is_pending : t -> bool
