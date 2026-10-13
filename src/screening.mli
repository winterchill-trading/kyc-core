(** Sanctions, PEP and adverse-media screening.

    A screening run produces an {!Outcome}. A run with hits waits for a reviewer:
    [False_positive] clears it, [True_match] confirms it. A case cannot be approved while
    any run is [Pending_review] or [True_match]; blocking or rejecting the case after a
    true match is a separate, explicit operator decision. *)

open! Core

module List_kind : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Sanctions
        | Pep (** politically exposed person *)
        | Adverse_media
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Sanctions
    | Pep
    | Adverse_media
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Hit : sig
  (** One candidate match. [matched_name] is the list entry's name (public list data), not
      the customer's. *)

  module Stable : sig
    module V1 : sig
      type t =
        { list_kind : List_kind.Stable.V1.t
        ; list_name : string (** e.g. a consolidated sanctions list *)
        ; entry_ref : string (** the list's own id for the entry *)
        ; matched_name : string
        ; score : int (** match strength, 0 to 100 *)
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { list_kind : List_kind.t
    ; list_name : string
    ; entry_ref : string
    ; matched_name : string
    ; score : int
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Outcome : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Clear
        | Hits of Hit.Stable.V1.t list
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Clear
    | Hits of Hit.t list (** non-empty; [Hits []] is treated as [Clear] *)
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Decision : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Not_required (** the run was clear *)
        | Pending_review
        | False_positive of string (** reviewer's note *)
        | True_match of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Not_required
    | Pending_review
    | False_positive of string
    | True_match of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

(** A recorded run and where its review stands. *)
type t =
  { id : Screening_id.t
  ; screened_at : Timestamp.t
  ; outcome : Outcome.t
  ; decision : Decision.t
  ; reviewed_by : Operator_id.t option
  }
[@@deriving compare, equal, hash, sexp]

(** [decision] is [Not_required] for a clear run, [Pending_review] otherwise. *)
val create : id:Screening_id.t -> screened_at:Timestamp.t -> outcome:Outcome.t -> t

(** Records a reviewer's decision. Only an operator may review, only a run that is
    [Pending_review], and the decision must be [False_positive] or [True_match]. *)
val review : t -> decision:Decision.t -> actor:Actor.t -> t Or_error.t

val blocks_approval : t -> bool
