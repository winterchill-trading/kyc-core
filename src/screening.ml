open! Core

module List_kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Sanctions
        | Pep
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

module Hit = struct
  module Stable = struct
    module V1 = struct
      type t =
        { list_kind : List_kind.Stable.V1.t
        ; list_name : string
        ; entry_ref : string
        ; matched_name : string
        ; score : int
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { list_kind : List_kind.t
    ; list_name : string
    ; entry_ref : string
    ; matched_name : string
    ; score : int [@quickcheck.generator Int.gen_incl 0 100]
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Outcome = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Clear
        | Hits of Hit.Stable.V1.t list
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Clear
    | Hits of Hit.t list
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Decision = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Not_required
        | Pending_review
        | False_positive of string
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

type t =
  { id : Screening_id.t
  ; screened_at : Timestamp.t
  ; outcome : Outcome.t
  ; decision : Decision.t
  ; reviewed_by : Operator_id.t option
  }
[@@deriving compare, equal, hash, sexp]

let create ~id ~screened_at ~(outcome : Outcome.t) =
  let decision : Decision.t =
    match outcome with
    | Clear | Hits [] -> Not_required
    | Hits (_ :: _) -> Pending_review
  in
  { id; screened_at; outcome; decision; reviewed_by = None }
;;

let review t ~(decision : Decision.t) ~(actor : Actor.t) =
  match actor, t.decision, decision with
  | Operator operator, Pending_review, (False_positive _ | True_match _) ->
    Ok { t with decision; reviewed_by = Some operator }
  | (System | Provider _), _, _ ->
    Or_error.error_s
      [%message "only an operator may review a screening" (actor : Actor.t)]
  | Operator _, Pending_review, (Not_required | Pending_review) ->
    Or_error.error_s
      [%message "a review must decide the hits" ~decision:(decision : Decision.t)]
  | Operator _, (Not_required | False_positive _ | True_match _), _ ->
    Or_error.error_s
      [%message
        "screening is not awaiting review"
          ~screening:(t.id : Screening_id.t)
          ~current:(t.decision : Decision.t)]
;;

let blocks_approval t =
  match t.decision with
  | Pending_review | True_match _ -> true
  | Not_required | False_positive _ -> false
;;
