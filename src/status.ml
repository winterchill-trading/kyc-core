open! Core

module Stable = struct
  module V1 = struct
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

module Transition = struct
  module Stable = struct
    module V1 = struct
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
    | Request_resubmission of Person.Field.t list
    | Block of string
    | Unblock
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

let apply t (transition : Transition.t) ~actor =
  let illegal () =
    Or_error.error_s
      [%message "illegal status transition" ~from:(t : t) (transition : Transition.t)]
  in
  let needs_operator next =
    if Actor.is_operator actor
    then Ok next
    else
      Or_error.error_s
        [%message
          "only a compliance operator may block or unblock"
            (actor : Actor.t)
            (transition : Transition.t)]
  in
  match t, transition with
  | (Created | Needs_resubmission _), Submit -> Ok Verifying
  | Verifying, Approve -> Ok Approved
  | (Verifying | Needs_resubmission _), Reject reason -> Ok (Rejected reason)
  | (Verifying | Approved), Request_resubmission [] ->
    Or_error.error_s [%message "resubmission request names no fields"]
  | (Verifying | Approved), Request_resubmission fields ->
    Ok (Needs_resubmission (List.dedup_and_sort fields ~compare:Person.Field.compare))
  | Blocked _, Block _ -> Or_error.error_s [%message "case is already blocked"]
  | previous, Block reason -> needs_operator (Blocked { reason; previous })
  | Blocked { previous; reason = _ }, Unblock -> needs_operator previous
  | ( (Created | Verifying | Approved | Rejected _ | Needs_resubmission _ | Blocked _)
    , (Submit | Approve | Reject _ | Request_resubmission _ | Unblock) ) -> illegal ()
;;
