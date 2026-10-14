open! Core

module Stable = struct
  module V1 = struct
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
  | Draft
  | Submitted
  | In_review
  | More_information of Kyb_item.t list
  | Approved
  | Rejected of string
  | Blocked of
      { reason : string
      ; previous : t
      }
  | Closed of string
[@@deriving compare, equal, hash, sexp, quickcheck]

module Transition = struct
  module Stable = struct
    module V1 = struct
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
    | Request_information of Kyb_item.t list
    | Approve
    | Reject of string
    | Block of string
    | Unblock
    | Close of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

let is_editable = function
  | Draft | More_information _ -> true
  | Submitted | In_review | Approved | Rejected _ | Blocked _ | Closed _ -> false
;;

let is_pending = function
  | Submitted | In_review -> true
  | Draft | More_information _ | Approved | Rejected _ | Blocked _ | Closed _ -> false
;;

let apply t (transition : Transition.t) ~(actor : Actor.t) =
  let illegal () =
    Or_error.error_s
      [%message "illegal KYB transition" ~from:(t : t) (transition : Transition.t)]
  in
  let need_text what s =
    if String.is_empty (String.strip s)
    then Or_error.error_s [%message "a reason is required" (what : string)]
    else Ok ()
  in
  let open Or_error.Let_syntax in
  match transition, t with
  | Submit, (Draft | More_information _) -> Ok Submitted
  | Start_review, Submitted -> Ok In_review
  | Request_information [], _ -> Or_error.error_s [%message "nothing requested"]
  | Request_information items, (Submitted | In_review | Approved) ->
    Ok (More_information items)
  | Approve, (Submitted | In_review) -> Ok Approved
  | Reject reason, (Submitted | In_review | More_information _) ->
    let%map () = need_text "reject" reason in
    Rejected reason
  | ( Block reason
    , (Draft | Submitted | In_review | More_information _ | Approved | Rejected _) ) ->
    if not (Actor.is_operator actor)
    then Or_error.error_s [%message "only an operator may block"]
    else (
      let%map () = need_text "block" reason in
      Blocked { reason; previous = t })
  | Unblock, Blocked { previous; reason = _ } ->
    if Actor.is_operator actor
    then Ok previous
    else Or_error.error_s [%message "only an operator may unblock"]
  | ( Close reason
    , ( Draft
      | Submitted
      | In_review
      | More_information _
      | Approved
      | Rejected _
      | Blocked _ ) ) ->
    let%map () = need_text "close" reason in
    Closed reason
  | ( ( Submit
      | Start_review
      | Request_information _
      | Approve
      | Reject _
      | Block _
      | Unblock
      | Close _ )
    , _ ) -> illegal ()
;;
