open! Core
open Kyc_core

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:30:00Z"
let a = Actor.Operator (Operator_id.of_string "op-a")
let b = Actor.Operator (Operator_id.of_string "op-b")

let hit source entry_id : Screening.Hit.t =
  { source
  ; list_version = "2026-10-13"
  ; entry_id
  ; matched_name = "A. Example"
  ; score = 91
  ; algorithm = "jaro-winkler-1"
  }
;;

let run hits =
  Or_error.ok_exn
    (Screening.create
       ~id:(Screening_id.of_string "scr-1")
       ~reason:Onboarding
       ~screened_at:at
       ~hits)
;;

let review s entry_id action actor =
  match Screening.review s ~entry_id ~action ~actor with
  | Ok (s, state) ->
    print_s [%sexp (state : Screening.Hit_state.t)];
    s
  | Error e ->
    print_s [%sexp (e : Error.t)];
    s
;;

let%expect_test "discounting needs two operators" =
  let s = run [ hit Eu_fsf "eu-1" ] in
  print_s [%sexp (Screening.blocks_approval s : bool)];
  let s = review s "eu-1" Take System in
  let s = review s "eu-1" Take a in
  let s = review s "eu-1" (Propose_discount "date of birth differs") a in
  let s = review s "eu-1" Confirm_discount a in
  let s = review s "eu-1" Confirm_discount b in
  print_s [%sexp (Screening.blocks_approval s : bool)];
  let _ = review s "eu-1" (Confirm "x") b in
  [%expect
    {|
    true
    ("only an operator may review a hit" (actor System))
    (Under_review (by op-a) (discount_proposal ()))
    (Under_review (by op-a) (discount_proposal ("date of birth differs")))
    ("discounting needs a second operator" (proposed_by op-a))
    (Discounted (reason "date of birth differs") (proposed_by op-a)
     (confirmed_by op-b))
    false
    ("illegal hit review"
     (state
      (Discounted (reason "date of birth differs") (proposed_by op-a)
       (confirmed_by op-b)))
     (action (Confirm x)))
    |}]
;;

let%expect_test "escalate and confirm" =
  let s = run [ hit Pep_list "pep-1"; hit Adverse_media "am-1" ] in
  let s = review s "pep-1" (Escalate "senior review: same employer") a in
  let s = review s "pep-1" (Escalate "again") b in
  let s = review s "pep-1" (Confirm "same person") b in
  let s = review s "nope" Take a in
  print_s [%sexp (Screening.confirmed s : Screening.Hit.t list)];
  print_s [%sexp (List.map (Screening.pending s) ~f:(fun h -> h.entry_id) : string list)];
  [%expect
    {|
    (Escalated (by op-a) (reason "senior review: same employer"))
    ("illegal hit review"
     (state (Escalated (by op-a) (reason "senior review: same employer")))
     (action (Escalate again)))
    (Confirmed (by op-b) (reason "same person"))
    ("unknown hit" (screening scr-1) (entry_id nope))
    (((source Pep_list) (list_version 2026-10-13) (entry_id pep-1)
      (matched_name "A. Example") (score 91) (algorithm jaro-winkler-1)))
    (am-1)
    |}]
;;

let%expect_test "duplicate entries are rejected" =
  print_s
    [%sexp
      (Screening.create
         ~id:(Screening_id.of_string "scr-2")
         ~reason:List_update
         ~screened_at:at
         ~hits:[ hit Un_sc "un-1"; hit Un_sc "un-1" ]
       |> Result.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  [%expect {| (Error ("duplicate hit in screening" (entry_id un-1))) |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Screening.Hit.Stable.V1.t];
  print_endline [%bin_digest: Screening.Review_action.Stable.V1.t];
  [%expect
    {|
    6229de2add140e0cc4628968c257d834
    896cc61de958cd543e8090c915458bcd
    |}]
;;
