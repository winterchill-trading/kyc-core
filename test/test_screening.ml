open! Core
open Kyc_core

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:30:00Z"
let operator = Actor.Operator (Operator_id.of_string "op-1")

let hit : Screening.Hit.t =
  { list_kind = Pep
  ; list_name = "pep-list"
  ; entry_ref = "entry-42"
  ; matched_name = "A. Example"
  ; score = 81
  }
;;

let run outcome =
  Screening.create ~id:(Screening_id.of_string "scr-1") ~screened_at:at ~outcome
;;

let show r = print_s [%sexp (r : Screening.t Or_error.t)]

let%expect_test "a clear run needs no review" =
  let clear = run Clear in
  print_s [%sexp (clear : Screening.t)];
  print_s [%sexp (Screening.blocks_approval clear : bool)];
  show (Screening.review clear ~decision:(False_positive "n/a") ~actor:operator);
  [%expect
    {|
    ((id scr-1) (screened_at "2026-10-13 09:30:00Z") (outcome Clear)
     (decision Not_required) (reviewed_by ()))
    false
    (Error
     ("screening is not awaiting review" (screening scr-1)
      (current Not_required)))
    |}]
;;

let%expect_test "hits wait for an operator" =
  let hits = run (Hits [ hit ]) in
  print_s [%sexp (Screening.blocks_approval hits : bool)];
  show
    (Screening.review
       hits
       ~decision:(False_positive "different date of birth")
       ~actor:System);
  show (Screening.review hits ~decision:Pending_review ~actor:operator);
  let cleared =
    Screening.review
      hits
      ~decision:(False_positive "different date of birth")
      ~actor:operator
  in
  show cleared;
  print_s [%sexp (Screening.blocks_approval (Or_error.ok_exn cleared) : bool)];
  show
    (Screening.review (Or_error.ok_exn cleared) ~decision:(True_match "") ~actor:operator);
  let confirmed =
    Screening.review hits ~decision:(True_match "same person") ~actor:operator
  in
  print_s [%sexp (Screening.blocks_approval (Or_error.ok_exn confirmed) : bool)];
  [%expect
    {|
    true
    (Error ("only an operator may review a screening" (actor System)))
    (Error ("a review must decide the hits" (decision Pending_review)))
    (Ok
     ((id scr-1) (screened_at "2026-10-13 09:30:00Z")
      (outcome
       (Hits
        (((list_kind Pep) (list_name pep-list) (entry_ref entry-42)
          (matched_name "A. Example") (score 81)))))
      (decision (False_positive "different date of birth")) (reviewed_by (op-1))))
    false
    (Error
     ("screening is not awaiting review" (screening scr-1)
      (current (False_positive "different date of birth"))))
    true
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Screening.Outcome.Stable.V1.t];
  print_endline [%bin_digest: Screening.Decision.Stable.V1.t];
  [%expect
    {|
    15774b4975223edbc3ade03b09ff97ef
    836f509521655e074c59440363e0141d
    |}]
;;
