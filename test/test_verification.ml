open! Core
open Kyc_core
open Verification

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:30:00Z"

let states : State.t list =
  [ Pending; Sent; Verified at; Failed "document unreadable"; Expired ]
;;

let%expect_test "every transition" =
  List.iter states ~f:(fun from ->
    List.iter states ~f:(fun to_ ->
      let verdict =
        match transition ~from ~to_ with
        | Ok _ -> "ok"
        | Error _ -> "illegal"
      in
      print_s [%message (from : State.t) (to_ : State.t) verdict]));
  [%expect
    {|
    ((from Pending) (to_ Pending) illegal)
    ((from Pending) (to_ Sent) ok)
    ((from Pending) (to_ (Verified "2026-10-13 09:30:00Z")) ok)
    ((from Pending) (to_ (Failed "document unreadable")) ok)
    ((from Pending) (to_ Expired) illegal)
    ((from Sent) (to_ Pending) illegal)
    ((from Sent) (to_ Sent) ok)
    ((from Sent) (to_ (Verified "2026-10-13 09:30:00Z")) ok)
    ((from Sent) (to_ (Failed "document unreadable")) ok)
    ((from Sent) (to_ Expired) ok)
    ((from (Verified "2026-10-13 09:30:00Z")) (to_ Pending) illegal)
    ((from (Verified "2026-10-13 09:30:00Z")) (to_ Sent) illegal)
    ((from (Verified "2026-10-13 09:30:00Z"))
     (to_ (Verified "2026-10-13 09:30:00Z")) illegal)
    ((from (Verified "2026-10-13 09:30:00Z"))
     (to_ (Failed "document unreadable")) ok)
    ((from (Verified "2026-10-13 09:30:00Z")) (to_ Expired) ok)
    ((from (Failed "document unreadable")) (to_ Pending) ok)
    ((from (Failed "document unreadable")) (to_ Sent) ok)
    ((from (Failed "document unreadable"))
     (to_ (Verified "2026-10-13 09:30:00Z")) illegal)
    ((from (Failed "document unreadable")) (to_ (Failed "document unreadable"))
     illegal)
    ((from (Failed "document unreadable")) (to_ Expired) illegal)
    ((from Expired) (to_ Pending) ok)
    ((from Expired) (to_ Sent) ok)
    ((from Expired) (to_ (Verified "2026-10-13 09:30:00Z")) illegal)
    ((from Expired) (to_ (Failed "document unreadable")) illegal)
    ((from Expired) (to_ Expired) illegal)
    |}]
;;

let%expect_test "the error names both states" =
  print_s [%sexp (transition ~from:Expired ~to_:(Verified at) : State.t Or_error.t)];
  [%expect
    {|
    (Error
     ("illegal verification transition" (from Expired)
      (to_ (Verified "2026-10-13 09:30:00Z"))))
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Step.Stable.V1.t];
  print_endline [%bin_digest: State.Stable.V1.t];
  [%expect
    {|
    9cd1526b613b5a62ee9189d40d19934a
    bf8827ea3b088e1f78b8a2a567dd5a8c
    |}]
;;
