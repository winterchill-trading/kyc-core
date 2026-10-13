open! Core
open Kyc_core

let operator = Actor.Operator (Operator_id.of_string "op-1")

let statuses : Status.t list =
  [ Created
  ; Verifying
  ; Approved
  ; Rejected "document forged"
  ; Needs_resubmission [ Residential_address ]
  ; Blocked { reason = "sanctions match"; previous = Approved }
  ]
;;

let transitions : Status.Transition.t list =
  [ Submit
  ; Approve
  ; Reject "document forged"
  ; Request_resubmission [ Mobile; Email; Mobile ]
  ; Block "sanctions match"
  ; Unblock
  ]
;;

let%expect_test "every transition, as an operator" =
  List.iter statuses ~f:(fun from ->
    List.iter transitions ~f:(fun transition ->
      let result =
        match Status.apply from transition ~actor:operator with
        | Ok next -> [%sexp (next : Status.t)]
        | Error _ -> [%sexp "illegal"]
      in
      print_s
        [%message (from : Status.t) (transition : Status.Transition.t) (result : Sexp.t)]));
  [%expect
    {|
    ((from Created) (transition Submit) (result Verifying))
    ((from Created) (transition Approve) (result illegal))
    ((from Created) (transition (Reject "document forged")) (result illegal))
    ((from Created) (transition (Request_resubmission (Mobile Email Mobile)))
     (result illegal))
    ((from Created) (transition (Block "sanctions match"))
     (result (Blocked (reason "sanctions match") (previous Created))))
    ((from Created) (transition Unblock) (result illegal))
    ((from Verifying) (transition Submit) (result illegal))
    ((from Verifying) (transition Approve) (result Approved))
    ((from Verifying) (transition (Reject "document forged"))
     (result (Rejected "document forged")))
    ((from Verifying) (transition (Request_resubmission (Mobile Email Mobile)))
     (result (Needs_resubmission (Email Mobile))))
    ((from Verifying) (transition (Block "sanctions match"))
     (result (Blocked (reason "sanctions match") (previous Verifying))))
    ((from Verifying) (transition Unblock) (result illegal))
    ((from Approved) (transition Submit) (result illegal))
    ((from Approved) (transition Approve) (result illegal))
    ((from Approved) (transition (Reject "document forged")) (result illegal))
    ((from Approved) (transition (Request_resubmission (Mobile Email Mobile)))
     (result (Needs_resubmission (Email Mobile))))
    ((from Approved) (transition (Block "sanctions match"))
     (result (Blocked (reason "sanctions match") (previous Approved))))
    ((from Approved) (transition Unblock) (result illegal))
    ((from (Rejected "document forged")) (transition Submit) (result illegal))
    ((from (Rejected "document forged")) (transition Approve) (result illegal))
    ((from (Rejected "document forged")) (transition (Reject "document forged"))
     (result illegal))
    ((from (Rejected "document forged"))
     (transition (Request_resubmission (Mobile Email Mobile))) (result illegal))
    ((from (Rejected "document forged")) (transition (Block "sanctions match"))
     (result
      (Blocked (reason "sanctions match")
       (previous (Rejected "document forged")))))
    ((from (Rejected "document forged")) (transition Unblock) (result illegal))
    ((from (Needs_resubmission (Residential_address))) (transition Submit)
     (result Verifying))
    ((from (Needs_resubmission (Residential_address))) (transition Approve)
     (result illegal))
    ((from (Needs_resubmission (Residential_address)))
     (transition (Reject "document forged"))
     (result (Rejected "document forged")))
    ((from (Needs_resubmission (Residential_address)))
     (transition (Request_resubmission (Mobile Email Mobile))) (result illegal))
    ((from (Needs_resubmission (Residential_address)))
     (transition (Block "sanctions match"))
     (result
      (Blocked (reason "sanctions match")
       (previous (Needs_resubmission (Residential_address))))))
    ((from (Needs_resubmission (Residential_address))) (transition Unblock)
     (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition Submit) (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition Approve) (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition (Reject "document forged")) (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition (Request_resubmission (Mobile Email Mobile))) (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition (Block "sanctions match")) (result illegal))
    ((from (Blocked (reason "sanctions match") (previous Approved)))
     (transition Unblock) (result Approved))
    |}]
;;

let%expect_test "block and unblock need an operator" =
  List.iter [ Actor.System; Provider "provider-a"; operator ] ~f:(fun actor ->
    print_s
      [%sexp
        (Status.apply Approved (Block "sanctions match") ~actor : Status.t Or_error.t)];
    print_s
      [%sexp
        (Status.apply (Blocked { reason = "x"; previous = Verifying }) Unblock ~actor
         : Status.t Or_error.t)]);
  [%expect
    {|
    (Error
     ("only a compliance operator may block or unblock" (actor System)
      (transition (Block "sanctions match"))))
    (Error
     ("only a compliance operator may block or unblock" (actor System)
      (transition Unblock)))
    (Error
     ("only a compliance operator may block or unblock"
      (actor (Provider provider-a)) (transition (Block "sanctions match"))))
    (Error
     ("only a compliance operator may block or unblock"
      (actor (Provider provider-a)) (transition Unblock)))
    (Ok (Blocked (reason "sanctions match") (previous Approved)))
    (Ok Verifying)
    |}]
;;

let%expect_test "errors" =
  let show from transition =
    print_s [%sexp (Status.apply from transition ~actor:operator : Status.t Or_error.t)]
  in
  show Verifying (Request_resubmission []);
  show (Rejected "x") Approve;
  show (Blocked { reason = "x"; previous = Created }) (Block "y");
  [%expect
    {|
    (Error "resubmission request names no fields")
    (Error
     ("illegal status transition" (from (Rejected x)) (transition Approve)))
    (Error "case is already blocked")
    |}]
;;

let%test_unit "unblock restores the status before the block" =
  Quickcheck.test
    [%quickcheck.generator: Status.t]
    ~sexp_of:[%sexp_of: Status.t]
    ~f:(fun status ->
      match status with
      | Blocked _ -> ()
      | _ ->
        let restored =
          let open Or_error.Let_syntax in
          let%bind blocked = Status.apply status (Block "review") ~actor:operator in
          Status.apply blocked Unblock ~actor:operator
        in
        [%test_result: Status.t Or_error.t] restored ~expect:(Ok status))
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Status.Stable.V1.t];
  print_endline [%bin_digest: Status.Transition.Stable.V1.t];
  [%expect
    {|
    007f2150bd05fbebc312f09e09becf22
    61a9b5e3f2f1bc099e50f232de48a620
    |}]
;;
