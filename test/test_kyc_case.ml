open! Core
open Kyc_core
open Fixtures

let print_audit ?(audience = Visibility.Audience.Compliance) case =
  List.iter
    (Audit.for_audience (Kyc_case.audit case) audience)
    ~f:(fun e -> print_s [%sexp (e.change : Audit.Change.t)])
;;

let%expect_test "happy path to approval" =
  let case = opened () in
  show_case case;
  let case = apply_all case [ verified Email; verified Mobile ] in
  show_case case;
  let case =
    apply_all
      case
      [ status ~actor:System Submit
      ; event (Step_changed { step = Identity_document; state = Sent })
      ; verified Identity_document
      ; verified Liveness
      ; screened "scr-1" []
      ; assessed ()
      ]
  in
  show_case case;
  print_s [%sexp (Kyc_case.cdd_level case ~now:t0 : Cdd_level.t)];
  let case = apply_all case [ status ~note:"documents consistent" Approve ] in
  show_case case;
  print_audit case;
  [%expect
    {|
    ((status Created) (tier T0) (band ()) (missing (Email Mobile)))
    ((status Created) (tier T1) (band ()) (missing (Identity_document Liveness)))
    ((status Verifying) (tier T1) (band (Low))
     (missing (Address_proof Source_of_funds)))
    Simplified
    ((status Approved) (tier T2) (band (Low))
     (missing (Address_proof Source_of_funds)))
    Opened
    (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Created) (to_ Verifying))
    (Step (step Identity_document) (from Pending) (to_ Sent))
    (Step (step Identity_document) (from Sent)
     (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Liveness) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Screened (screening_id scr-1) (reason Onboarding) (hits 0))
    (Risk_assessed (score 0) (computed_band Low) (band Low)
     (model_version kyc-risk-2026-10))
    (Status (from Verifying) (to_ Approved))
    |}]
;;

let%expect_test "approval guards" =
  reset ();
  let case = apply_all (opened ()) [ verified Email; verified Mobile; status Submit ] in
  print_s [%sexp (Kyc_case.can_approve case ~now:t0 : unit Or_error.t)];
  let case = apply_all case [ verified Identity_document; verified Liveness ] in
  print_s [%sexp (Kyc_case.can_approve case ~now:t0 : unit Or_error.t)];
  let case = apply_all case [ screened "scr-1" [ hit Pep_list "pep-9" ] ] in
  print_s [%sexp (Kyc_case.can_approve case ~now:t0 : unit Or_error.t)];
  (* Approving now is an error and leaves the case as it was. *)
  let before = case in
  let case = apply_all case [ status Approve ] in
  print_s [%sexp (Kyc_case.equal before case : bool)];
  let case =
    apply_all
      case
      [ review "scr-1" "pep-9" (Propose_discount "different date of birth")
      ; review ~actor:op_b "scr-1" "pep-9" Confirm_discount
      ; status Approve
      ; assessed ()
      ; status Approve
      ]
  in
  show_case case;
  [%expect
    {|
    (Error
     ("steps not verified for approval" (approval_tier T2)
      (missing (Identity_document Liveness))))
    (Error "no screening recorded")
    (Error ("screening hits not discounted" (screenings (scr-1))))
    ((event evt-8) ("screening hits not discounted" (screenings (scr-1))))
    true
    ((event evt-11) "no risk assessment recorded")
    ((status Approved) (tier T2) (band (Low))
     (missing (Address_proof Source_of_funds)))
    |}]
;;

let%expect_test "illegal events are errors, never exceptions" =
  let fresh = Kyc_case.create customer in
  reset ();
  let show case e =
    print_s
      [%sexp
        (Kyc_case.apply case (number e) |> Result.map ~f:Kyc_case.status
         : Status.t Or_error.t)]
  in
  show fresh (verified Email);
  let case = opened () in
  show case (event (Opened Test_person.person));
  show case (status Approve);
  show case (event (Step_changed { step = Liveness; state = Expired }));
  show case (event (Person_updated Test_person.person));
  show case (fun n ->
    { (verified Email n) with customer_id = Customer_id.of_string "cus-2" });
  show case (review "nope" "x" Take);
  show case (status ~actor:System (Block "suspicious"));
  show
    case
    (event ~actor:System (Legal_hold_set { until = t0; reason = "x"; authority = "y" }));
  show
    case
    (event
       ~actor:op_a
       (Alert_decided
          { alert_id = Monitoring.Alert_id.of_string "a"; decision = Escalated }));
  [%expect
    {|
    (Error ((event evt-1) "case not opened yet"))
    (Error ((event evt-2) "case already opened"))
    (Error
     ((event evt-3)
      ("illegal status transition" (from Created) (transition Approve))))
    (Error
     ((event evt-4)
      ((step Liveness)
       ("illegal verification transition" (from Pending) (to_ Expired)))))
    (Error ((event evt-5) "person update changes nothing"))
    (Error ("event is for another customer" (case cus-1) (event cus-2)))
    (Error ((event evt-7) (unknown (what screening) (id nope))))
    (Error
     ((event evt-8)
      ("only a compliance operator may block or unblock" (actor System)
       (transition (Block suspicious)))))
    (Error
     ((event evt-9)
      ("only an operator may do this" (what "set a legal hold") (actor System))))
    (Error ((event evt-10) (unknown (what alert) (id a))))
    |}]
;;

let%expect_test "resubmission" =
  let case = apply_all (opened ()) [ verified Email; verified Mobile; status Submit ] in
  let case =
    apply_all
      case
      [ status
          ~note:"proof of address is older than three months"
          (Request_resubmission [ Residential_address ])
      ]
  in
  print_s [%sexp (Kyc_case.customer_view case : Kyc_case.Customer_view.t)];
  let moved =
    { Test_person.person with
      residential_address =
        Pii.map Test_person.person.residential_address ~f:(fun a ->
          { a with city = "Hamburg" })
    }
  in
  let case =
    apply_all
      case
      [ event ~actor:System (Person_updated moved); status ~actor:System Submit ]
  in
  show_case case;
  print_audit case;
  [%expect
    {|
    ((status Action_required) (tier T1)
     (steps_to_complete (Identity_document Liveness))
     (fields_to_resubmit (Residential_address)))
    ((status Verifying) (tier T1) (band ())
     (missing (Identity_document Liveness)))
    Opened
    (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Created) (to_ Verifying))
    (Status (from Verifying) (to_ (Needs_resubmission (Residential_address))))
    (Person_updated (Residential_address))
    (Status (from (Needs_resubmission (Residential_address))) (to_ Verifying))
    |}]
;;

let%expect_test "a redelivered event is a no-op; a reused id is an error" =
  let case = opened () in
  let e =
    number
      (event
         ~id:"provider-delivery-9"
         (Step_changed { step = Email; state = Verified t0 }))
  in
  let once = Or_error.ok_exn (Kyc_case.apply case e) in
  let twice = Or_error.ok_exn (Kyc_case.apply once e) in
  print_s [%sexp (Kyc_case.equal once twice : bool)];
  print_s [%sexp (Audit.length (Kyc_case.audit twice) : int)];
  let reused = { e with kind = Step_changed { step = Mobile; state = Sent } } in
  print_s
    [%sexp
      (Kyc_case.apply once reused |> Result.map ~f:Kyc_case.status : Status.t Or_error.t)];
  [%expect
    {|
    true
    2
    (Error
     ("event id already applied with different content" (id provider-delivery-9)))
    |}]
;;

let%expect_test "limits hook" =
  let module Limits = struct
    type limits = { daily_eur_minor : int } [@@deriving sexp_of]

    let limits : Tier.t -> limits = function
      | T0 -> { daily_eur_minor = 0 }
      | T1 -> { daily_eur_minor = 10_000 }
      | T2 -> { daily_eur_minor = 1_000_000 }
      | T3 -> { daily_eur_minor = 10_000_000 }
    ;;
  end
  in
  let case = apply_all (opened ()) [ verified Email; verified Mobile ] in
  print_s [%sexp (Kyc_case.limits (module Limits) case : Limits.limits)];
  [%expect {| ((daily_eur_minor 10000)) |}]
;;

let foreign_pep =
  Or_error.ok_exn
    (Pep.create
       ~kind:Pep
       ~scope:Foreign
       ~function_:"minister"
       ~started:(Date.of_string "2015-01-01")
       ())
;;

let%expect_test "a PEP needs EDD: T3 steps and senior approval" =
  let inputs = { Risk.Inputs.baseline with pep = Some foreign_pep } in
  let case = ready_for_approval ~inputs () in
  print_s [%sexp (Kyc_case.cdd_level case ~now:t0 : Cdd_level.t)];
  print_s [%sexp (Kyc_case.can_approve case ~now:t0 : unit Or_error.t)];
  let case = apply_all case [ verified Address_proof; verified Source_of_funds ] in
  (* Senior approval must come from an operator and carry a rationale. *)
  let case =
    apply_all
      case
      [ event ~actor:System Senior_approval
      ; event ~actor:op_b Senior_approval
      ; event ~actor:op_b ~note:"source of wealth documented" Senior_approval
      ; status Approve
      ]
  in
  show_case case;
  (* A new assessment needs a new sign-off. *)
  let case =
    apply_all case [ assessed ~inputs:{ inputs with monthly_volume = Up_to_100k } () ]
  in
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect
    {|
    Enhanced
    (Error
     ("enhanced due diligence outstanding"
      (edd
       (Edd_outstanding (triggers (Pep))
        (missing_steps (Address_proof Source_of_funds)) (senior_approval true)))))
    ((event evt-11)
     ("only an operator may do this" (what "give senior approval")
      (actor System)))
    ((event evt-12) "senior approval needs a rationale in the note")
    ((status Approved) (tier T3) (band (High)) (missing ()))
    ((Edd_outstanding (triggers (Pep)) (missing_steps ()) (senior_approval true)))
    |}]
;;

let%expect_test "simplified and standard due diligence need no senior approval" =
  let case = ready_for_approval () in
  print_s [%sexp (Kyc_case.cdd_level case ~now:t0 : Cdd_level.t)];
  let case = apply_all case [ event ~actor:op_a ~note:"x" Senior_approval ] in
  ignore (case : Kyc_case.t);
  let crypto = { Risk.Inputs.baseline with products = [ Crypto ] } in
  print_s
    [%sexp
      (Kyc_case.cdd_level (ready_for_approval ~inputs:crypto ()) ~now:t0 : Cdd_level.t)];
  [%expect
    {|
    Simplified
    ((event evt-9) "no enhanced due diligence applies")
    Standard
    |}]
;;

let%expect_test "periodic review due dates follow the band" =
  List.iter
    [ "Low", Risk.Inputs.baseline
    ; ( "Medium"
      , { Risk.Inputs.baseline with residence = Elevated; channel = Remote_unattended } )
    ; ( "High"
      , { Risk.Inputs.baseline with
          sector = High
        ; residence = Elevated
        ; channel = Remote_unattended
        } )
    ]
    ~f:(fun (name, inputs) ->
      let case =
        apply_all
          (ready_for_approval ~inputs ())
          [ verified Address_proof
          ; verified Source_of_funds
          ; event ~actor:op_b ~note:"high-risk profile reviewed" Senior_approval
          ; status Approve
          ]
      in
      print_s
        [%message
          name
            ~band:(Kyc_case.band case : Risk.Band.t option)
            ~due:(Kyc_case.next_review_due case : Timestamp.t option)]);
  (* A band change moves the due date. *)
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  print_s [%sexp (Kyc_case.next_review_due case : Timestamp.t option)];
  let case =
    apply_all case [ assessed ~override:(Medium, "employer under investigation") () ]
  in
  print_s [%sexp (Kyc_case.next_review_due case : Timestamp.t option)];
  [%expect
    {|
    ((event evt-11) "no enhanced due diligence applies")
    (Low (band (Low)) (due ("2031-10-12 09:12:00Z")))
    ((event evt-11) "no enhanced due diligence applies")
    (Medium (band (Medium)) (due ("2029-10-12 09:12:00Z")))
    (High (band (High)) (due ("2027-10-13 09:12:00Z")))
    ("2031-10-12 09:09:00Z")
    ("2029-10-12 09:09:00Z")
    |}]
;;

let%expect_test "completing a periodic review" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  show_follow_ups case ~now:"2032-01-01 00:00:00Z";
  let case = apply_all case [ event ~actor:op_a Periodic_review_completed ] in
  let case =
    apply_all
      case
      [ assessed (); event ~actor:op_a ~note:"no changes" Periodic_review_completed ]
  in
  print_s [%sexp (Kyc_case.next_review_due case : Timestamp.t option)];
  [%expect
    {|
    ((Periodic_review_due "2031-10-12 09:09:00Z"))
    ((event evt-10) "a review needs a risk assessment since the last review")
    ("2031-10-12 09:12:00Z")
    |}]
;;

let%expect_test "a confirmed sanctions match blocks the customer, silently" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  let case =
    apply_all
      case
      [ screened ~reason:List_update "scr-2" [ hit Eu_fsf "eu-77" ]
      ; review "scr-2" "eu-77" (Confirm "same name, date of birth and nationality")
      ]
  in
  show_case case;
  print_s [%sexp (Kyc_case.customer_view case : Kyc_case.Customer_view.t)];
  print_endline "-- customer audit";
  print_audit ~audience:Customer case;
  print_endline "-- compliance audit, last three";
  List.iter
    (List.rev (List.take (List.rev (Audit.entries (Kyc_case.audit case))) 3))
    ~f:(fun e -> print_s [%sexp (e : Audit.Entry.t)]);
  [%expect
    {|
    ((status (Blocked (reason "confirmed sanctions match") (previous Approved)))
     (tier T0) (band (Low)) (missing (Address_proof Source_of_funds)))
    ((status Under_review) (tier T0) (steps_to_complete ())
     (fields_to_resubmit ()))
    -- customer audit
    Opened
    (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Created) (to_ Verifying))
    (Step (step Identity_document) (from Pending)
     (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Liveness) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Verifying) (to_ Approved))
    -- compliance audit, last three
    ((at "2026-10-13 09:10:00Z") (actor System) (event_id evt-10)
     (change (Screened (screening_id scr-2) (reason List_update) (hits 1)))
     (why ()))
    ((at "2026-10-13 09:11:00Z") (actor (Operator op-a)) (event_id evt-11)
     (change
      (Hit_reviewed (screening_id scr-2) (entry_id eu-77)
       (action (Confirm "same name, date of birth and nationality"))))
     (why ()))
    ((at "2026-10-13 09:11:00Z") (actor (Operator op-a)) (event_id evt-11)
     (change
      (Status (from Approved)
       (to_ (Blocked (reason "confirmed sanctions match") (previous Approved)))))
     (why ()))
    |}]
;;

let%expect_test "monitoring, investigation and report" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  let alert = Monitoring.Alert_id.of_string "alert-1" in
  let inv = Investigation.Investigation_id.of_string "inv-1" in
  let case =
    apply_all
      case
      [ event
          ~actor:System
          (Alert_raised
             { alert_id = alert
             ; source = Transaction_monitoring
             ; severity = High
             ; summary = "inflows far above declared income"
             })
      ; event ~actor:System (Alert_decided { alert_id = alert; decision = Escalated })
      ; event
          ~actor:op_a
          (Investigation_opened { investigation_id = inv; alerts = [ alert ] })
      ; event ~actor:op_a (Alert_decided { alert_id = alert; decision = Escalated })
      ; event
          ~actor:op_a
          (Investigation_opened { investigation_id = inv; alerts = [ alert ] })
      ]
  in
  print_s [%sexp (Kyc_case.edd_triggers case ~now:t0 : Risk.Edd_trigger.t list)];
  print_s [%sexp (Kyc_case.cdd_level case ~now:t0 : Cdd_level.t)];
  let case =
    apply_all
      case
      [ event
          ~actor:op_a
          (Investigation_moved { investigation_id = inv; state = Investigating })
      ; event
          ~actor:op_a
          (Investigation_moved { investigation_id = inv; state = Pending_decision })
      ; event ~actor:op_a (Report_changed { investigation_id = inv; state = Draft })
      ; event ~actor:op_a (Report_changed { investigation_id = inv; state = Approved })
      ; event ~actor:op_b (Report_changed { investigation_id = inv; state = Approved })
      ; event
          ~actor:op_a
          (Investigation_moved { investigation_id = inv; state = Closed_reported })
      ; event ~actor:op_a (Report_changed { investigation_id = inv; state = Filed })
      ; event
          ~actor:op_a
          (Investigation_moved { investigation_id = inv; state = Closed_reported })
      ; event
          ~actor:System
          (Fiu_request_received
             { request_id = Investigation.Request_id.of_string "fiu-1"
             ; received_on = Date.of_string "2026-10-16"
             })
      ]
  in
  show_follow_ups case ~now:"2026-10-17 00:00:00Z";
  print_s [%sexp (Kyc_case.customer_view case : Kyc_case.Customer_view.t)];
  print_endline "-- support sees";
  print_audit ~audience:Support case;
  print_endline "-- customer sees";
  print_audit ~audience:Customer case;
  [%expect
    {|
    ((event evt-11)
     ("only an operator may do this" (what "decide an alert") (actor System)))
    ((event evt-12) ("only escalated alerts are investigated" (id alert-1)))
    (Unusual_transaction)
    Enhanced
    ((event evt-18) "a report is approved by someone other than its drafter")
    ((event evt-20) "the report must be filed before closing as reported")
    ((Edd_outstanding (triggers (Unusual_transaction))
      (missing_steps (Address_proof Source_of_funds)) (senior_approval true))
     (Fiu_request_due ((fiu-1 2026-10-23))))
    ((status Active) (tier T2) (steps_to_complete ()) (fields_to_resubmit ()))
    -- support sees
    Opened
    (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Created) (to_ Verifying))
    (Step (step Identity_document) (from Pending)
     (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Liveness) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Screened (screening_id scr-1) (reason Onboarding) (hits 0))
    (Risk_assessed (score 0) (computed_band Low) (band Low)
     (model_version kyc-risk-2026-10))
    (Status (from Verifying) (to_ Approved))
    -- customer sees
    Opened
    (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Created) (to_ Verifying))
    (Step (step Identity_document) (from Pending)
     (to_ (Verified "2026-10-13 09:00:00Z")))
    (Step (step Liveness) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z")))
    (Status (from Verifying) (to_ Approved))
    |}]
;;

let%expect_test "screening finds a PEP the assessment missed" =
  let case =
    apply_all
      (ready_for_approval ())
      [ screened "scr-2" [ hit Pep_list "pep-3" ]
      ; review "scr-2" "pep-3" (Confirm "same date of birth and address")
      ]
  in
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect {| (Risk_assessment_needed) |}]
;;

let%expect_test "rejection and closure start retention; legal holds extend it" =
  let refused =
    apply_all (ready_for_approval ()) [ status (Reject "documents forged") ]
  in
  print_s [%sexp (Kyc_case.retain_until refused : Timestamp.t option)];
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  print_s [%sexp (Kyc_case.retain_until case : Timestamp.t option)];
  let case =
    apply_all case [ status ~note:"customer request" (Close "account closed") ]
  in
  print_s [%sexp (Kyc_case.retain_until case : Timestamp.t option)];
  print_s [%sexp (Kyc_case.customer_view case : Kyc_case.Customer_view.t)];
  (* Frozen after closure, apart from monitoring, reporting and retention. *)
  let case = apply_all case [ verified Address_proof; assessed () ] in
  let hold until : Retention.Legal_hold.t =
    { until = Time_ns.of_string_with_utc_offset until
    ; reason = "ongoing criminal investigation"
    ; authority = "public prosecutor"
    }
  in
  let case =
    apply_all
      case
      [ event ~actor:op_a (Legal_hold_set (hold "2040-01-01 00:00:00Z"))
      ; event ~actor:op_a (Legal_hold_set (hold "2033-01-01 00:00:00Z"))
      ]
  in
  print_s [%sexp (Kyc_case.retain_until case : Timestamp.t option)];
  show_follow_ups case ~now:"2032-01-01 00:00:00Z";
  show_follow_ups case ~now:"2033-01-02 00:00:00Z";
  let case = apply_all case [ event ~actor:op_a Legal_hold_released ] in
  show_follow_ups case ~now:"2032-01-01 00:00:00Z";
  [%expect
    {|
    ("2031-10-12 09:09:00Z")
    ()
    ("2031-10-12 09:10:00Z")
    ((status Closed) (tier T0) (steps_to_complete ()) (fields_to_resubmit ()))
    ((event evt-11) ("case is closed" (status (Closed "account closed"))))
    ((event evt-12) ("case is closed" (status (Closed "account closed"))))
    ((event evt-13)
     ("a legal hold extends retention by at most 5 years"
      (retention_ends "2031-10-12 09:10:00Z") (until "2040-01-01 00:00:00Z")))
    ("2033-01-01 00:00:00Z")
    ()
    ((Retention_ended "2033-01-01 00:00:00Z"))
    ((Retention_ended "2031-10-12 09:10:00Z"))
    |}]
;;
