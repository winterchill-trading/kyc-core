open! Core
open Kyc_core
open Kyb_fixtures

let%expect_test "opening checks the entity and the jurisdiction" =
  reset ();
  let policy =
    { Kyb_case.Policy.default with
      requirements = { Kyb_requirements.default with unlisted_covered = false }
    }
  in
  let _ : Kyb_case.t =
    apply_all (Kyb_case.create ~policy entity_id) [ event (Opened entity) ]
  in
  let _ : Kyb_case.t =
    apply_all (Kyb_case.create entity_id) [ event (Opened { entity with legal_name = " x" }) ]
  in
  let case = opened () in
  show_case case;
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  [%expect {|
    ((event kyb-1) ("jurisdiction not covered" (jurisdiction GB)))
    ((event kyb-2) ("invalid legal entity" (errors ("invalid legal name"))))
    ((status Draft) (round 0) (band ())
     (missing
      ((Entity_field Registration_number) (Entity_field Incorporation_date)
       (Entity_field Registered_address) (Entity_field Industry) Parties
       Beneficial_owners
       (Document ((kind Certificate_of_incorporation) (party ())))
       (Document ((kind Articles_of_association) (party ())))
       (Document ((kind Shareholder_register) (party ())))
       (Document ((kind Register_of_directors) (party ()))) Source_of_funds))
     (parties_pending ()))
    ((status Action_required) (round 0) (requested ())
     (missing
      ((Entity_field Registration_number) (Entity_field Incorporation_date)
       (Entity_field Registered_address) (Entity_field Industry) Parties
       Beneficial_owners
       (Document ((kind Certificate_of_incorporation) (party ())))
       (Document ((kind Articles_of_association) (party ())))
       (Document ((kind Shareholder_register) (party ())))
       (Document ((kind Register_of_directors) (party ()))) Source_of_funds))
     (parties_pending ()) (reason ()))
    |}]
;;

let%expect_test "drafting: entity, parties, documents, source of funds" =
  let case = opened () in
  let case =
    apply_all
      case
      [ event (Entity_updated entity)
      ; event (Entity_updated entity)
      ; event (Parties_set parties)
      ; document "doc-coi" Certificate_of_incorporation
      ; document ~party:"zed" "doc-x" Power_of_attorney
      ; document "doc-coi" Register_extract
      ]
  in
  show_case case;
  let case =
    apply_all
      case
      (List.tl_exn all_documents @ [ event (Source_of_funds_declared source_of_funds) ])
  in
  show_case case;
  let case = apply_all case [ event (Document_removed (Document_id.of_string "doc-poa")) ] in
  show_case case;
  [%expect {|
    ((event kyb-3) "entity update changes nothing")
    ((event kyb-6) "document about someone who is not a party")
    ((event kyb-7) ("document already recorded" (id doc-coi)))
    ((status Draft) (round 0) (band ())
     (missing
      ((Document ((kind Articles_of_association) (party ())))
       (Document ((kind Shareholder_register) (party ())))
       (Document ((kind Register_of_directors) (party ())))
       (Document ((kind Ownership_chart) (party ())))
       (Document ((kind Power_of_attorney) (party (carol)))) Source_of_funds))
     (parties_pending (alice bob carol)))
    ((status Draft) (round 0) (band ()) (missing ())
     (parties_pending (alice bob carol)))
    ((status Draft) (round 0) (band ())
     (missing ((Document ((kind Power_of_attorney) (party (carol))))))
     (parties_pending (alice bob carol)))
    |}]
;;

let%expect_test "submit needs a complete case and opens round 1" =
  let _ : Kyb_case.t = apply_all (opened ()) [ status Submit ] in
  let case = complete () in
  let case = apply_all case [ status Submit ] in
  show_case case;
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect {|
    ((event kyb-2)
     ("KYB case incomplete"
      (missing
       ((Entity_field Registration_number) (Entity_field Incorporation_date)
        (Entity_field Registered_address) (Entity_field Industry) Parties
        Beneficial_owners
        (Document ((kind Certificate_of_incorporation) (party ())))
        (Document ((kind Articles_of_association) (party ())))
        (Document ((kind Shareholder_register) (party ())))
        (Document ((kind Register_of_directors) (party ()))) Source_of_funds))))
    ((status Submitted) (round 1) (band ()) (missing ())
     (parties_pending (alice bob carol)))
    ((Awaiting_decision (round 1) (since "2026-10-13 09:11:00Z"))
     (Party_cases_pending (alice bob carol)) Risk_assessment_needed)
    |}]
;;

let%expect_test "nothing changes while under review; jurisdiction fixed after submission" =
  let case = apply_all (complete ()) [ status Submit ] in
  let case =
    apply_all
      case
      [ event (Entity_updated { entity with industry = Some "64.19" })
      ; event (Parties_set (List.tl_exn parties))
      ; document "doc-late" Financial_statements
      ]
  in
  let case =
    apply_all
      case
      [ status ~actor:provider Start_review
      ; status ~actor:provider (Request_information [ Source_of_funds ])
      ; event (Entity_updated { entity with jurisdiction = Country.of_string "IE" })
      ; event (Entity_updated { entity with industry = Some "64.19" })
      ]
  in
  show_case case;
  [%expect {|
    ((event kyb-12)
     ("the case cannot be changed now" (what "update the entity")
      (status Submitted)))
    ((event kyb-13)
     ("the case cannot be changed now" (what "change the parties")
      (status Submitted)))
    ((event kyb-14)
     ("the case cannot be changed now" (what "add a document")
      (status Submitted)))
    ((event kyb-17) "the jurisdiction is fixed after the first submission")
    ((status (More_information (Source_of_funds))) (round 1) (band ())
     (missing ()) (parties_pending (alice bob carol)))
    |}]
;;

let%expect_test "more information, resubmission and stale rounds" =
  let case = apply_all (complete ()) [ status Submit ] in
  let case =
    apply_all
      case
      [ status ~actor:provider ~round:1 Start_review
      ; status
          ~actor:provider
          ~round:1
          ~note:"Please explain the revenue"
          (Request_information
             [ Document { kind = Source_of_funds_evidence; party = None }
             ; Explanation "main customers"
             ])
      ]
  in
  show_case case;
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  let case =
    apply_all
      case
      [ document "doc-sof" Source_of_funds_evidence
      ; status ~round:3 Submit
      ; status ~round:2 Submit
      ; status ~actor:provider ~round:1 (Reject "outside_risk_appetite")
      ]
  in
  show_case case;
  print_s [%sexp (Kyb_case.decisions case : Kyb_case.Decision.t list)];
  [%expect {|
    ((status
      (More_information
       ((Document ((kind Source_of_funds_evidence) (party ())))
        (Explanation "main customers"))))
     (round 1) (band ()) (missing ()) (parties_pending (alice bob carol)))
    ((status Action_required) (round 1)
     (requested
      ((Document ((kind Source_of_funds_evidence) (party ())))
       (Explanation "main customers")))
     (missing ()) (parties_pending (alice bob carol)) (reason ()))
    ((Missing
      ((Document ((kind Source_of_funds_evidence) (party ())))
       (Explanation "main customers")))
     (Party_cases_pending (alice bob carol)) Risk_assessment_needed)
    ((event kyb-15) ("a submission opens the next round" (round 3) (current 1)))
    ((event kyb-17) ("event for another submission round" (round 1) (current 2)))
    ((status Submitted) (round 2) (band ()) (missing ())
     (parties_pending (alice bob carol)))
    (((round 1)
      (outcome
       (More_information
        ((Document ((kind Source_of_funds_evidence) (party ())))
         (Explanation "main customers"))))
      (at "2026-10-13 09:13:00Z") (actor (Provider provider-a))
      (note ("Please explain the revenue"))))
    |}]
;;

let%expect_test "approval needs the people verified and a risk assessment" =
  let case = apply_all (complete ()) [ status Submit ] in
  let case = apply_all case [ status Approve ] in
  let case = apply_all case [ party_case "alice" Approved; party_case "bob" Verifying ] in
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  let case =
    apply_all
      case
      [ party_case "bob" Approved
      ; party_case "carol" Approved
      ; party_case "carol" Approved
      ; party_case "zed" Approved
      ; status Approve
      ; assessed ()
      ; status ~actor:provider ~note:"all checks passed" Approve
      ]
  in
  show_case case;
  print_s [%sexp (Kyb_case.next_review_due case : Timestamp.t option)];
  [%expect {|
    ((event kyb-12) ("linked people not verified" (pending (alice bob carol))))
    ((status Under_review) (round 1) (requested ()) (missing ())
     (parties_pending (bob carol)) (reason ()))
    ((event kyb-17) "party case status unchanged")
    ((event kyb-18) ("not a party to this case" (party zed)))
    ((event kyb-19) "no risk assessment recorded")
    ((status Approved) (round 1) (band (Low)) (missing ()) (parties_pending ()))
    ("2031-10-12 09:21:00Z")
    |}]
;;

let%expect_test "EDD: opaque ownership needs a senior sign-off" =
  let case = ready_for_approval ~inputs:{ Kyb_risk.Inputs.baseline with ownership = Opaque } () in
  print_s [%sexp (Kyb_case.cdd_level case ~now:t0 : Cdd_level.t)];
  let case =
    apply_all
      case
      [ status Approve
      ; event ~actor:provider ~note:"ok" Senior_approval
      ; event ~actor:op_b Senior_approval
      ; event ~actor:op_b ~note:"structure explained by group chart" Senior_approval
      ; status Approve
      ]
  in
  show_case case;
  [%expect {|
    Enhanced
    ((event kyb-16)
     ("enhanced due diligence outstanding"
      (edd
       (Edd_outstanding (triggers (Opaque_ownership)) (senior_approval true)))))
    ((event kyb-17)
     ("only an operator may do this" (what "give senior approval")
      (actor (Provider provider-a))))
    ((event kyb-18) "senior approval needs a rationale in the note")
    ((status Approved) (round 1) (band (High)) (missing ()) (parties_pending ()))
    |}]
;;

let%expect_test "senior approval only under EDD" =
  let case = ready_for_approval () in
  let _ : Kyb_case.t = apply_all case [ event ~actor:op_b ~note:"x" Senior_approval ] in
  [%expect {| ((event kyb-16) "no enhanced due diligence applies") |}]
;;

let%expect_test "rejection starts retention" =
  let case = ready_for_approval () in
  let case = apply_all case [ status (Reject "outside_risk_appetite") ] in
  show_case case;
  print_s [%sexp (Kyb_case.retain_until case : Timestamp.t option)];
  show_follow_ups case ~now:"2032-01-01 00:00:00Z";
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  [%expect {|
    ((status (Rejected outside_risk_appetite)) (round 1) (band (Low))
     (missing ()) (parties_pending ()))
    ("2031-10-12 09:16:00Z")
    ((Retention_ended "2031-10-12 09:16:00Z"))
    ((status Declined) (round 1) (requested ()) (missing ()) (parties_pending ())
     (reason (outside_risk_appetite)))
    |}]
;;

let%expect_test "block and unblock: the applicant only sees Under_review" =
  let case = ready_for_approval () in
  let case =
    apply_all
      case
      [ status ~actor:provider (Block "x")
      ; party_case "bob" (Blocked { reason = "confirmed sanctions match"; previous = Approved })
      ; status ~note:"sanctions match on an owner" (Block "sanctions")
      ]
  in
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  let case = apply_all case [ status Unblock ] in
  print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)];
  let audit audience =
    List.map (Kyb_audit.for_audience (Kyb_case.audit case) audience) ~f:(fun e -> e.change)
    |> List.filter ~f:(function
      | Kyb_audit.Change.Document_added _ -> false
      | _ -> true)
  in
  print_s [%message "customer" ~_:(audit Customer : Kyb_audit.Change.t list)];
  print_s [%message "support" ~_:(audit Support : Kyb_audit.Change.t list)];
  print_s [%message "compliance" ~_:(audit Compliance : Kyb_audit.Change.t list)];
  [%expect {|
    ((event kyb-16) "only an operator may block")
    ((status Under_review) (round 1) (requested ()) (missing ())
     (parties_pending ()) (reason ()))
    ((status Under_review) (round 1) (requested ()) (missing ())
     (parties_pending ()) (reason ()))
    (customer
     (Opened
      (Entity_updated
       (Registration_number Incorporation_date Registered_address Industry))
      (Parties_set (alice bob carol)) Source_of_funds_declared
      (Status (from Draft) (to_ Submitted) (round 1))))
    (support
     (Opened
      (Entity_updated
       (Registration_number Incorporation_date Registered_address Industry))
      (Parties_set (alice bob carol)) Source_of_funds_declared
      (Status (from Draft) (to_ Submitted) (round 1))
      (Party_case_changed (party alice) (status Approved))
      (Party_case_changed (party bob) (status Approved))
      (Party_case_changed (party carol) (status Approved))
      (Risk_assessed (score 10) (computed_band Low) (band Low)
       (model_version kyb-risk-2026-10))))
    (compliance
     (Opened
      (Entity_updated
       (Registration_number Incorporation_date Registered_address Industry))
      (Parties_set (alice bob carol)) Source_of_funds_declared
      (Status (from Draft) (to_ Submitted) (round 1))
      (Party_case_changed (party alice) (status Approved))
      (Party_case_changed (party bob) (status Approved))
      (Party_case_changed (party carol) (status Approved))
      (Risk_assessed (score 10) (computed_band Low) (band Low)
       (model_version kyb-risk-2026-10))
      (Party_case_changed (party bob)
       (status
        (Blocked (reason "confirmed sanctions match") (previous Approved))))
      (Status (from Submitted)
       (to_ (Blocked (reason sanctions) (previous Submitted))) (round 1))
      (Status (from (Blocked (reason sanctions) (previous Submitted)))
       (to_ Submitted) (round 1))))
    |}]
;;

let%expect_test "periodic review" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  show_follow_ups case ~now:"2031-10-14 00:00:00Z";
  let case =
    apply_all
      case
      [ event ~actor:op_a Periodic_review_completed
      ; status
          ~note:"periodic review"
          (Request_information [ Document { kind = Financial_statements; party = None } ])
      ; document "doc-fs" Financial_statements
      ; status Submit
      ; status Approve
      ; assessed ()
      ; event ~actor:System Periodic_review_completed
      ; event ~actor:op_a Periodic_review_completed
      ]
  in
  show_case case;
  print_s [%sexp (Kyb_case.last_review_at case : Timestamp.t option)];
  [%expect {|
    ((Periodic_review_due "2031-10-12 09:16:00Z"))
    ((event kyb-17) "a review needs a risk assessment since the last review")
    ((event kyb-23)
     ("only an operator may do this" (what "complete a periodic review")
      (actor System)))
    ((status Approved) (round 2) (band (Low)) (missing ()) (parties_pending ()))
    ("2026-10-13 09:24:00Z")
    |}]
;;

let%expect_test "closing freezes the case" =
  let case = apply_all (ready_for_approval ()) [ status Approve; status (Close "ended") ] in
  let case =
    apply_all
      case
      [ event (Source_of_funds_declared source_of_funds)
      ; party_case "alice" (Closed "left")
      ; assessed ()
      ; status Unblock
      ]
  in
  show_case case;
  print_s [%sexp (Kyb_case.retain_until case : Timestamp.t option)];
  [%expect {|
    ((event kyb-18) ("case is closed" (status (Closed ended))))
    ((event kyb-19) ("case is closed" (status (Closed ended))))
    ((event kyb-20) ("case is closed" (status (Closed ended))))
    ((event kyb-21)
     ("illegal KYB transition" (from (Closed ended)) (transition Unblock)))
    ((status (Closed ended)) (round 1) (band (Low)) (missing ())
     (parties_pending ()))
    ("2031-10-12 09:17:00Z")
    |}]
;;

let%expect_test "idempotency and foreign events" =
  let case = opened () in
  let e = number (event (Source_of_funds_declared source_of_funds)) in
  let case = Kyb_case.apply case e |> Or_error.ok_exn in
  print_s [%sexp (Kyb_case.apply case e |> Or_error.map ~f:(Kyb_case.equal case) : bool Or_error.t)];
  print_s
    [%sexp
      (Kyb_case.apply case { e with note = Some "changed" } |> Or_error.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  print_s
    [%sexp
      (Kyb_case.apply case { e with id = Event_id.of_string "other"; entity_id = Entity_id.of_string "ent-2" }
       |> Or_error.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  [%expect {|
    (Ok true)
    (Error ("event id already applied with different content" (id kyb-2)))
    (Error ("event is for another entity" (case ent-1) (event ent-2)))
    |}]
;;
