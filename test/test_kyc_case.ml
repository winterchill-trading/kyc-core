open! Core
open Kyc_core

let customer = Customer_id.of_string "cus-1"
let operator = Actor.Operator (Operator_id.of_string "op-1")
let provider = Actor.Provider "provider-a"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let person = Test_person.person

(* Events are built as drafts and numbered when applied, so ids and times follow
   application order (OCaml evaluates list literals right to left). *)
let counter = ref 0

let number (draft : int -> Event.t) =
  incr counter;
  draft !counter
;;

let event ?(actor = provider) ?note ?id kind n : Event.t =
  let id = Option.value id ~default:(sprintf "evt-%d" n) |> Event_id.of_string in
  { id
  ; customer_id = customer
  ; at = Time_ns.add t0 (Time_ns.Span.of_int_sec (60 * n))
  ; actor
  ; note
  ; kind
  }
;;

let verified step = event (Step_changed { step; state = Verified t0 })

let status ?(actor = operator) ?note transition =
  event ~actor ?note (Status_changed transition)
;;

let standard : Risk.Factors.t =
  { country = Low; product = Low; channel = Medium; pep = Not_pep; adverse_media = false }
;;

let assessed ?(factors = standard) () =
  event ~actor:operator (Risk_assessed { factors; override = None })
;;

let show_case case =
  print_s
    [%message
      ""
        ~status:(Kyc_case.status case : Status.t)
        ~tier:(Kyc_case.tier case : Tier.t)
        ~verified_tier:(Kyc_case.verified_tier case : Tier.t)
        ~missing:(Kyc_case.missing_steps case : Verification.Step.Set.t)]
;;

let apply_all case events =
  List.fold events ~init:case ~f:(fun case draft ->
    match Kyc_case.apply case (number draft) with
    | Ok case -> case
    | Error err ->
      print_s [%sexp (err : Error.t)];
      case)
;;

let opened () =
  counter := 0;
  apply_all (Kyc_case.create customer) [ event (Opened person) ]
;;

let%expect_test "happy path to T2 approval" =
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
      ; event
          ~actor:System
          (Screened { screening_id = Screening_id.of_string "scr-1"; outcome = Clear })
      ; assessed ()
      ]
  in
  show_case case;
  let case = apply_all case [ status ~note:"documents consistent" Approve ] in
  show_case case;
  List.iter
    (Audit.entries (Kyc_case.audit case))
    ~f:(fun e -> print_s [%sexp (e : Audit.Entry.t)]);
  [%expect
    {|
    ((status Created) (tier T0) (verified_tier T0) (missing (Email Mobile)))
    ((status Created) (tier T1) (verified_tier T1)
     (missing (Identity_document Liveness)))
    ((status Verifying) (tier T1) (verified_tier T2)
     (missing (Address_proof Source_of_funds)))
    ((status Approved) (tier T2) (verified_tier T2)
     (missing (Address_proof Source_of_funds)))
    ((at "2026-10-13 09:01:00Z") (actor (Provider provider-a)) (event_id evt-1)
     (change Opened) (why ()))
    ((at "2026-10-13 09:02:00Z") (actor (Provider provider-a)) (event_id evt-2)
     (change
      (Step (step Email) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z"))))
     (why ()))
    ((at "2026-10-13 09:03:00Z") (actor (Provider provider-a)) (event_id evt-3)
     (change
      (Step (step Mobile) (from Pending) (to_ (Verified "2026-10-13 09:00:00Z"))))
     (why ()))
    ((at "2026-10-13 09:04:00Z") (actor System) (event_id evt-4)
     (change (Status (from Created) (to_ Verifying))) (why ()))
    ((at "2026-10-13 09:05:00Z") (actor (Provider provider-a)) (event_id evt-5)
     (change (Step (step Identity_document) (from Pending) (to_ Sent))) (why ()))
    ((at "2026-10-13 09:06:00Z") (actor (Provider provider-a)) (event_id evt-6)
     (change
      (Step (step Identity_document) (from Sent)
       (to_ (Verified "2026-10-13 09:00:00Z"))))
     (why ()))
    ((at "2026-10-13 09:07:00Z") (actor (Provider provider-a)) (event_id evt-7)
     (change
      (Step (step Liveness) (from Pending)
       (to_ (Verified "2026-10-13 09:00:00Z"))))
     (why ()))
    ((at "2026-10-13 09:08:00Z") (actor System) (event_id evt-8)
     (change (Screened (screening_id scr-1) (hits 0))) (why ()))
    ((at "2026-10-13 09:09:00Z") (actor (Operator op-1)) (event_id evt-9)
     (change (Risk_assessed (computed Medium) (rating Medium))) (why ()))
    ((at "2026-10-13 09:10:00Z") (actor (Operator op-1)) (event_id evt-10)
     (change (Status (from Verifying) (to_ Approved)))
     (why ("documents consistent")))
    |}]
;;

let%expect_test "approval guards" =
  let case = apply_all (opened ()) [ verified Email; verified Mobile; status Submit ] in
  print_s [%sexp (Kyc_case.can_approve case : unit Or_error.t)];
  let case = apply_all case [ verified Identity_document; verified Liveness ] in
  print_s [%sexp (Kyc_case.can_approve case : unit Or_error.t)];
  let hit : Screening.Hit.t =
    { list_kind = Sanctions
    ; list_name = "sanctions-list"
    ; entry_ref = "e-7"
    ; matched_name = "A. Example"
    ; score = 90
    }
  in
  let scr = Screening_id.of_string "scr-1" in
  let case =
    apply_all
      case
      [ event ~actor:System (Screened { screening_id = scr; outcome = Hits [ hit ] }) ]
  in
  print_s [%sexp (Kyc_case.can_approve case : unit Or_error.t)];
  (* Approving now is an error and leaves the case as it was. *)
  let before = case in
  let case = apply_all case [ status Approve ] in
  print_s [%sexp (Kyc_case.equal before case : bool)];
  let case =
    apply_all
      case
      [ event
          ~actor:operator
          (Screening_reviewed
             { screening_id = scr; decision = False_positive "different date of birth" })
      ; status Approve
      ]
  in
  show_case case;
  let case = apply_all case [ assessed (); status Approve ] in
  show_case case;
  [%expect
    {|
    (Error
     ("steps not verified for approval" (approval_tier T2)
      (missing (Identity_document Liveness))))
    (Error "no screening recorded")
    (Error ("screening blocks approval" (screenings ((scr-1 Pending_review)))))
    ((event evt-8)
     ("screening blocks approval" (screenings ((scr-1 Pending_review)))))
    true
    ((event evt-10) "no risk assessment recorded")
    ((status Verifying) (tier T1) (verified_tier T2)
     (missing (Address_proof Source_of_funds)))
    ((status Approved) (tier T2) (verified_tier T2)
     (missing (Address_proof Source_of_funds)))
    |}]
;;

let%expect_test "illegal events are errors, never exceptions" =
  let fresh = Kyc_case.create customer in
  counter := 0;
  let show case e =
    print_s
      [%sexp
        (Kyc_case.apply case (number e) |> Result.map ~f:Kyc_case.status
         : Status.t Or_error.t)]
  in
  show fresh (verified Email);
  let case = opened () in
  show case (event (Opened person));
  show case (status Approve);
  show case (event (Step_changed { step = Liveness; state = Expired }));
  show case (event (Person_updated person));
  show case (fun n ->
    { (verified Email n) with customer_id = Customer_id.of_string "cus-2" });
  show
    case
    (event
       (Screening_reviewed
          { screening_id = Screening_id.of_string "nope"; decision = True_match "" }));
  show case (status ~actor:System (Block "suspicious"));
  let case = apply_all case [ status Submit ] in
  show case (event (Person_updated { person with email = Pii.create "new@example.com" }));
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
    (Error ((event evt-7) ("unknown screening" (screening_id nope))))
    (Error
     ((event evt-8)
      ("only a compliance operator may block or unblock" (actor System)
       (transition (Block suspicious)))))
    (Error
     ((event evt-10)
      ("person can only change before submission or when resubmission is requested"
       (status Verifying))))
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
  show_case case;
  let moved =
    { person with
      residential_address =
        Pii.map person.residential_address ~f:(fun a -> { a with city = "Hamburg" })
    }
  in
  let case =
    apply_all
      case
      [ event ~actor:System (Person_updated moved); status ~actor:System Submit ]
  in
  show_case case;
  List.iter
    (Audit.entries (Kyc_case.audit case))
    ~f:(fun e -> print_s [%sexp (e.change : Audit.Change.t)]);
  [%expect
    {|
    ((status (Needs_resubmission (Residential_address))) (tier T1)
     (verified_tier T1) (missing (Identity_document Liveness)))
    ((status Verifying) (tier T1) (verified_tier T1)
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

let%expect_test "block and unblock" =
  let case = apply_all (opened ()) [ verified Email; verified Mobile ] in
  show_case case;
  let case =
    apply_all
      case
      [ status ~note:"adverse media under review" (Block "compliance review") ]
  in
  show_case case;
  let case = apply_all case [ status Unblock ] in
  show_case case;
  [%expect
    {|
    ((status Created) (tier T1) (verified_tier T1)
     (missing (Identity_document Liveness)))
    ((status (Blocked (reason "compliance review") (previous Created))) (tier T0)
     (verified_tier T1) (missing (Identity_document Liveness)))
    ((status Created) (tier T1) (verified_tier T1)
     (missing (Identity_document Liveness)))
    |}]
;;

let%expect_test "rejected is final" =
  let case =
    apply_all
      (opened ())
      [ verified Email
      ; verified Mobile
      ; status Submit
      ; status (Reject "document forged")
      ]
  in
  show_case case;
  let case = apply_all case [ status Submit; status Approve ] in
  show_case case;
  [%expect
    {|
    ((status (Rejected "document forged")) (tier T0) (verified_tier T1)
     (missing (Identity_document Liveness)))
    ((event evt-6)
     ("illegal status transition" (from (Rejected "document forged"))
      (transition Submit)))
    ((event evt-7)
     ("illegal status transition" (from (Rejected "document forged"))
      (transition Approve)))
    ((status (Rejected "document forged")) (tier T0) (verified_tier T1)
     (missing (Identity_document Liveness)))
    |}]
;;

let%expect_test "a redelivered event is a no-op; a reused id is an error" =
  let case = opened () in
  let e =
    event
      ~id:"provider-delivery-9"
      (Step_changed { step = Email; state = Verified t0 })
      99
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

let ready_for_approval ?(factors = standard) () =
  apply_all
    (opened ())
    [ verified Email
    ; verified Mobile
    ; status Submit
    ; verified Identity_document
    ; verified Liveness
    ; event
        ~actor:System
        (Screened { screening_id = Screening_id.of_string "scr-1"; outcome = Clear })
    ; assessed ~factors ()
    ]
;;

let show_follow_ups case ~now =
  print_s
    [%sexp
      (Kyc_case.follow_ups case ~now:(Time_ns.of_string_with_utc_offset now)
       : Kyc_case.Follow_up.t list)]
;;

let%expect_test "a foreign PEP needs EDD: T3 steps and senior approval" =
  let case = ready_for_approval ~factors:{ standard with pep = Foreign } () in
  print_s [%sexp (Kyc_case.edd_triggers case : Risk.Edd_trigger.t list)];
  print_s [%sexp (Kyc_case.can_approve case : unit Or_error.t)];
  let case = apply_all case [ verified Address_proof; verified Source_of_funds ] in
  print_s [%sexp (Kyc_case.can_approve case : unit Or_error.t)];
  (* Senior approval must come from an operator and carry a rationale. *)
  let case =
    apply_all
      case
      [ event ~actor:System Senior_approval
      ; event ~actor:operator Senior_approval
      ; event ~actor:operator ~note:"source of wealth documented" Senior_approval
      ; status Approve
      ]
  in
  show_case case;
  (* A new assessment needs a new sign-off before the next review. *)
  let case =
    apply_all
      case
      [ assessed ~factors:{ standard with pep = Foreign; product = High } () ]
  in
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect
    {|
    (High_risk_rating Foreign_pep)
    (Error
     ("enhanced due diligence outstanding"
      (edd
       (Edd_outstanding (triggers (High_risk_rating Foreign_pep))
        (missing_steps (Address_proof Source_of_funds)) (senior_approval true)))))
    (Error
     ("enhanced due diligence outstanding"
      (edd
       (Edd_outstanding (triggers (High_risk_rating Foreign_pep))
        (missing_steps ()) (senior_approval true)))))
    ((event evt-11)
     ("only an operator may do this" (what "give senior approval")
      (actor System)))
    ((event evt-12) "senior approval needs a rationale in the note")
    ((status Approved) (tier T3) (verified_tier T3) (missing ()))
    ((Edd_outstanding (triggers (High_risk_rating Foreign_pep))
      (missing_steps ()) (senior_approval true)))
    |}]
;;

let%expect_test "standard due diligence needs no senior approval" =
  let case = ready_for_approval () in
  print_s
    [%sexp
      (Kyc_case.apply case (number (event ~actor:operator ~note:"x" Senior_approval))
       |> Result.map ~f:Kyc_case.status
       : Status.t Or_error.t)];
  [%expect {| (Error ((event evt-9) "no enhanced due diligence applies")) |}]
;;

let%expect_test "periodic review due dates follow the rating" =
  List.iter Risk.Level.all ~f:(fun level ->
    let factors = { standard with country = level; channel = level } in
    let edd =
      match level with
      | High ->
        [ verified Address_proof
        ; verified Source_of_funds
        ; event
            ~actor:operator
            ~note:"high-risk country, funds documented"
            Senior_approval
        ]
      | Low | Medium -> []
    in
    let case = apply_all (ready_for_approval ~factors ()) (edd @ [ status Approve ]) in
    print_s
      [%message
        ""
          ~rating:(level : Risk.Level.t)
          ~approved:(Kyc_case.last_review_at case : Timestamp.t option)
          ~due:(Kyc_case.next_review_due case : Timestamp.t option)]);
  [%expect
    {|
    ((rating Low) (approved ("2026-10-13 09:09:00Z"))
     (due ("2031-10-12 09:09:00Z")))
    ((rating Medium) (approved ("2026-10-13 09:09:00Z"))
     (due ("2029-10-12 09:09:00Z")))
    ((rating High) (approved ("2026-10-13 09:12:00Z"))
     (due ("2027-10-13 09:12:00Z")))
    |}]
;;

let%expect_test "completing a periodic review" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  show_follow_ups case ~now:"2030-01-01 00:00:00Z";
  (* The review needs a fresh rating. *)
  let case = apply_all case [ event ~actor:operator Periodic_review_completed ] in
  let case =
    apply_all
      case
      [ assessed (); event ~actor:operator ~note:"no changes" Periodic_review_completed ]
  in
  print_s [%sexp (Kyc_case.next_review_due case : Timestamp.t option)];
  show_follow_ups case ~now:"2030-01-01 00:00:00Z";
  [%expect
    {|
    ((Periodic_review_due "2029-10-12 09:09:00Z"))
    ((event evt-10) "a review needs a risk assessment since the last review")
    ("2029-10-12 09:12:00Z")
    ((Periodic_review_due "2029-10-12 09:12:00Z"))
    |}]
;;

let%expect_test "monitoring alerts" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  let alert = Monitoring.Alert_id.of_string "alert-1" in
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
      ]
  in
  print_s [%sexp (Kyc_case.edd_triggers case : Risk.Edd_trigger.t list)];
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  let case =
    apply_all
      case
      [ event ~actor:System (Alert_closed { alert_id = alert; resolution = No_action })
      ; event
          ~actor:operator
          ~note:"salary change confirmed"
          (Alert_closed { alert_id = alert; resolution = No_action })
      ; event ~actor:operator (Alert_closed { alert_id = alert; resolution = Reported })
      ]
  in
  print_s [%sexp (Kyc_case.edd_triggers case : Risk.Edd_trigger.t list)];
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect
    {|
    (Monitoring_alert)
    ((Edd_outstanding (triggers (Monitoring_alert))
      (missing_steps (Address_proof Source_of_funds)) (senior_approval true))
     (Open_alerts (alert-1)))
    ((event evt-11)
     ("only an operator may do this" (what "close an alert") (actor System)))
    ((event evt-13) ("alert already closed" (alert_id alert-1)))
    ()
    ()
    |}]
;;

let%expect_test "screening finds a PEP the rating missed" =
  let case = ready_for_approval () in
  let scr = Screening_id.of_string "scr-2" in
  let hit : Screening.Hit.t =
    { list_kind = Pep
    ; list_name = "pep-list"
    ; entry_ref = "e-1"
    ; matched_name = "A. Example"
    ; score = 97
    }
  in
  let case =
    apply_all
      case
      [ event ~actor:System (Screened { screening_id = scr; outcome = Hits [ hit ] })
      ; event
          ~actor:operator
          ~note:"same date of birth and address"
          (Screening_reviewed { screening_id = scr; decision = True_match "confirmed" })
      ]
  in
  show_follow_ups case ~now:"2026-10-14 00:00:00Z";
  [%expect {| (Risk_assessment_needed) |}]
;;

let%expect_test "closure starts the retention clock and freezes the record" =
  let case = apply_all (ready_for_approval ()) [ status Approve ] in
  let case =
    apply_all case [ status ~note:"customer request" (Close "account closed") ]
  in
  show_case case;
  print_s [%sexp (Kyc_case.retain_until case : Timestamp.t option)];
  let case = apply_all case [ verified Address_proof; assessed () ] in
  show_follow_ups case ~now:"2031-10-12 00:00:00Z";
  show_follow_ups case ~now:"2031-10-13 00:00:00Z";
  [%expect
    {|
    ((status (Closed "account closed")) (tier T0) (verified_tier T2)
     (missing (Address_proof Source_of_funds)))
    ("2031-10-12 09:10:00Z")
    ((event evt-11) ("case is closed" (status (Closed "account closed"))))
    ((event evt-12) ("case is closed" (status (Closed "account closed"))))
    ()
    ((Retention_ended "2031-10-12 09:10:00Z"))
    |}]
;;
