open! Core
open Kyc_core
module G = Quickcheck.Generator

let customer = Customer_id.of_string "cus-1"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

(* Plausible events, weighted so that random streams pass through every status (Reject,
   Block and Close are near-absorbing, so they are rare), plus a few odd ones. Ids come
   from a pool of 100, so 60-event streams often reuse ids with different content. *)
let kind_gen : Event.Kind.t G.t =
  let open G.Let_syntax in
  let n3 = Int.gen_incl 0 2 in
  let scr n = Screening_id.of_string (sprintf "scr-%d" n) in
  let alert n = Monitoring.Alert_id.of_string (sprintf "alert-%d" n) in
  let inv n = Investigation.Investigation_id.of_string (sprintf "inv-%d" n) in
  let hit n : Screening.Hit.t =
    { source = (if n = 0 then Eu_fsf else Pep_list)
    ; list_version = "v1"
    ; entry_id = sprintf "e-%d" n
    ; matched_name = "A. Example"
    ; score = 90
    ; algorithm = "jw"
    }
  in
  G.weighted_union
    [ ( 4.
      , let%map step = Verification.Step.quickcheck_generator
        and state =
          G.weighted_union
            [ 6., G.return (Verification.State.Verified t0)
            ; 1., G.of_list Verification.State.[ Sent; Failed "blurry"; Expired; Pending ]
            ]
        in
        Event.Kind.Step_changed { step; state } )
    ; ( 3.
      , let%map transition =
          G.weighted_union
            (List.map
               Status.Transition.
                 [ 3., Submit
                 ; 3., Approve
                 ; 0.3, Reject "no"
                 ; 1., Request_resubmission [ Mobile ]
                 ; 0.3, Block "review"
                 ; 1., Unblock
                 ; 0.05, Close "customer left"
                 ]
               ~f:(fun (w, t) -> w, G.return t))
        in
        Event.Kind.Status_changed transition )
    ; ( 1.
      , let%map n = n3
        and hits =
          G.weighted_union [ 3., G.return []; 1., G.map n3 ~f:(fun k -> [ hit k ]) ]
        in
        Event.Kind.Screened { screening_id = scr n; reason = Onboarding; hits } )
    ; ( 1.
      , let%map n = n3
        and k = n3
        and action =
          G.of_list
            Screening.Review_action.
              [ Take
              ; Propose_discount "dob differs"
              ; Confirm_discount
              ; Confirm "same"
              ; Escalate "unsure"
              ]
        in
        Event.Kind.Hit_reviewed
          { screening_id = scr n; entry_id = sprintf "e-%d" k; action } )
    ; ( 1.
      , let%map inputs =
          G.weighted_union
            [ 3., G.return Risk.Inputs.baseline; 1., Risk.Inputs.quickcheck_generator ]
        in
        Event.Kind.Risk_assessed (Risk.assess Risk.Model.default inputs ~at:t0) )
    ; 0.3, G.return Event.Kind.Senior_approval
    ; 0.3, G.return Event.Kind.Periodic_review_completed
    ; ( 0.5
      , let%map n = n3
        and severity = Risk.Band.quickcheck_generator in
        Event.Kind.Alert_raised
          { alert_id = alert n
          ; source = Transaction_monitoring
          ; severity
          ; summary = "pattern"
          } )
    ; ( 0.5
      , let%map n = n3
        and decision = Monitoring.Decision.quickcheck_generator in
        Event.Kind.Alert_decided { alert_id = alert n; decision } )
    ; ( 0.3
      , let%map n = n3
        and a = n3 in
        Event.Kind.Investigation_opened { investigation_id = inv n; alerts = [ alert a ] }
      )
    ; ( 0.3
      , let%map n = n3
        and state = Investigation.Case_state.quickcheck_generator in
        Event.Kind.Investigation_moved { investigation_id = inv n; state } )
    ; ( 0.3
      , let%map n = n3
        and state = Investigation.Report_state.quickcheck_generator in
        Event.Kind.Report_changed { investigation_id = inv n; state } )
    ; ( 0.2
      , let%map hold = Retention.Legal_hold.quickcheck_generator in
        Event.Kind.Legal_hold_set hold )
    ; 0.1, G.return Event.Kind.Legal_hold_released
    ; ( 0.2
      , let%map p = Person.quickcheck_generator in
        Event.Kind.Person_updated p )
    ; 0.05, G.return (Event.Kind.Opened Test_person.person)
    ]
;;

let event_gen : Event.t G.t =
  let open G.Let_syntax in
  let%map n = Int.gen_incl 0 99
  and secs = Int.gen_incl 0 86_400
  and actor =
    G.of_list
      [ Actor.System
      ; Provider "provider-a"
      ; Operator (Operator_id.of_string "op-a")
      ; Operator (Operator_id.of_string "op-b")
      ]
  and note = G.weighted_union [ 1., G.return None; 1., G.return (Some "why") ]
  and kind = kind_gen in
  { Event.id = Event_id.of_string (sprintf "evt-%d" n)
  ; customer_id = customer
  ; at = Time_ns.add t0 (Time_ns.Span.of_int_sec secs)
  ; actor
  ; note
  ; kind
  }
;;

let opened =
  { Event.id = Event_id.of_string "open"
  ; customer_id = customer
  ; at = t0
  ; actor = System
  ; note = None
  ; kind = Opened Test_person.person
  }
;;

(* Errors leave the case as it was, which is what a consumer does with them. *)
let apply_lenient case e =
  Kyc_case.apply case e |> Result.ok |> Option.value ~default:case
;;

let replay events =
  List.fold (opened :: events) ~init:(Kyc_case.create customer) ~f:apply_lenient
;;

let%test_unit "applying the same event twice equals applying it once" =
  Quickcheck.test
    (G.both (List.gen_with_length 60 event_gen) event_gen)
    ~sexp_of:[%sexp_of: Event.t list * Event.t]
    ~f:(fun (history, e) ->
      let case = replay history in
      let once = apply_lenient case e in
      let twice = apply_lenient once e in
      [%test_result: Kyc_case.t] twice ~expect:once;
      (* and the second application is never an error once the first succeeded *)
      match Kyc_case.apply case e with
      | Error _ -> ()
      | Ok once -> assert (Result.is_ok (Kyc_case.apply once e)))
;;

(* Only accepted events: an event rejected the first time may legitimately succeed later,
   once the case has moved on. *)
let%test_unit "redelivering every accepted event changes nothing" =
  Quickcheck.test
    (List.gen_with_length 60 event_gen)
    ~sexp_of:[%sexp_of: Event.t list]
    ~f:(fun events ->
      let case, accepted =
        List.fold
          (opened :: events)
          ~init:(Kyc_case.create customer, [])
          ~f:(fun (case, accepted) e ->
            match Kyc_case.apply case e with
            | Ok case -> case, e :: accepted
            | Error _ -> case, accepted)
      in
      [%test_result: Kyc_case.t]
        (List.fold_result (List.rev accepted) ~init:case ~f:Kyc_case.apply
         |> Or_error.ok_exn)
        ~expect:case)
;;

let%expect_test "random streams pass through every status" =
  let seen = Hash_set.create (module String) in
  let name : Status.t -> string = function
    | Created -> "Created"
    | Verifying -> "Verifying"
    | Approved -> "Approved"
    | Rejected _ -> "Rejected"
    | Needs_resubmission _ -> "Needs_resubmission"
    | Blocked _ -> "Blocked"
    | Closed _ -> "Closed"
  in
  Quickcheck.iter ~trials:2_000 (List.gen_with_length 60 event_gen) ~f:(fun events ->
    List.iter
      (Audit.entries (Kyc_case.audit (replay events)))
      ~f:(fun entry ->
        match entry.change with
        | Status { to_; from = _ } -> Hash_set.add seen (name to_)
        | _ -> ()));
  print_s
    [%sexp (Hash_set.to_list seen |> List.sort ~compare:String.compare : string list)];
  [%expect {| (Approved Blocked Closed Created Needs_resubmission Rejected Verifying) |}]
;;
