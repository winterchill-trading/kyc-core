open! Core
open Kyc_core
module G = Quickcheck.Generator

let customer = Customer_id.of_string "cus-1"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

(* Mostly plausible events so that random streams reach every status; the derived
   generator adds arbitrary ones. Ids come from a small pool, so streams also reuse ids
   with different content. *)
let kind_gen : Event.Kind.t G.t =
  let open G.Let_syntax in
  let plausible =
    G.weighted_union
      [ ( 4.
        , let%map step = Verification.Step.quickcheck_generator
          and state =
            G.of_list
              Verification.State.[ Sent; Verified t0; Failed "blurry"; Expired; Pending ]
          in
          Event.Kind.Step_changed { step; state } )
      ; ( 3.
        , let%map transition =
            (* Reject and Block are near-absorbing; keep them rare so streams also reach
               Approved. *)
            G.weighted_union
              (List.map
                 Status.Transition.
                   [ 3., Submit
                   ; 3., Approve
                   ; 0.3, Reject "no"
                   ; 1., Request_resubmission [ Mobile ]
                   ; 0.3, Block "review"
                   ; 1., Unblock
                   ]
                 ~f:(fun (w, t) -> w, G.return t))
          in
          Event.Kind.Status_changed transition )
      ; ( 1.
        , let%map n = Int.gen_incl 0 2
          and outcome =
            G.weighted_union
              [ 3., G.return Screening.Outcome.Clear
              ; 1., Screening.Outcome.quickcheck_generator
              ]
          in
          Event.Kind.Screened
            { screening_id = Screening_id.of_string (sprintf "scr-%d" n); outcome } )
      ; ( 1.
        , let%map n = Int.gen_incl 0 2
          and decision =
            G.of_list Screening.Decision.[ False_positive "ok"; True_match "same" ]
          in
          Event.Kind.Screening_reviewed
            { screening_id = Screening_id.of_string (sprintf "scr-%d" n); decision } )
      ]
  in
  G.weighted_union [ 9., plausible; 1., Event.Kind.quickcheck_generator ]
;;

let event_gen : Event.t G.t =
  let open G.Let_syntax in
  let%map n = Int.gen_incl 0 99
  and secs = Int.gen_incl 0 86_400
  and actor =
    G.of_list
      [ Actor.System; Provider "provider-a"; Operator (Operator_id.of_string "op-1") ]
  and kind = kind_gen in
  { Event.id = Event_id.of_string (sprintf "evt-%d" n)
  ; customer_id = customer
  ; at = Time_ns.add t0 (Time_ns.Span.of_int_sec secs)
  ; actor
  ; note = None
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
    (G.both (List.gen_with_length 40 event_gen) event_gen)
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
    (List.gen_with_length 40 event_gen)
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
  in
  Quickcheck.iter ~trials:2_000 (List.gen_with_length 40 event_gen) ~f:(fun events ->
    List.iter
      (Audit.entries (Kyc_case.audit (replay events)))
      ~f:(fun entry ->
        match entry.change with
        | Status { to_; from = _ } -> Hash_set.add seen (name to_)
        | _ -> ()));
  print_s
    [%sexp (Hash_set.to_list seen |> List.sort ~compare:String.compare : string list)];
  [%expect {| (Approved Blocked Created Needs_resubmission Rejected Verifying) |}]
;;
