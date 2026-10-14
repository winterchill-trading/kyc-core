open! Core
open Kyc_core
module G = Quickcheck.Generator

let t0 = Kyb_fixtures.t0

(* Plausible KYB events over the fixture entity and parties, weighted so random streams
   reach every status (Reject, Block and Close are near-absorbing, so they are rare). Ids
   come from a pool of 100, so 60-event streams often reuse ids with different content. *)
let kind_gen : Kyb_event.Kind.t G.t =
  let open G.Let_syntax in
  let party = G.of_list [ "alice"; "bob"; "carol"; "zed" ] in
  let docs = Kyb_fixtures.all_documents in
  G.weighted_union
    [ ( 3.
      , let%map transition =
          G.weighted_union
            (List.map
               Kyb_status.Transition.
                 [ 4., Submit
                 ; 2., Start_review
                 ; 3., Approve
                 ; 1., Request_information [ Source_of_funds ]
                 ; 0.3, Reject "outside_risk_appetite"
                 ; 0.3, Block "review"
                 ; 1., Unblock
                 ; 0.05, Close "ended"
                 ]
               ~f:(fun (w, t) -> w, G.return t))
        and round =
          G.weighted_union
            [ 4., G.return None; 1., G.map (Int.gen_incl 0 3) ~f:Option.some ]
        in
        Kyb_event.Kind.Status_changed { transition; round } )
    ; ( 2.
      , let%map id = party
        and status =
          G.of_list
            Status.
              [ Approved
              ; Approved
              ; Verifying
              ; Blocked { reason = "match"; previous = Approved }
              ; Rejected "no"
              ]
        in
        Kyb_event.Kind.Party_case_changed { party = Customer_id.of_string id; status } )
    ; ( 2.
      , let%map n = Int.gen_incl 0 (List.length docs - 1) in
        (List.nth_exn docs n 0).kind )
    ; ( 0.3
      , let%map n = Int.gen_incl 0 (List.length docs - 1) in
        Kyb_event.Kind.Document_removed
          (Document_id.of_string
             (match (List.nth_exn docs n 0).kind with
              | Document_added d -> Document_id.to_string d.id
              | _ -> "none")) )
    ; 0.5, G.return (Kyb_event.Kind.Entity_updated Kyb_fixtures.entity)
    ; ( 0.3
      , let%map industry = G.of_list [ "62.01"; "64.19"; "47.11" ] in
        Kyb_event.Kind.Entity_updated
          { Kyb_fixtures.entity with industry = Some industry } )
    ; ( 0.5
      , let%map n = Int.gen_incl 1 3 in
        Kyb_event.Kind.Parties_set (List.take Kyb_fixtures.parties n) )
    ; 0.5, G.return (Kyb_event.Kind.Source_of_funds_declared Kyb_fixtures.source_of_funds)
    ; ( 1.
      , let%map inputs =
          G.weighted_union
            [ 3., G.return Kyb_risk.Inputs.baseline
            ; 1., Kyb_risk.Inputs.quickcheck_generator
            ]
        in
        Kyb_event.Kind.Risk_assessed
          (Kyb_risk.assess Kyb_risk.Model.default inputs ~at:t0) )
    ; 0.3, G.return Kyb_event.Kind.Senior_approval
    ; 0.3, G.return Kyb_event.Kind.Periodic_review_completed
    ; 0.05, G.return (Kyb_event.Kind.Opened Kyb_fixtures.entity)
    ]
;;

let event_gen : Kyb_event.t G.t =
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
  { Kyb_event.id = Event_id.of_string (sprintf "kyb-%d" n)
  ; entity_id = Kyb_fixtures.entity_id
  ; at = Time_ns.add t0 (Time_ns.Span.of_int_sec secs)
  ; actor
  ; note
  ; kind
  }
;;

let opened =
  { Kyb_event.id = Event_id.of_string "open"
  ; entity_id = Kyb_fixtures.entity_id
  ; at = t0
  ; actor = System
  ; note = None
  ; kind = Opened Kyb_fixtures.entity
  }
;;

let apply_lenient case e =
  Kyb_case.apply case e |> Result.ok |> Option.value ~default:case
;;

let replay events =
  List.fold
    (opened :: events)
    ~init:(Kyb_case.create Kyb_fixtures.entity_id)
    ~f:apply_lenient
;;

let%test_unit "applying the same event twice equals applying it once" =
  Quickcheck.test
    (G.both (List.gen_with_length 60 event_gen) event_gen)
    ~sexp_of:[%sexp_of: Kyb_event.t list * Kyb_event.t]
    ~f:(fun (history, e) ->
      let case = replay history in
      let once = apply_lenient case e in
      [%test_result: Kyb_case.t] (apply_lenient once e) ~expect:once;
      match Kyb_case.apply case e with
      | Error _ -> ()
      | Ok once -> assert (Result.is_ok (Kyb_case.apply once e)))
;;

let%test_unit "redelivering every accepted event changes nothing" =
  Quickcheck.test
    (List.gen_with_length 60 event_gen)
    ~sexp_of:[%sexp_of: Kyb_event.t list]
    ~f:(fun events ->
      let case, accepted =
        List.fold
          (opened :: events)
          ~init:(Kyb_case.create Kyb_fixtures.entity_id, [])
          ~f:(fun (case, accepted) e ->
            match Kyb_case.apply case e with
            | Ok case -> case, e :: accepted
            | Error _ -> case, accepted)
      in
      [%test_result: Kyb_case.t]
        (List.fold_result (List.rev accepted) ~init:case ~f:Kyb_case.apply
         |> Or_error.ok_exn)
        ~expect:case;
      (* and the rebuilt case from accepted events alone is the same *)
      [%test_result: Kyb_case.t]
        (Kyb_case.of_events Kyb_fixtures.entity_id (List.rev accepted) |> Or_error.ok_exn)
        ~expect:case)
;;

let%test_unit "rounds never go down and the applicant view never shows a block" =
  Quickcheck.test
    (List.gen_with_length 60 event_gen)
    ~sexp_of:[%sexp_of: Kyb_event.t list]
    ~f:(fun events ->
      ignore
        (List.fold
           (opened :: events)
           ~init:(Kyb_case.create Kyb_fixtures.entity_id)
           ~f:(fun case e ->
             let next = apply_lenient case e in
             assert (Kyb_case.round next >= Kyb_case.round case);
             (match Kyb_case.status next with
              | Blocked _ ->
                let v = Kyb_case.applicant_view next in
                assert (
                  Kyc_case.Customer_view.equal_status v.status Under_review
                  && List.is_empty v.requested
                  && List.is_empty v.parties_pending)
              | _ -> ());
             next)
         : Kyb_case.t))
;;

let%expect_test "random streams pass through every status" =
  let seen = Hash_set.create (module String) in
  let name : Kyb_status.t -> string = function
    | Draft -> "Draft"
    | Submitted -> "Submitted"
    | In_review -> "In_review"
    | More_information _ -> "More_information"
    | Approved -> "Approved"
    | Rejected _ -> "Rejected"
    | Blocked _ -> "Blocked"
    | Closed _ -> "Closed"
  in
  Quickcheck.iter ~trials:2_000 (List.gen_with_length 60 event_gen) ~f:(fun events ->
    List.iter
      (Kyb_audit.entries (Kyb_case.audit (replay events)))
      ~f:(fun entry ->
        match entry.change with
        | Status { to_; _ } -> Hash_set.add seen (name to_)
        | _ -> ()));
  print_s
    [%sexp (Hash_set.to_list seen |> List.sort ~compare:String.compare : string list)];
  [%expect
    {| (Approved Blocked Closed Draft In_review More_information Rejected Submitted) |}]
;;
