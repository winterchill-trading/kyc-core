open! Core
open Kyc_core
module S = Kyb_status

let op = Actor.Operator (Operator_id.of_string "op-a")

let statuses : S.t list =
  [ Draft
  ; Submitted
  ; In_review
  ; More_information [ Source_of_funds ]
  ; Approved
  ; Rejected "outside_risk_appetite"
  ; Blocked { reason = "review"; previous = Approved }
  ; Closed "ended"
  ]
;;

let transitions : S.Transition.t list =
  [ Submit
  ; Start_review
  ; Request_information [ Entity_field Registration_number ]
  ; Approve
  ; Reject "outside_risk_appetite"
  ; Block "review"
  ; Unblock
  ; Close "ended"
  ]
;;

let short (t : S.t) =
  match t with
  | Draft -> "Draft"
  | Submitted -> "Submitted"
  | In_review -> "In_review"
  | More_information _ -> "More_information"
  | Approved -> "Approved"
  | Rejected _ -> "Rejected"
  | Blocked _ -> "Blocked"
  | Closed _ -> "Closed"
;;

let%expect_test "every transition from every status, as an operator" =
  List.iter transitions ~f:(fun tr ->
    let name =
      Sexp.to_string (S.Transition.sexp_of_t tr)
      |> String.split_on_chars ~on:[ ' '; '(' ]
      |> List.find_exn ~f:(Fn.non String.is_empty)
    in
    let row =
      List.filter_map statuses ~f:(fun from ->
        match S.apply from tr ~actor:op with
        | Ok to_ -> Some (sprintf "%s->%s" (short from) (short to_))
        | Error _ -> None)
    in
    printf "%-20s %s\n" name (String.concat ~sep:" " row));
  [%expect
    {|
    Submit               Draft->Submitted More_information->Submitted
    Start_review         Submitted->In_review
    Request_information  Submitted->More_information In_review->More_information Approved->More_information
    Approve              Submitted->Approved In_review->Approved
    Reject               Submitted->Rejected In_review->Rejected More_information->Rejected
    Block                Draft->Blocked Submitted->Blocked In_review->Blocked More_information->Blocked Approved->Blocked Rejected->Blocked
    Unblock              Blocked->Approved
    Close                Draft->Closed Submitted->Closed In_review->Closed More_information->Closed Approved->Closed Rejected->Closed Blocked->Closed
    |}]
;;

let%expect_test "guards" =
  let show r = print_s [%sexp (r : S.t Or_error.t)] in
  show (S.apply Submitted (Block "x") ~actor:System);
  show
    (S.apply (Blocked { reason = "x"; previous = Draft }) Unblock ~actor:(Provider "p"));
  show (S.apply Submitted (Request_information []) ~actor:op);
  show (S.apply Submitted (Reject " ") ~actor:op);
  show (S.apply (Blocked { reason = "x"; previous = In_review }) Unblock ~actor:op);
  [%expect
    {|
    (Error "only an operator may block")
    (Error "only an operator may unblock")
    (Error "nothing requested")
    (Error ("a reason is required" (what reject)))
    (Ok In_review)
    |}]
;;

let%test_unit "Closed is final and Unblock restores the status before Block" =
  Quickcheck.test
    (Quickcheck.Generator.both S.quickcheck_generator S.Transition.quickcheck_generator)
    ~sexp_of:[%sexp_of: S.t * S.Transition.t]
    ~f:(fun (from, tr) ->
      match from, S.apply from tr ~actor:op with
      | Closed _, result -> assert (Result.is_error result)
      | _, Ok (Blocked { previous; _ } as blocked) ->
        [%test_result: S.t Or_error.t]
          (S.apply blocked Unblock ~actor:op)
          ~expect:(Ok previous)
      | _ -> ())
;;
