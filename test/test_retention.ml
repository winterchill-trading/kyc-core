open! Core
open Kyc_core

let ts = Time_ns.of_string_with_utc_offset
let five_years = Time_ns.Span.of_day (5. *. 365.)

let%expect_test "retain_until" =
  let show ?ended ?refused ?hold () =
    print_s
      [%sexp
        (Retention.retain_until
           ~ended:(Option.map ended ~f:ts)
           ~refused:(Option.map refused ~f:ts)
           ~retention:five_years
           ~hold
         : Timestamp.t option)]
  in
  show ();
  show ~ended:"2026-01-01 00:00:00Z" ();
  show ~ended:"2026-01-01 00:00:00Z" ~refused:"2024-01-01 00:00:00Z" ();
  show
    ~refused:"2024-01-01 00:00:00Z"
    ~hold:{ until = ts "2031-06-01 00:00:00Z"; reason = "x"; authority = "y" }
    ();
  [%expect
    {|
    ()
    ("2030-12-31 00:00:00Z")
    ("2030-12-31 00:00:00Z")
    ("2031-06-01 00:00:00Z")
    |}]
;;

let%expect_test "investigation four eyes and FIU deadlines" =
  let at = ts "2026-10-13 09:00:00Z" in
  let a = Operator_id.of_string "op-a"
  and b = Operator_id.of_string "op-b" in
  let i =
    Investigation.open_
      ~id:(Investigation.Investigation_id.of_string "inv-1")
      ~at
      ~alerts:[]
  in
  let step r f =
    match Or_error.bind r ~f with
    | Ok (i : Investigation.t) ->
      print_s
        [%sexp
          (i.state : Investigation.Case_state.t)
          , (i.report : (Investigation.Report_state.t * Operator_id.t) option)];
      Ok i
    | Error e ->
      print_s [%sexp (e : Error.t)];
      r
  in
  let r = Ok i in
  let r = step r (fun i -> Investigation.move i Pending_decision) in
  let r = step r (fun i -> Investigation.move i Investigating) in
  let r = step r (fun i -> Investigation.report i Draft ~by:a) in
  let r = step r (fun i -> Investigation.move i Pending_decision) in
  let r = step r (fun i -> Investigation.move i Closed_no_report) in
  let r = step r (fun i -> Investigation.report i Approved ~by:b) in
  let r = step r (fun i -> Investigation.report i Filed ~by:b) in
  let r =
    step r (fun i ->
      Investigation.report i (Rejected_by_fiu "missing account data") ~by:b)
  in
  let r = step r (fun i -> Investigation.report i Draft ~by:b) in
  ignore (r : Investigation.t Or_error.t);
  List.iter [ "2026-10-12"; "2026-10-16"; "2026-10-17" ] ~f:(fun d ->
    let req =
      Investigation.Fiu_request.create
        ~id:(Investigation.Request_id.of_string "fiu-1")
        ~received_on:(Date.of_string d)
        ()
    in
    printf
      "%s (%s) -> due %s\n"
      d
      (Day_of_week.to_string (Date.day_of_week (Date.of_string d)))
      (Date.to_string req.due));
  [%expect
    {|
    ("illegal investigation move" (from Open) (to_ Pending_decision))
    (Investigating ())
    (Investigating ((Draft op-a)))
    (Pending_decision ((Draft op-a)))
    "a report exists; close as reported"
    (Pending_decision ((Approved op-a)))
    (Pending_decision ((Filed op-a)))
    (Pending_decision (((Rejected_by_fiu "missing account data") op-a)))
    (Pending_decision ((Draft op-b)))
    2026-10-12 (MON) -> due 2026-10-19
    2026-10-16 (FRI) -> due 2026-10-23
    2026-10-17 (SAT) -> due 2026-10-26
    |}]
;;

let%expect_test "visibility" =
  List.iter Visibility.Audience.all ~f:(fun audience ->
    print_s
      [%message
        ""
          (audience : Visibility.Audience.t)
          ~sees:
            (List.filter Visibility.Class.all ~f:(Visibility.can_see audience)
             : Visibility.Class.t list)]);
  [%expect
    {|
    ((audience Customer) (sees (Public Pii Sensitive_pii)))
    ((audience Support) (sees (Public Internal Pii)))
    ((audience Compliance) (sees (Public Internal Pii Sensitive_pii Restricted)))
    |}]
;;
