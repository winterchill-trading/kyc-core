open! Core
open Kyc_core

let d = Date.of_string

let create ?ended ?edd_until () =
  Pep.create
    ~kind:Pep
    ~scope:Foreign
    ~function_:"minister"
    ~started:(d "2018-01-01")
    ?ended
    ?edd_until
    ()
;;

let%expect_test "edd_until defaults to 12 months after the end and cannot be shorter" =
  print_s [%sexp (create ~ended:(d "2025-03-31") () : Pep.t Or_error.t)];
  print_s
    [%sexp
      (create ~ended:(d "2025-03-31") ~edd_until:(d "2025-12-31") () : Pep.t Or_error.t)];
  print_s [%sexp (create ~edd_until:(d "2026-12-31") () : Pep.t Or_error.t)];
  print_s [%sexp (create ~ended:(d "2017-01-01") () : Pep.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((kind Pep) (scope Foreign) (function_ minister) (started 2018-01-01)
      (ended (2025-03-31)) (edd_until (2026-03-31))))
    (Error
     ("edd_until is earlier than 12 months after the end" (u 2025-12-31)
      (minimum 2026-03-31)))
    (Error "edd_until is only set once the function has ended")
    (Error
     ("PEP function ends before it starts" (started 2018-01-01) (e 2017-01-01)))
    |}]
;;

let%expect_test "extend, never shorten" =
  let pep = Or_error.ok_exn (create ~ended:(d "2025-03-31") ()) in
  print_s [%sexp (Pep.extend_edd pep ~until:(d "2027-01-01") : Pep.t Or_error.t)];
  print_s [%sexp (Pep.extend_edd pep ~until:(d "2026-01-01") : Pep.t Or_error.t)];
  print_s
    [%sexp
      (Pep.extend_edd (Or_error.ok_exn (create ())) ~until:(d "2030-01-01")
       : Pep.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((kind Pep) (scope Foreign) (function_ minister) (started 2018-01-01)
      (ended (2025-03-31)) (edd_until (2027-01-01))))
    (Error
     ("EDD can be extended, not shortened" (current 2026-03-31)
      (until 2026-01-01)))
    (Error "the function is still held; nothing to extend")
    |}]
;;

let%expect_test "edd_applies" =
  let pep = Or_error.ok_exn (create ~ended:(d "2025-03-31") ()) in
  List.iter [ "2024-01-01"; "2026-03-31"; "2026-04-01" ] ~f:(fun on ->
    printf "%s %b\n" on (Pep.edd_applies pep ~on:(d on)));
  [%expect {|
    2024-01-01 true
    2026-03-31 true
    2026-04-01 false
    |}]
;;

let%expect_test "t_of_sexp checks the invariant" =
  let sexp =
    Sexp.of_string
      "((kind Pep) (scope Domestic) (function_ mayor) (started 2020-01-01) (ended \
       (2024-01-01)) (edd_until (2024-06-01)))"
  in
  print_s [%sexp (Or_error.try_with (fun () -> Pep.t_of_sexp sexp) : Pep.t Or_error.t)];
  [%expect
    {|
    (Error
     (Of_sexp_error
       "(\"edd_until is earlier than 12 months after the end\" (u 2024-06-01)\
      \n (minimum 2025-01-01))"
      (invalid_sexp
       ((kind Pep) (scope Domestic) (function_ mayor) (started 2020-01-01)
        (ended (2024-01-01)) (edd_until (2024-06-01))))))
    |}]
;;

let%test_unit "generated records keep the 12-month tail" =
  Quickcheck.test Pep.quickcheck_generator ~sexp_of:[%sexp_of: Pep.t] ~f:(fun pep ->
    match pep.ended, pep.edd_until with
    | Some e, Some u -> assert (Date.( >= ) u (Date.add_months e 12))
    | None, None -> ()
    | _ -> assert false)
;;
