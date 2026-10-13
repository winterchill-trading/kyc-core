open! Core
open Kyc_core

let%expect_test "of_string checks the shape" =
  List.iter [ "DE"; "de"; "DEU"; ""; " D" ] ~f:(fun s ->
    print_s
      [%sexp (Or_error.try_with (fun () -> Country.of_string s) : Country.t Or_error.t)]);
  [%expect
    {|
    (Ok DE)
    (Error (Invalid_argument "(\"expected an ISO 3166-1 alpha-2 code\"(s de))"))
    (Error (Invalid_argument "(\"expected an ISO 3166-1 alpha-2 code\"(s DEU))"))
    (Error
     (Invalid_argument "'' is not a valid Kyc_core.Country because it is empty"))
    (Error
     (Invalid_argument
      "' D' is not a valid Kyc_core.Country because it has whitespace on the edge"))
    |}]
;;

let%test_unit "generator yields valid codes" =
  Quickcheck.test Country.quickcheck_generator ~f:(fun c ->
    ignore (Country.of_string (Country.to_string c) : Country.t))
;;
