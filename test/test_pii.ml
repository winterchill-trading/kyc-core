open! Core
open Kyc_core

let%expect_test "sexp and to_string redact" =
  let email = Pii.create "someone@example.com" in
  print_s [%sexp (email : string Pii.t)];
  print_endline (Pii.to_string email);
  print_s [%message "lookup failed" (email : string Pii.t)];
  [%expect
    {|
    <redacted>
    <redacted>
    ("lookup failed" (email <redacted>))
    |}]
;;

let%expect_test "reveal and t_of_sexp give the clear value" =
  let email = [%of_sexp: string Pii.t] (Sexp.Atom "a@example.com") in
  print_endline (Pii.reveal email);
  print_endline (Pii.reveal (Pii.map email ~f:String.uppercase));
  [%expect {|
    a@example.com
    A@EXAMPLE.COM
    |}]
;;

let%expect_test "bin_io carries the clear value" =
  let module V = struct
    type t = string Pii.Stable.V1.t [@@deriving bin_io]
  end
  in
  let s = Binable.to_string (module V) (Pii.create "secret") in
  print_endline (Pii.reveal (Binable.of_string (module V) s));
  (* Same digest as plain [string]: the wrapper adds nothing on the wire. *)
  print_endline [%bin_digest: string Pii.Stable.V1.t];
  [%expect {|
    secret
    d9a8da25d5656b016fb4dbdc2e4197fb
    |}]
;;

let%test_unit "bin_io round trip" =
  let module V = struct
    type t = string Pii.Stable.V1.t [@@deriving bin_io]
  end
  in
  Quickcheck.test [%quickcheck.generator: string Pii.t] ~f:(fun p ->
    [%test_result: string]
      (Pii.reveal (Binable.of_string (module V) (Binable.to_string (module V) p)))
      ~expect:(Pii.reveal p))
;;
