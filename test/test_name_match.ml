open! Core
open Kyc_core

let%expect_test "normalize" =
  List.iter
    [ "Dr. José Ñúñez-García"
    ; "MÜLLER, Jürgen"
    ; "Æsir Trading GmbH"
    ; "Łukasz Żółć"
    ; "Straße"
    ; "Ivan Петров"
    ; "e\xcc\x81lise" (* e + combining acute *)
    ]
    ~f:(fun n -> print_endline (Name_match.normalize n));
  [%expect
    {|
    jose nunez garcia
    muller jurgen
    aesir trading
    lukasz zolc
    strasse
    ivan Петров
    elise
    |}]
;;

(* Reference values from Winkler's paper and the usual test pairs. *)
let%expect_test "jaro and jaro-winkler" =
  List.iter
    [ "martha", "marhta"; "dwayne", "duane"; "dixon", "dicksonx"; "abc", "xyz"; "", "" ]
    ~f:(fun (a, b) ->
      printf "%s %s %.3f %.3f\n" a b (Name_match.jaro a b) (Name_match.jaro_winkler a b));
  [%expect
    {|
    martha marhta 0.944 0.961
    dwayne duane 0.822 0.840
    dixon dicksonx 0.767 0.813
    abc xyz 0.000 0.000
      1.000 1.000
    |}]
;;

let%expect_test "score" =
  List.iter
    [ "Ada Example", "EXAMPLE, Ada"
    ; "Ada Example", "Ada Exampel"
    ; "Jürgen Müller", "Juergen Mueller"
    ; "Jürgen Müller", "Jurgen Muller"
    ; "Ada Example", "Bob Sample"
    ]
    ~f:(fun (a, b) ->
      printf
        "%-15s %-16s %.3f %b\n"
        a
        b
        (Name_match.score a b)
        (Name_match.is_candidate a b));
  [%expect
    {|
    Ada Example     EXAMPLE, Ada     1.000 true
    Ada Example     Ada Exampel      0.982 true
    Jürgen Müller Juergen Mueller  0.872 true
    Jürgen Müller Jurgen Muller    1.000 true
    Ada Example     Bob Sample       0.660 false
    |}]
;;

let%test_unit "score is symmetric and within 0..1" =
  Quickcheck.test
    [%quickcheck.generator: string * string]
    ~sexp_of:[%sexp_of: string * string]
    ~f:(fun (a, b) ->
      let s = Name_match.score a b in
      assert (Float.( >= ) s 0. && Float.( <= ) s 1.);
      assert (Float.( < ) (Float.abs (s -. Name_match.score b a)) 1e-9))
;;

let%test_unit "a name matches itself" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:[%sexp_of: string] ~f:(fun a ->
    if not (List.is_empty (Name_match.tokens a))
    then assert (Float.equal (Name_match.score a a) 1.))
;;
