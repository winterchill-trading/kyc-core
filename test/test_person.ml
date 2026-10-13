open! Core
open Kyc_core

let person =
  { Person.given_name = Pii.create "Ada"
  ; family_name = Pii.create "Example"
  ; date_of_birth = Pii.create (Date.of_string "1990-06-15")
  ; nationality = Pii.create (Country.of_string "DE")
  ; residential_address =
      Pii.create
        { Address.line1 = "1 Example Street"
        ; line2 = None
        ; city = "Berlin"
        ; region = None
        ; postal_code = Some "10115"
        ; country = Country.of_string "DE"
        }
  ; email = Pii.create "ada@example.com"
  ; mobile = Pii.create "+4930000000"
  }
;;

let%expect_test "a person prints with every field redacted" =
  print_s [%sexp (person : Person.t)];
  [%expect
    {|
    ((given_name <redacted>) (family_name <redacted>) (date_of_birth <redacted>)
     (nationality <redacted>) (residential_address <redacted>) (email <redacted>)
     (mobile <redacted>))
    |}]
;;

let%expect_test "changed_fields" =
  let moved =
    { person with
      residential_address =
        Pii.map person.residential_address ~f:(fun a -> { a with city = "Hamburg" })
    ; mobile = Pii.create "+4940000000"
    }
  in
  print_s [%sexp (Person.changed_fields person moved : Person.Field.t list)];
  print_s [%sexp (Person.changed_fields person person : Person.Field.t list)];
  [%expect {|
    (Residential_address Mobile)
    ()
    |}]
;;

let%expect_test "age counts whole years" =
  List.iter [ "2008-06-14"; "2008-06-15"; "2026-10-13" ] ~f:(fun d ->
    printf "%s %d\n" d (Person.age person ~on:(Date.of_string d)));
  [%expect {|
    2008-06-14 17
    2008-06-15 18
    2026-10-13 36
    |}]
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Person.Stable.V1.t];
  print_endline [%bin_digest: Address.Stable.V1.t];
  [%expect
    {|
    4891aab3372a69cca428d7af936d8b74
    d308469bade96e9cef1aa14c3cef37ac
    |}]
;;

let%test_unit "bin_io round trip" =
  Quickcheck.test Person.quickcheck_generator ~sexp_of:[%sexp_of: Person.t] ~f:(fun p ->
    [%test_result: Person.t]
      (Binable.of_string
         (module Person.Stable.V1)
         (Binable.to_string (module Person.Stable.V1) p))
      ~expect:p)
;;
