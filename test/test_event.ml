open! Core
open Kyc_core

let%expect_test "an event prints without PII" =
  let e : Event.t =
    { id = Event_id.of_string "evt-1"
    ; customer_id = Customer_id.of_string "cus-1"
    ; at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
    ; actor = Provider "provider-a"
    ; note = None
    ; kind = Opened Test_person.person
    }
  in
  print_s [%sexp (e : Event.t)];
  [%expect
    {|
    ((id evt-1) (customer_id cus-1) (at "2026-10-13 09:00:00Z")
     (actor (Provider provider-a)) (note ())
     (kind
      (Opened
       ((given_name <redacted>) (family_name <redacted>)
        (date_of_birth <redacted>) (nationality <redacted>)
        (residential_address <redacted>) (email <redacted>) (mobile <redacted>)))))
    |}]
;;

let%test_unit "bin_io round trip keeps PII" =
  Quickcheck.test Event.quickcheck_generator ~sexp_of:[%sexp_of: Event.t] ~f:(fun e ->
    [%test_result: Event.t]
      (Binable.of_string
         (module Event.Stable.V1)
         (Binable.to_string (module Event.Stable.V1) e))
      ~expect:e)
;;

(* A change here means old event logs no longer read: add a V2 instead. *)
let%expect_test "stable digests" =
  print_endline [%bin_digest: Event.Stable.V1.t];
  print_endline [%bin_digest: Audit.Entry.Stable.V1.t];
  print_endline [%bin_digest: Actor.Stable.V1.t];
  print_endline [%bin_digest: Customer_id.Stable.V1.t];
  print_endline [%bin_digest: Event_id.Stable.V1.t];
  [%expect
    {|
    ccf697536e0d1e9d38a7e293ebfc6c40
    fbce6c807a266a6a0dc45a0f9e8eb4a8
    f34dd673104d7fb8f766f5d7534e5088
    ccd424464ba7e6c22cebb7c939d997b2
    e01ddfc6032e4d2c1e9fedae4a0467a0
    |}]
;;
