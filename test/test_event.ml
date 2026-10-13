open! Core
open Kyc_core

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

let event kind : Event.t =
  { id = Event_id.of_string "evt-1"
  ; customer_id = Customer_id.of_string "cus-1"
  ; at
  ; actor = Provider "provider-a"
  ; note = None
  ; kind
  }
;;

let%expect_test "an event prints without PII" =
  print_s [%sexp (event (Opened Test_person.person) : Event.t)];
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

let%test_unit "bin_io round trip keeps PII and assessments" =
  let kinds : Event.Kind.t list =
    [ Opened Test_person.person
    ; Risk_assessed (Risk.assess Risk.Model.default Risk.Inputs.baseline ~at)
    ; Screened
        { screening_id = Screening_id.of_string "scr-1"
        ; reason = Payment
        ; hits =
            [ { source = Ofac_sdn
              ; list_version = "v"
              ; entry_id = "e"
              ; matched_name = "n"
              ; score = 88
              ; algorithm = "jw"
              }
            ]
        }
    ; Legal_hold_set { until = at; reason = "court order"; authority = "court" }
    ]
  in
  List.iter kinds ~f:(fun kind ->
    let e = event kind in
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
  print_endline [%bin_digest: Travel_rule.Stable.V1.t];
  print_endline [%bin_digest: Retention.Legal_hold.Stable.V1.t];
  [%expect
    {|
    4d45fc47cd619afd84093d67f741dc0f
    a117f7b9b3a9c5a9803dbc2cc7476c0e
    f34dd673104d7fb8f766f5d7534e5088
    ccd424464ba7e6c22cebb7c939d997b2
    e01ddfc6032e4d2c1e9fedae4a0467a0
    970cd1f0d339867a9ba7275a9b0f6ed0
    16116afaffffea3dd1748cecbd3ebabd
    |}]
;;
