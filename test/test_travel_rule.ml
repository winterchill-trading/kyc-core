open! Core
open Kyc_core

let originator : Travel_rule.Originator.t =
  { name = Pii.create "Ada Example"
  ; dlt_address = Some "bc1q-example"
  ; account_number = None
  ; address = None
  ; official_document_number = None
  ; customer_number = Some "cus-1"
  ; date_and_place_of_birth = Some (Pii.create (Date.of_string "1990-06-15", "Berlin"))
  ; lei = None
  }
;;

let transfer ?(amount = 50_000) counterparty : Travel_rule.t =
  { transfer_ref = "tx-1"
  ; outgoing = true
  ; amount_eur_minor = amount
  ; originator
  ; beneficiary =
      { name = Pii.create "Bob Example"
      ; dlt_address = Some "bc1q-other"
      ; account_number = None
      ; lei = None
      }
  ; counterparty
  }
;;

let show t = print_s [%sexp (Travel_rule.decide t : Travel_rule.Action.t)]

let%expect_test "decide" =
  show (transfer (Provider "casp-b"));
  show (transfer ~amount:100_000 (Self_hosted { ownership_verified = false }));
  show (transfer ~amount:100_001 (Self_hosted { ownership_verified = false }));
  show (transfer ~amount:500_000 (Self_hosted { ownership_verified = true }));
  let t = transfer (Provider "casp-b") in
  show
    { t with
      originator = { originator with date_and_place_of_birth = None; dlt_address = None }
    ; beneficiary = { t.beneficiary with name = Pii.create " " }
    };
  [%expect
    {|
    Proceed
    Proceed
    Await_ownership_proof
    Proceed
    (Missing_data
     (originator.dlt_address_or_account
      originator.address_document_and_customer_number_or_birth_date_and_place
      beneficiary.name))
    |}]
;;

let%expect_test "PII stays redacted" =
  print_s [%sexp (transfer (Provider "casp-b") : Travel_rule.t)];
  [%expect
    {|
    ((transfer_ref tx-1) (outgoing true) (amount_eur_minor 50000)
     (originator
      ((name <redacted>) (dlt_address (bc1q-example)) (account_number ())
       (address ()) (official_document_number ()) (customer_number (cus-1))
       (date_and_place_of_birth (<redacted>)) (lei ())))
     (beneficiary
      ((name <redacted>) (dlt_address (bc1q-other)) (account_number ()) (lei ())))
     (counterparty (Provider casp-b)))
    |}]
;;
