open! Core
open Kyc_core
module P = Kyb_party

let party ?(chains = []) id roles : P.t = { id = Customer_id.of_string id; roles; chains }
let direct bps = [ Ownership.Chain.direct ~bps ]

let via bps : Ownership.Chain.t list =
  [ { stake_bps = 10_000
    ; via =
        [ { name = "Holdco"
          ; jurisdiction = Country.of_string "LU"
          ; legal_form = Private_limited_company
          ; registration_number = Some "B123"
          ; stake_bps = bps
          ; nominee = false
          ; bearer_shares = false
          }
        ]
    }
  ]
;;

let show form parties =
  P.beneficial_owners form parties ~threshold_bps:2500
  |> List.iter ~f:(fun ((p : P.t), basis) ->
    print_s [%message "" ~_:(p.id : Customer_id.t) (basis : P.Basis.t list)]);
  print_s
    [%message
      ""
        ~needs_approved_case:
          (P.needs_approved_case form parties ~threshold_bps:2500 : Customer_id.t list)]
;;

let%expect_test "owners at or above 25%, directly or through a holding" =
  show
    Private_limited_company
    [ party "alice" [ Shareholder; Director ] ~chains:(direct 2500)
    ; party "bob" [ Shareholder ] ~chains:(direct 2499)
    ; party "carol" [ Shareholder ] ~chains:(via 4000)
    ; party "dan" [ Authorised_representative ]
    ];
  [%expect
    {|
    (alice (basis ((Ownership 25%))))
    (carol (basis ((Ownership 40%))))
    (needs_approved_case (alice carol dan))
    |}]
;;

let%expect_test "shares through several chains add up" =
  show
    Private_limited_company
    [ party "alice" [ Shareholder ] ~chains:(direct 1500 @ via 1000) ];
  [%expect
    {|
    (alice (basis ((Ownership 25%))))
    (needs_approved_case (alice))
    |}]
;;

let%expect_test "control by other means counts whatever the share" =
  show
    Private_limited_company
    [ party
        "alice"
        [ Shareholder; Control_by_other_means "veto on budget" ]
        ~chains:(direct 100)
    ];
  [%expect
    {|
    (alice (basis ((Control "veto on budget"))))
    (needs_approved_case (alice))
    |}]
;;

let%expect_test "no owner identified: senior managing officials" =
  show
    Public_limited_company
    [ party "alice" [ Shareholder ] ~chains:(direct 500)
    ; party "erin" [ Senior_managing_official; Director ]
    ];
  [%expect
    {|
    (erin (basis (Senior_managing_official)))
    (needs_approved_case (erin))
    |}]
;;

let%expect_test "every party to a trust, whatever the share" =
  show
    Trust
    [ party "sam" [ Settlor ]
    ; party "tia" [ Trustee ]
    ; party "pat" [ Protector ]
    ; party "ben" [ Beneficiary; Shareholder ] ~chains:(direct 100)
    ];
  [%expect
    {|
    (sam (basis ((Trust_role Settlor))))
    (tia (basis ((Trust_role Trustee))))
    (pat (basis ((Trust_role Protector))))
    (ben (basis ((Trust_role Beneficiary))))
    (needs_approved_case (sam tia pat ben))
    |}]
;;

let%expect_test "trust roles in a company are not beneficial ownership" =
  show Private_limited_company [ party "ben" [ Beneficiary ] ];
  [%expect {| (needs_approved_case ()) |}]
;;

let%expect_test "validation" =
  let check parties = print_s [%sexp (P.validate_all parties : unit Or_error.t)] in
  check [ party "a" [] ];
  check [ party "a" [ Director; Director ] ];
  check [ party "a" [ Shareholder ] ];
  check [ party "a" [ Director ] ~chains:(direct 100) ];
  check [ party "a" [ Director ]; party "a" [ Trustee ] ];
  check
    [ party "a" [ Shareholder ] ~chains:(direct 6000)
    ; party "b" [ Shareholder ] ~chains:(via 5000)
    ];
  check (List.init 51 ~f:(fun i -> party (sprintf "p%d" i) [ Director ]));
  check
    [ party "a" [ Shareholder ] ~chains:(direct 6000)
    ; party "b" [ Shareholder ] ~chains:(via 4000)
    ];
  [%expect
    {|
    (Error ("invalid party" (id a) (errors ("a party needs a role"))))
    (Error ("invalid party" (id a) (errors ("repeated role"))))
    (Error
     ("invalid party" (id a) (errors ("a shareholder needs an ownership chain"))))
    (Error
     ("invalid party" (id a)
      (errors ("ownership chains without the shareholder role"))))
    (Error "a person is listed twice")
    (Error ("ownership adds up to more than 100%" (total 110%)))
    (Error ("too many parties" (max 50)))
    (Ok ())
    |}]
;;
