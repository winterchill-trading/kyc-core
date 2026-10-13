open! Core
open Kyc_core
open Risk

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let operator = Actor.Operator (Operator_id.of_string "op-1")

let base : Factors.t =
  { country = Low; product = Low; channel = Low; pep = Not_pep; adverse_media = false }
;;

let show (f : Factors.t) =
  let rating = rate f in
  print_s
    [%message "" (rating : Level.t) ~edd:(edd_triggers f ~rating : Edd_trigger.t list)]
;;

let%expect_test "rating and EDD triggers" =
  show base;
  show { base with channel = Medium };
  show { base with country = High };
  show { base with pep = Domestic };
  show { base with pep = Family_or_close_associate Foreign };
  show { base with pep = International_organisation; product = High };
  show { base with adverse_media = true };
  [%expect
    {|
    ((rating Low) (edd ()))
    ((rating Medium) (edd ()))
    ((rating High) (edd (High_risk_rating High_risk_country)))
    ((rating Medium) (edd (Pep_in_higher_risk_relationship)))
    ((rating High) (edd (High_risk_rating Foreign_pep)))
    ((rating High) (edd (High_risk_rating Pep_in_higher_risk_relationship)))
    ((rating Medium) (edd (Adverse_media)))
    |}]
;;

let%expect_test "overrides" =
  let assess ?(actor = operator) factors override =
    print_s
      [%sexp (Risk.assess ~assessed_at:at ~factors ~override ~actor : Risk.t Or_error.t)]
  in
  assess base (Some { rating = High; rationale = "linked to a high-risk business" });
  assess ~actor:System base (Some { rating = High; rationale = "x" });
  assess base (Some { rating = Medium; rationale = " " });
  assess { base with pep = Foreign } (Some { rating = Low; rationale = "long retired" });
  [%expect
    {|
    (Ok
     ((assessed_at "2026-10-13 09:00:00Z")
      (factors
       ((country Low) (product Low) (channel Low) (pep Not_pep)
        (adverse_media false)))
      (computed Low) (rating High)
      (override
       ((((rating High) (rationale "linked to a high-risk business")) op-1)))))
    (Error "only an operator may override a risk rating")
    (Error "a risk override needs a rationale")
    (Error
     ("a foreign PEP rating cannot be lowered" (computed High) (requested Low)))
    |}]
;;

let%test_unit "rating is at least every factor and never below High for foreign PEPs" =
  Quickcheck.test Factors.quickcheck_generator ~sexp_of:[%sexp_of: Factors.t] ~f:(fun f ->
    let r = rate f in
    assert (Level.(r >= f.country && r >= f.product && r >= f.channel));
    if Pep_exposure.is_foreign f.pep then assert (Level.equal r High);
    if Level.equal r High
    then
      assert (
        List.mem (edd_triggers f ~rating:r) High_risk_rating ~equal:Edd_trigger.equal))
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Factors.Stable.V1.t];
  print_endline [%bin_digest: Override.Stable.V1.t];
  print_endline [%bin_digest: Edd_trigger.Stable.V1.t];
  [%expect
    {|
    c002aa4e5d55b9d5d9b6b170d6ec0f4e
    985db5391359f3812142556b18dc9ee3
    d8fa55580e69170266532156a29d3273
    |}]
;;
