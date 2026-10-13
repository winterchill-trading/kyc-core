open! Core
open Kyc_core
open Risk

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let op = Operator_id.of_string "op-1"
let base = Inputs.baseline

let foreign_pep ?ended () =
  Or_error.ok_exn
    (Pep.create
       ~kind:Pep
       ~scope:Foreign
       ~function_:"minister"
       ~started:(Date.of_string "2015-01-01")
       ?ended
       ())
;;

let show (i : Inputs.t) =
  let a = assess Model.default i ~at in
  print_s
    [%message
      ""
        ~score:(a.score : int)
        ~band:(Assessment.band a : Band.t)
        ~floors:(a.floors : Edd_trigger.t list)
        ~edd:(edd_triggers i ~on:(Date.of_string "2026-10-13") : Edd_trigger.t list)]
;;

let%expect_test "bands, floors and EDD triggers" =
  show base;
  show
    { base with
      channel = Remote_unattended
    ; products = [ Crypto ]
    ; monthly_volume = Up_to_10k
    };
  show { base with sector = High; products = [ Lightning ]; monthly_volume = Above_100k };
  show { base with pep = Some (foreign_pep ()) };
  show { base with pep = Some (foreign_pep ~ended:(Date.of_string "2025-01-31") ()) };
  show { base with nationality = High_risk_third_country };
  show { base with adverse_media = Unconfirmed };
  show { base with adverse_media = Confirmed };
  show
    { base with self_hosted_exposure = true; manual_edd = [ "unusual source of wealth" ] };
  [%expect
    {|
    ((score 0) (band Low) (floors ()) (edd ()))
    ((score 35) (band Medium) (floors ()) (edd ()))
    ((score 75) (band High) (floors ()) (edd ()))
    ((score 40) (band High) (floors (Pep)) (edd (Pep)))
    ((score 0) (band Low) (floors ()) (edd ()))
    ((score 40) (band High) (floors (High_risk_third_country))
     (edd (High_risk_third_country)))
    ((score 15) (band Low) (floors ()) (edd ()))
    ((score 40) (band High) (floors (Adverse_media)) (edd (Adverse_media)))
    ((score 15) (band Low) (floors ())
     (edd (Self_hosted_exposure (Manual "unusual source of wealth"))))
    |}]
;;

let%expect_test "points per factor" =
  print_s
    [%sexp
      (Model.points
         Model.default
         { base with channel = Remote_unattended; products = [ Card; Crypto ] }
         ~on:(Date.of_string "2026-10-13")
       : (string * int) list)];
  [%expect
    {|
    ((sector 0) (country 0) (adverse_media 0) (pep 0) (product 20) (volume 0)
     (channel 10) (self_hosted 0))
    |}]
;;

let%expect_test "overrides need a reason and respect floors" =
  let a = assess Model.default base ~at in
  let show r = print_s [%sexp (Result.map r ~f:Assessment.band : Band.t Or_error.t)] in
  show (override a ~band:High ~reason:"linked to a high-risk business" ~by:op ~at);
  show (override a ~band:Medium ~reason:" " ~by:op ~at);
  let pep = assess Model.default { base with pep = Some (foreign_pep ()) } ~at in
  show (override pep ~band:Low ~reason:"long retired" ~by:op ~at);
  [%expect
    {|
    (Ok High)
    (Error "a risk override needs a reason")
    (Error ("a hard floor keeps the band High" (floors (Pep)) (requested Low)))
    |}]
;;

let%expect_test "validate catches a tampered assessment" =
  let a = assess Model.default { base with nationality = High_risk_third_country } ~at in
  print_s [%sexp (Assessment.validate a : unit Or_error.t)];
  print_s [%sexp (Assessment.validate { a with computed_band = Low } : unit Or_error.t)];
  print_s [%sexp (Assessment.validate { a with score = 140 } : unit Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Error
     ("computed band does not match score and floors" (score 40)
      (floors (High_risk_third_country)) (computed_band Low)))
    (Error ("risk score out of range" (score 140)))
    |}]
;;

let%test_unit "high is reachable and floors always give High" =
  Quickcheck.test Inputs.quickcheck_generator ~sexp_of:[%sexp_of: Inputs.t] ~f:(fun i ->
    let a = assess Model.default i ~at in
    assert (a.score >= 0 && a.score <= 100);
    assert (Result.is_ok (Assessment.validate a));
    if not (List.is_empty a.floors) then assert (Band.equal (Assessment.band a) High))
;;

let%test_unit "the score never falls when a factor gets riskier" =
  Quickcheck.test
    [%quickcheck.generator: Inputs.t * Band.t]
    ~sexp_of:[%sexp_of: Inputs.t * Band.t]
    ~f:(fun (i, sector) ->
      let riskier = { i with sector = Band.max i.sector sector } in
      let on = Date.of_string "2026-10-13" in
      assert (Model.score Model.default riskier ~on >= Model.score Model.default i ~on))
;;

let%expect_test "cdd level" =
  let show band triggers products =
    print_s
      [%sexp
        (Cdd_level.decide ~band ~triggers ~products ~allow_simplified:true : Cdd_level.t)]
  in
  show Low [] [ Fiat_account ];
  show Low [] [ Fiat_account; Lightning ];
  show Medium [] [ Card ];
  show Low [ Self_hosted_exposure ] [ Crypto ];
  show High [] [ Fiat_account ];
  [%expect {|
    Simplified
    Standard
    Standard
    Enhanced
    Enhanced
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Inputs.Stable.V1.t];
  print_endline [%bin_digest: Assessment.Stable.V1.t];
  print_endline [%bin_digest: Edd_trigger.Stable.V1.t];
  print_endline [%bin_digest: Pep.Stable.V1.t];
  [%expect
    {|
    3627b1902ffa43e44d6a5720c77fa137
    9bacf929d84e5817e7d1441a979f87e3
    6e586801db82ae95aff9fa25dce158dc
    77fbcf5dae3e9894ed9b3cfe3db3e0be
    |}]
;;
