open! Core
open Kyc_core
module K = Kyb_risk

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let on = Date.of_string "2026-10-13"

let show (inputs : K.Inputs.t) =
  let a = K.assess K.Model.default inputs ~at in
  print_s
    [%message
      ""
        ~points:
          (List.filter (K.Model.points K.Model.default inputs ~on) ~f:(fun (_, p) ->
             p > 0)
           : (string * int) list)
        ~score:(a.score : int)
        ~floors:(a.floors : K.Edd_trigger.t list)
        ~band:(K.Assessment.band a : Risk.Band.t)]
;;

let%expect_test "baseline business" =
  show K.Inputs.baseline;
  [%expect {| ((points ((volume 5) (channel 5))) (score 10) (floors ()) (band Low)) |}]
;;

let%expect_test "entity factors" =
  show { K.Inputs.baseline with ownership = Complex; industry = Medium };
  show { K.Inputs.baseline with cash_intensive = true; places_of_business = Elevated };
  show { K.Inputs.baseline with ownership = Opaque };
  show { K.Inputs.baseline with owner_countries = High_risk_third_country };
  [%expect
    {|
    ((points ((sector 15) (volume 5) (channel 5) (ownership 15))) (score 40)
     (floors ()) (band Medium))
    ((points ((country 20) (volume 5) (channel 5) (cash_intensive 15)))
     (score 45) (floors ()) (band Medium))
    ((points ((volume 5) (channel 5) (ownership 40))) (score 50)
     (floors (Opaque_ownership)) (band High))
    ((points ((country 40) (volume 5) (channel 5))) (score 50)
     (floors ((Customer High_risk_third_country))) (band High))
    |}]
;;

let%expect_test "overrides cannot go below a floor" =
  let a = K.assess K.Model.default { K.Inputs.baseline with ownership = Opaque } ~at in
  let by = Operator_id.of_string "op-a" in
  print_s
    [%sexp
      (K.override a ~band:Low ~reason:"known group" ~by ~at
       |> Or_error.map ~f:K.Assessment.band
       : Risk.Band.t Or_error.t)];
  let a = K.assess K.Model.default K.Inputs.baseline ~at in
  print_s
    [%sexp
      (K.override a ~band:High ~reason:"sector news" ~by ~at
       |> Or_error.map ~f:K.Assessment.band
       : Risk.Band.t Or_error.t)];
  [%expect
    {|
    (Error
     ("a hard floor keeps the band High" (floors (Opaque_ownership))
      (requested Low)))
    (Ok High)
    |}]
;;

let%test_unit "assessments validate and floors force High" =
  Quickcheck.test K.Inputs.quickcheck_generator ~sexp_of:K.Inputs.sexp_of_t ~f:(fun i ->
    let a = K.assess K.Model.default i ~at in
    Or_error.ok_exn (K.Assessment.validate a);
    if not (List.is_empty a.floors) then assert (Risk.Band.equal a.computed_band High))
;;

let%test_unit "more opaque ownership never lowers the score" =
  Quickcheck.test K.Inputs.quickcheck_generator ~sexp_of:K.Inputs.sexp_of_t ~f:(fun i ->
    let score o = K.Model.score K.Model.default { i with ownership = o } ~on in
    List.iter
      (List.zip_exn
         (List.drop_last_exn Ownership.Opacity.all)
         (List.tl_exn Ownership.Opacity.all))
      ~f:(fun (lo, hi) -> assert (score lo <= score hi)))
;;
