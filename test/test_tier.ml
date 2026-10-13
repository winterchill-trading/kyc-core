open! Core
open Kyc_core
module Step = Verification.Step

let%expect_test "default rules" =
  print_s [%sexp (Tier.Rules.default : Tier.Rules.t)];
  [%expect
    {|
    ((T0 ()) (T1 (Email Mobile)) (T2 (Email Mobile Identity_document Liveness))
     (T3 (Email Mobile Identity_document Liveness Address_proof Source_of_funds)))
    |}]
;;

let%expect_test "tier_of" =
  let show steps =
    print_s
      [%sexp
        (Tier.Rules.tier_of Tier.Rules.default ~verified:(Step.Set.of_list steps)
         : Tier.t)]
  in
  show [];
  show [ Email ];
  show [ Email; Mobile ];
  (* Identity without contact details does not skip T1. *)
  show [ Identity_document; Liveness ];
  show [ Email; Mobile; Identity_document; Liveness; Address_proof ];
  show Step.all;
  [%expect {|
    T0
    T0
    T1
    T0
    T2
    T3
    |}]
;;

let%expect_test "create rejects malformed tables" =
  let show entries =
    print_s [%sexp (Tier.Rules.create entries : Tier.Rules.t Or_error.t)]
  in
  show [ T0, []; T1, [ Email ]; T2, [ Email ]; T3, [ Email ]; T1, [] ];
  show [ T0, []; T1, [ Email ]; T2, [ Email ] ];
  show [ T0, [ Email ]; T1, [ Email ]; T2, [ Email ]; T3, [ Email ] ];
  show [ T0, []; T1, [ Email; Mobile ]; T2, [ Identity_document ]; T3, Step.all ];
  [%expect
    {|
    (Error ("tier listed twice" (tier T1)))
    (Error ("tier missing from rules" (tier T3)))
    (Error ("T0 must require no steps" (steps (Email))))
    (Error
     ("a tier must require every step of the tier below" (higher T2) (lower T1)
      (missing (Email Mobile))))
    |}]
;;

let%test_unit "tier_of is monotone in verified steps" =
  Quickcheck.test
    [%quickcheck.generator: Step.t list * Step.t list]
    ~sexp_of:[%sexp_of: Step.t list * Step.t list]
    ~f:(fun (a, b) ->
      let small = Step.Set.of_list a in
      let large = Set.union small (Step.Set.of_list b) in
      let tier verified = Tier.Rules.tier_of Tier.Rules.default ~verified in
      assert (Tier.( <= ) (tier small) (tier large)))
;;

let%test_unit "the tier's own requirements are met" =
  Quickcheck.test
    [%quickcheck.generator: Step.t list]
    ~sexp_of:[%sexp_of: Step.t list]
    ~f:(fun steps ->
      let verified = Step.Set.of_list steps in
      let tier = Tier.Rules.tier_of Tier.Rules.default ~verified in
      assert (
        Set.is_subset (Tier.Rules.required_for Tier.Rules.default tier) ~of_:verified))
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Tier.Stable.V1.t];
  [%expect {| f8e0ccc810e23ec490dd0eb668113eea |}]
;;
