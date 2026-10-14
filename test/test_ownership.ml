open! Core
open Kyc_core
module O = Ownership

let entity =
  Legal_entity.create
    ~legal_name:"Example Trading Ltd"
    ~jurisdiction:(Country.of_string "GB")
    ~legal_form:Private_limited_company
;;

let holding ?(country = "GB") ?(form = Legal_form.Private_limited_company) ?(nominee = false) name bps
  : O.Intermediate.t
  =
  { name
  ; jurisdiction = Country.of_string country
  ; legal_form = form
  ; registration_number = None
  ; stake_bps = bps
  ; nominee
  ; bearer_shares = false
  }
;;

let show chain =
  let share = O.effective chain in
  printf "%s (%d bps)\n" (O.Share.to_percent_string share) (O.Share.to_bps share)
;;

let%expect_test "effective share multiplies along the chain" =
  show (O.Chain.direct ~bps:2500);
  show { stake_bps = 5000; via = [ holding "Holdco" 5000 ] };
  show { stake_bps = 10_000; via = [ holding "A" 3333; holding "B" 7500 ] };
  show { stake_bps = 6000; via = [ holding "A" 7000; holding "B" 8000; holding "C" 9000 ] };
  [%expect
    {|
    25% (2500 bps)
    25% (2500 bps)
    24.9975% (2499 bps)
    30.24% (3024 bps)
    |}]
;;

let%test_unit "a chain never exceeds its weakest stake" =
  Quickcheck.test
    (let open Quickcheck.Generator.Let_syntax in
     let bps = Int.gen_incl 1 10_000 in
     let%map stake_bps = bps
     and stakes = List.gen_with_length 4 bps
     and layers = Int.gen_incl 0 4 in
     ({ stake_bps
      ; via = List.take stakes layers |> List.map ~f:(fun b -> holding "X" b)
      }
      : O.Chain.t))
    ~sexp_of:O.Chain.sexp_of_t
    ~f:(fun c ->
      let weakest =
        List.fold c.via ~init:c.stake_bps ~f:(fun m i -> Int.min m i.stake_bps)
      in
      assert (O.Share.( <= ) (O.effective c) (O.Share.of_bps weakest)))
;;

let%expect_test "validation" =
  let check c = print_s [%sexp (O.Chain.validate c : unit Or_error.t)] in
  check (O.Chain.direct ~bps:0);
  check { stake_bps = 5000; via = [ holding " " 10_001 ] };
  check { stake_bps = 5000; via = List.init 11 ~f:(fun i -> holding (sprintf "L%d" i) 10_000) };
  [%expect
    {|
    (Error
     ("invalid ownership chain" (errors (("stake out of range" (stake_bps 0))))))
    (Error
     ("invalid ownership chain"
      (errors
       (("intermediate stake out of range" (name " ") (stake_bps 10001))
        "intermediate without a name"))))
    (Error
     ("invalid ownership chain"
      (errors (("too many intermediates" (layers 11))))))
    |}]
;;

let%expect_test "opacity" =
  let show entity chains = print_s [%sexp (O.opacity entity chains : O.Opacity.t)] in
  show entity [ O.Chain.direct ~bps:10_000 ];
  show entity [ { stake_bps = 10_000; via = [ holding "Holdco" 10_000 ] } ];
  show
    entity
    [ { stake_bps = 10_000; via = [ holding "A" 10_000; holding "B" 10_000; holding "C" 10_000 ] }
    ];
  show entity [ { stake_bps = 10_000; via = [ holding ~form:Trust "Family Trust" 10_000 ] } ];
  show
    entity
    [ { stake_bps = 10_000; via = [ holding ~country:"LU" "A" 10_000 ] }
    ; { stake_bps = 10_000; via = [ holding ~country:"CY" "B" 10_000 ] }
    ];
  show entity [ { stake_bps = 10_000; via = [ holding ~nominee:true "Nominee" 10_000 ] } ];
  show { entity with bearer_shares = true } [ O.Chain.direct ~bps:10_000 ];
  [%expect
    {|
    Transparent
    Layered
    Complex
    Complex
    Complex
    Opaque
    Opaque
    |}]
;;
