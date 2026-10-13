open! Core

module Stable = struct
  module V1 = struct
    type t =
      | T0
      | T1
      | T2
      | T3
    [@@deriving bin_io, compare, equal, hash, sexp, enumerate]
  end
end

type t = Stable.V1.t =
  | T0
  | T1
  | T2
  | T3
[@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

include Comparable.Make (Stable.V1)

module Rules = struct
  module Step = Verification.Step
  module Tier = Stable.V1

  let all_tiers = all

  (* Ascending by tier, one entry per tier; [create] guarantees it. *)
  type t = (Tier.t * Step.Set.t) list [@@deriving compare, equal, sexp]

  let create entries =
    let open Or_error.Let_syntax in
    let%bind () =
      match List.find_a_dup entries ~compare:(fun (a, _) (b, _) -> Tier.compare a b) with
      | Some (tier, _) -> Or_error.error_s [%message "tier listed twice" (tier : Tier.t)]
      | None -> Ok ()
    in
    let%bind table =
      List.map all_tiers ~f:(fun tier ->
        match List.Assoc.find entries tier ~equal:Tier.equal with
        | Some steps -> Ok (tier, Step.Set.of_list steps)
        | None -> Or_error.error_s [%message "tier missing from rules" (tier : Tier.t)])
      |> Or_error.combine_errors
    in
    let%bind () =
      match table with
      | (T0, steps) :: _ when not (Core.Set.is_empty steps) ->
        Or_error.error_s [%message "T0 must require no steps" (steps : Step.Set.t)]
      | _ -> Ok ()
    in
    let rec check_monotone = function
      | (lower, below) :: ((higher, above) :: _ as rest) ->
        if Core.Set.is_subset below ~of_:above
        then check_monotone rest
        else
          Or_error.error_s
            [%message
              "a tier must require every step of the tier below"
                (higher : Tier.t)
                (lower : Tier.t)
                ~missing:(Core.Set.diff below above : Step.Set.t)]
      | [ _ ] | [] -> Ok ()
    in
    let%map () = check_monotone table in
    table
  ;;

  let default =
    let t1 = Step.[ Email; Mobile ] in
    let t2 = t1 @ Step.[ Identity_document; Liveness ] in
    let t3 = t2 @ Step.[ Address_proof; Source_of_funds ] in
    Or_error.ok_exn (create [ T0, []; T1, t1; T2, t2; T3, t3 ])
  ;;

  let required_for t tier = List.Assoc.find_exn t tier ~equal:Tier.equal

  let tier_of t ~verified =
    List.fold t ~init:T0 ~f:(fun acc (tier, required) ->
      if Core.Set.is_subset required ~of_:verified then tier else acc)
  ;;

  let to_list t = t
end

module type Limits = sig
  type limits [@@deriving sexp_of]

  val limits : t -> limits
end
