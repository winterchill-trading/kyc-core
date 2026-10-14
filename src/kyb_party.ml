open! Core

module Role = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Director
        | Authorised_representative
        | Shareholder
        | Control_by_other_means of string
        | Senior_managing_official
        | Settlor
        | Trustee
        | Protector
        | Beneficiary
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Director
    | Authorised_representative
    | Shareholder
    | Control_by_other_means of string
    | Senior_managing_official
    | Settlor
    | Trustee
    | Protector
    | Beneficiary
  [@@deriving compare, equal, hash, sexp, quickcheck]

  let is_trust_role = function
    | Settlor | Trustee | Protector | Beneficiary -> true
    | Director
    | Authorised_representative
    | Shareholder
    | Control_by_other_means _
    | Senior_managing_official -> false
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Customer_id.Stable.V1.t
      ; roles : Role.Stable.V1.t list
      ; chains : Ownership.Chain.Stable.V1.t list
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : Customer_id.t
  ; roles : Role.t list
  ; chains : Ownership.Chain.t list
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

let has_role t role = List.mem t.roles role ~equal:Role.equal

let effective_share t =
  List.fold t.chains ~init:Ownership.Share.zero ~f:(fun acc c ->
    Ownership.Share.( + ) acc (Ownership.effective c))
;;

module Basis = struct
  type t =
    | Ownership of Ownership.Share.t
    | Control of string
    | Trust_role of Role.t
    | Senior_managing_official
  [@@deriving compare, equal, sexp_of]
end

let beneficial_owners legal_form parties ~threshold_bps =
  let threshold = Ownership.Share.of_bps threshold_bps in
  let arrangement = Legal_form.is_legal_arrangement legal_form in
  let basis p =
    let share = effective_share p in
    List.filter_opt
      [ Option.some_if
          (has_role p Shareholder && Ownership.Share.( >= ) share threshold)
          (Basis.Ownership share)
      ]
    @ List.filter_map p.roles ~f:(fun (r : Role.t) ->
      match r with
      | Control_by_other_means how -> Some (Basis.Control how)
      | (Settlor | Trustee | Protector | Beneficiary) when arrangement ->
        Some (Basis.Trust_role r)
      | _ -> None)
  in
  match
    List.filter_map parties ~f:(fun p ->
      match basis p with
      | [] -> None
      | b -> Some (p, b))
  with
  | _ :: _ as owners -> owners
  | [] ->
    List.filter_map parties ~f:(fun p ->
      Option.some_if
        (has_role p Senior_managing_official)
        (p, [ Basis.Senior_managing_official ]))
;;

let max_parties = 50

let validate t =
  let errors =
    List.filter_opt
      [ Option.some_if (List.is_empty t.roles) [%message "a party needs a role"]
      ; Option.some_if
          (List.contains_dup t.roles ~compare:Role.compare)
          [%message "repeated role"]
      ; Option.some_if
          (has_role t Shareholder && List.is_empty t.chains)
          [%message "a shareholder needs an ownership chain"]
      ; Option.some_if
          ((not (has_role t Shareholder)) && not (List.is_empty t.chains))
          [%message "ownership chains without the shareholder role"]
      ; (match
           List.map t.chains ~f:Ownership.Chain.validate |> Or_error.combine_errors
         with
         | Ok _ -> None
         | Error e -> Some (Error.sexp_of_t e))
      ]
  in
  match errors with
  | [] -> Ok ()
  | errors ->
    Or_error.error_s
      [%message "invalid party" ~id:(t.id : Customer_id.t) (errors : Sexp.t list)]
;;

let validate_all parties =
  let open Or_error.Let_syntax in
  let%bind _ = List.map parties ~f:validate |> Or_error.combine_errors in
  let total =
    List.fold parties ~init:Ownership.Share.zero ~f:(fun acc p ->
      Ownership.Share.( + ) acc (effective_share p))
  in
  if List.length parties > max_parties
  then Or_error.error_s [%message "too many parties" ~max:(max_parties : int)]
  else if List.contains_dup parties ~compare:(fun a b -> Customer_id.compare a.id b.id)
  then Or_error.error_s [%message "a person is listed twice"]
  else if Ownership.Share.( > ) total Ownership.Share.whole
  then
    Or_error.error_s
      [%message
        "ownership adds up to more than 100%"
          ~total:(Ownership.Share.to_percent_string total : string)]
  else Ok ()
;;

let needs_approved_case legal_form parties ~threshold_bps =
  let owners =
    beneficial_owners legal_form parties ~threshold_bps
    |> List.map ~f:(fun (p, _) -> p.id)
    |> Customer_id.Set.of_list
  in
  List.filter_map parties ~f:(fun p ->
    Option.some_if
      (Set.mem owners p.id || has_role p Director || has_role p Authorised_representative)
      p.id)
;;
