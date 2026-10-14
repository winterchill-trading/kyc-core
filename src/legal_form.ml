open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Sole_trader
      | General_partnership
      | Limited_partnership
      | Private_limited_company
      | Public_limited_company
      | Cooperative
      | Association
      | Foundation
      | Trust
      | Other of string
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Sole_trader
  | General_partnership
  | Limited_partnership
  | Private_limited_company
  | Public_limited_company
  | Cooperative
  | Association
  | Foundation
  | Trust
  | Other of string
[@@deriving compare, equal, hash, sexp, quickcheck]

let standard =
  [ Sole_trader
  ; General_partnership
  ; Limited_partnership
  ; Private_limited_company
  ; Public_limited_company
  ; Cooperative
  ; Association
  ; Foundation
  ; Trust
  ]
;;

let is_legal_arrangement = function
  | Trust -> true
  | Sole_trader
  | General_partnership
  | Limited_partnership
  | Private_limited_company
  | Public_limited_company
  | Cooperative
  | Association
  | Foundation
  | Other _ -> false
;;

let to_string = function
  | Other s -> "other:" ^ s
  | t -> Sexp.to_string (sexp_of_t t) |> String.lowercase
;;

let of_string s =
  match String.chop_prefix s ~prefix:"other:" with
  | Some name when not (String.is_empty (String.strip name)) -> Ok (Other name)
  | Some _ -> Or_error.error_s [%message "empty national legal form name"]
  | None ->
    (match List.find standard ~f:(fun t -> String.equal (to_string t) s) with
     | Some t -> Ok t
     | None -> Or_error.error_s [%message "unknown legal form" (s : string)])
;;
