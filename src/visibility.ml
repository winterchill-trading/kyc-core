open! Core

module Class = struct
  type t =
    | Public
    | Internal
    | Pii
    | Sensitive_pii
    | Restricted
  [@@deriving compare, equal, hash, sexp, enumerate]
end

module Audience = struct
  type t =
    | Customer
    | Support
    | Compliance
  [@@deriving compare, equal, hash, sexp, enumerate]
end

let can_see (audience : Audience.t) (cls : Class.t) =
  match audience, cls with
  | Compliance, _ -> true
  | (Customer | Support), Restricted -> false
  | Customer, Internal -> false
  | Customer, (Public | Pii | Sensitive_pii) -> true
  (* Support helps the customer but does not handle documents or PEP files. *)
  | Support, (Public | Internal | Pii) -> true
  | Support, Sensitive_pii -> false
;;
