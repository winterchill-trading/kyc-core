(** Who may see what. Every audit change and every piece of case state has a class; an
    audience sees a class or not.

    - [Public]: the customer's own status and what they must do next.
    - [Internal]: operations data (risk band, screening, review dates).
    - [Pii] / [Sensitive_pii]: personal data; sensitive is e.g. document images or PEP
      details.
    - [Restricted]: monitoring alerts, investigations, reports and FIU requests.
      Disclosing them to the customer is tipping-off (FATF R.21, AMLR Art. 73), so only
      compliance sees them. *)

open! Core

module Class : sig
  type t =
    | Public
    | Internal
    | Pii
    | Sensitive_pii
    | Restricted
  [@@deriving compare, equal, hash, sexp, enumerate]
end

module Audience : sig
  type t =
    | Customer (** the customer, about themselves *)
    | Support (** customer-facing staff *)
    | Compliance
  [@@deriving compare, equal, hash, sexp, enumerate]
end

val can_see : Audience.t -> Class.t -> bool
