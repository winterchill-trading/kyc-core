(** Which jurisdictions a bank onboards businesses from, which legal forms it accepts
    there, and which documents it needs, by legal form and jurisdiction. This is policy
    (configuration in sexp form), not law: {!default} is a reasonable starting point that
    a bank replaces with its own table. *)

open! Core

module Limit : sig
  (** A monthly volume limit that applies to businesses onboarded under a jurisdiction,
      for the caller to enforce. *)
  type t =
    { currency : string (** ISO 4217 *)
    ; amount_minor : int
    }
  [@@deriving compare, equal, sexp]
end

module Jurisdiction : sig
  type t =
    { country : Country.t
    ; covered : bool (** businesses registered here may be onboarded *)
    ; legal_forms : Legal_form.t list (** accepted forms; empty accepts every form *)
    ; extra_documents : Kyb_document.Kind.t list (** on top of the legal form's list *)
    ; monthly_limit : Limit.t option
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { by_legal_form : (Legal_form.t * Kyb_document.Kind.t list) list
  ; other_forms : Kyb_document.Kind.t list (** for forms missing from [by_legal_form] *)
  ; layered_ownership : Kyb_document.Kind.t list
  (** when any owner holds through an intermediate *)
  ; per_representative : Kyb_document.Kind.t list
  (** for each authorised representative who is not a director *)
  ; jurisdictions : Jurisdiction.t list
  ; unlisted_covered : bool
  (** whether a country missing from [jurisdictions] is covered *)
  }
[@@deriving compare, equal, sexp]

(** Companies: certificate of incorporation, articles, shareholder register (private) or
    register extract (public), register of directors. Partnerships: register extract and
    partnership agreement. Sole traders: register extract. Cooperatives and associations:
    register extract and articles. Foundations: register extract and charter. Trusts:
    trust deed. Others: register extract and articles. An ownership chart for layered
    ownership, a power of attorney per representative. No jurisdictions listed and every
    country covered. *)
val default : t

(** The country's entry, or a covered-per-[unlisted_covered] entry with no extras. *)
val jurisdiction : t -> Country.t -> Jurisdiction.t

(** The entity's jurisdiction is covered and accepts its legal form. *)
val check_entity : t -> Legal_entity.t -> unit Or_error.t

(** Documents needed for the entity and its parties, sorted and without repeats. *)
val required : t -> Legal_entity.t -> Kyb_party.t list -> Kyb_document.Requirement.t list
