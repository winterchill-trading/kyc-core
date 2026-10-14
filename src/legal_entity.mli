(** A business applicant as registered: name, registration number, jurisdiction, legal
    form and addresses. Company register data is public, so nothing here is {!Pii.t}; the
    people linked to the entity have their own {!Kyc_case}s.

    Only [legal_name], [jurisdiction] and [legal_form] are needed to open a case; the rest
    may be filled in while drafting, and {!missing_fields} says what submission still
    needs. *)

open! Core

module Field : sig
  (** Names a field, e.g. in a request for more information. Carries no data. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Legal_name
        | Registration_number
        | Jurisdiction
        | Legal_form
        | Incorporation_date
        | Registered_address
        | Business_address
        | Industry
        | Bearer_shares
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Legal_name
    | Registration_number
    | Jurisdiction
    | Legal_form
    | Incorporation_date
    | Registered_address
    | Business_address
    | Industry
    | Bearer_shares
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Stable : sig
  module V1 : sig
    type t =
      { legal_name : string
      ; registration_number : string option
      ; jurisdiction : Country.Stable.V1.t
      ; legal_form : Legal_form.Stable.V1.t
      ; incorporated_on : Date.Stable.V1.t option
      ; registered_address : Address.Stable.V1.t option
      ; business_address : Address.Stable.V1.t option
      ; industry : string option
      ; bearer_shares : bool
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { legal_name : string
  ; registration_number : string option
  (** in the register of [jurisdiction]; trusts often have none *)
  ; jurisdiction : Country.t (** country of registration *)
  ; legal_form : Legal_form.t
  ; incorporated_on : Date.t option
  ; registered_address : Address.t option
  ; business_address : Address.t option (** main place of business, if different *)
  ; industry : string option
  (** activity code in the bank's classification, e.g. NACE Rev. 2 ["62.01"] *)
  ; bearer_shares : bool
  (** the entity has issued bearer shares, so its owners cannot be read from a register *)
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** A draft with only the three opening fields. *)
val create : legal_name:string -> jurisdiction:Country.t -> legal_form:Legal_form.t -> t

(** Shape checks: a legal name of 1 to 200 characters with no control characters and no
    surrounding blanks, a non-blank registration number when given, an incorporation date
    not after [today]. *)
val validate : t -> today:Date.t -> unit Or_error.t

(** Fields whose values differ between two versions. *)
val changed_fields : t -> t -> Field.t list

(** Fields submission needs that are empty: registration number (except for trusts),
    incorporation date, registered address and industry. *)
val missing_fields : t -> Field.t list
