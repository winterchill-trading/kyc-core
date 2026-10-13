(** The minimal personal data a verification needs. Every field is {!Pii.t}, so printing a
    person (or an event that carries one) never shows the data. *)

open! Core

module Field : sig
  (** Names a field, e.g. in a request to resubmit it. Carries no data. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Given_name
        | Family_name
        | Date_of_birth
        | Nationality
        | Residential_address
        | Email
        | Mobile
      [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
    end
  end

  type t = Stable.V1.t =
    | Given_name
    | Family_name
    | Date_of_birth
    | Nationality
    | Residential_address
    | Email
    | Mobile
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.S with type t := t
end

module Stable : sig
  module V1 : sig
    type t =
      { given_name : string Pii.Stable.V1.t
      ; family_name : string Pii.Stable.V1.t
      ; date_of_birth : Date.Stable.V1.t Pii.Stable.V1.t
      ; nationality : Country.Stable.V1.t Pii.Stable.V1.t
      ; residential_address : Address.Stable.V1.t Pii.Stable.V1.t
      ; email : string Pii.Stable.V1.t
      ; mobile : string Pii.Stable.V1.t (** E.164, e.g. [+4930...] *)
      }
    [@@deriving bin_io, compare, equal, hash, sexp]

    include Quickcheckable.S with type t := t
  end
end

type t = Stable.V1.t =
  { given_name : string Pii.t
  ; family_name : string Pii.t
  ; date_of_birth : Date.t Pii.t
  ; nationality : Country.t Pii.t
  ; residential_address : Address.t Pii.t
  ; email : string Pii.t
  ; mobile : string Pii.t
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** Fields whose values differ between two versions of a person, e.g. to check that a
    resubmission changed what was asked for. *)
val changed_fields : t -> t -> Field.t list

(** Age in whole years on [on]. *)
val age : t -> on:Date.t -> int
