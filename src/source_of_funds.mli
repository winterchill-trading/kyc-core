(** A business applicant's declaration of where the money it will move comes from and how
    much it expects to move (FATF R.10 purpose and intended nature; AMLR Art. 20(1)(c)
    and, under EDD, Art. 34(4)(b)). Evidence is a separate document
    ({!Kyb_document.Kind.Source_of_funds_evidence}) that a reviewer may request. *)

open! Core

module Source : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Operating_revenue
        | Owner_equity
        | Debt_financing
        | Investment_income
        | Sale_of_assets
        | Grants_or_donations
        | Other of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Operating_revenue
    | Owner_equity (** capital paid in by the owners *)
    | Debt_financing
    | Investment_income
    | Sale_of_assets
    | Grants_or_donations
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Stable : sig
  module V1 : sig
    type t =
      { sources : Source.Stable.V1.t list
      ; description : string
      ; monthly_volume : Risk.Volume_band.Stable.V1.t
      ; counterparty_countries : Country.Stable.V1.t list
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { sources : Source.t list
  ; description : string (** in the applicant's words *)
  ; monthly_volume : Risk.Volume_band.t (** expected, in EUR *)
  ; counterparty_countries : Country.t list (** main countries money comes from or goes to *)
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** At least one source, no repeats, a description of 1 to 2000 characters. *)
val validate : t -> unit Or_error.t
