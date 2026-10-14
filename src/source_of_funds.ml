open! Core

module Source = struct
  module Stable = struct
    module V1 = struct
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
    | Owner_equity
    | Debt_financing
    | Investment_income
    | Sale_of_assets
    | Grants_or_donations
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Stable = struct
  module V1 = struct
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
  ; description : string
  ; monthly_volume : Risk.Volume_band.t
  ; counterparty_countries : Country.t list
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

let validate t =
  let d = String.strip t.description in
  if List.is_empty t.sources
  then Or_error.error_s [%message "no source of funds declared"]
  else if List.contains_dup t.sources ~compare:Source.compare
  then Or_error.error_s [%message "repeated source of funds"]
  else if String.is_empty d || String.length d > 2000
  then Or_error.error_s [%message "description must be 1 to 2000 characters"]
  else Ok ()
;;
