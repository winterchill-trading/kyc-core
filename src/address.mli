(** A postal address. The whole address is PII; {!Person} wraps it in [Pii.t]. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { line1 : string
      ; line2 : string option
      ; city : string
      ; region : string option (** state, province or county where used *)
      ; postal_code : string option (** some countries have none *)
      ; country : Country.Stable.V1.t
      }
    [@@deriving bin_io, compare, equal, hash, sexp]

    include Quickcheckable.S with type t := t
  end
end

type t = Stable.V1.t =
  { line1 : string
  ; line2 : string option
  ; city : string
  ; region : string option
  ; postal_code : string option
  ; country : Country.t
  }
[@@deriving compare, equal, hash, sexp, quickcheck]
