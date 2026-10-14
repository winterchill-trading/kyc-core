(** Risk rating for a business applicant. It reuses {!Risk}: the entity's factors are
    mapped onto {!Risk.Inputs} (industry as the sector, the registration country as
    residence, the worst of the other countries as nationality, a PEP among the linked
    people as the PEP) and scored with {!Risk.Model}; two entity factors are added on top,
    ownership opacity and a cash-intensive business (EBA/GL/2021/02 2.3-2.6, AMLD Annex
    III). Bands, overrides and the hard floors work as in {!Risk}; opaque ownership is one
    more hard floor. *)

open! Core

module Inputs : sig
  module Stable : sig
    module V1 : sig
      type t =
        { industry : Risk.Band.Stable.V1.t
        ; cash_intensive : bool
        ; jurisdiction : Risk.Country_risk.Stable.V1.t
        ; places_of_business : Risk.Country_risk.Stable.V1.t
        ; owner_countries : Risk.Country_risk.Stable.V1.t
        ; ownership : Ownership.Opacity.Stable.V1.t
        ; pep : Pep.Stable.V1.t option
        ; adverse_media : Risk.Adverse_media.Stable.V1.t
        ; products : Risk.Product.Stable.V1.t list
        ; monthly_volume : Risk.Volume_band.Stable.V1.t
        ; channel : Risk.Channel.Stable.V1.t
        ; self_hosted_exposure : bool
        ; manual_edd : string list
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { industry : Risk.Band.t (** risk of the business activity *)
    ; cash_intensive : bool
    ; jurisdiction : Risk.Country_risk.t (** country of registration *)
    ; places_of_business : Risk.Country_risk.t
    (** worst of the main places of business and counterparty countries *)
    ; owner_countries : Risk.Country_risk.t
    (** worst of the beneficial owners' residences and the intermediates' jurisdictions *)
    ; ownership : Ownership.Opacity.t
    ; pep : Pep.t option
    (** a beneficial owner, director or representative who is a PEP *)
    ; adverse_media : Risk.Adverse_media.t (** about the entity *)
    ; products : Risk.Product.t list
    ; monthly_volume : Risk.Volume_band.t
    ; channel : Risk.Channel.t
    ; self_hosted_exposure : bool
    ; manual_edd : string list
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  (** A low-risk business: low-risk industry, standard countries, transparent ownership,
      no PEP or findings, a fiat account, up to 10k a month, remote attended. *)
  val baseline : t

  (** The customer-risk view of the inputs that {!Risk.Model} scores. *)
  val to_customer : t -> Risk.Inputs.t
end

module Edd_trigger : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Customer of Risk.Edd_trigger.Stable.V1.t
        | Opaque_ownership
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Customer of Risk.Edd_trigger.t
    (** as for a natural person, from {!Inputs.to_customer} *)
    | Opaque_ownership (** bearer shares or nominee holders in the structure *)
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

val edd_triggers : Inputs.t -> on:Date.t -> Edd_trigger.t list
val hard_floors : Inputs.t -> on:Date.t -> Edd_trigger.t list

module Model : sig
  type t =
    { version : string
    ; customer : Risk.Model.t
    ; ownership : (Ownership.Opacity.t * int) list
    ; cash_intensive : int
    }
  [@@deriving compare, equal, sexp]

  (** Version ["kyb-risk-2026-10"] over {!Risk.Model.default}; ownership 0 / 5 / 15 / 40
      (Transparent / Layered / Complex / Opaque), cash intensive 15. *)
  val default : t

  (** The customer model's points, then ["ownership"] and ["cash_intensive"]. *)
  val points : t -> Inputs.t -> on:Date.t -> (string * int) list

  (** Sum of the points, capped at 100. *)
  val score : t -> Inputs.t -> on:Date.t -> int
end

module Assessment : sig
  module Stable : sig
    module V1 : sig
      type t =
        { inputs : Inputs.Stable.V1.t
        ; model_version : string
        ; score : int
        ; floors : Edd_trigger.Stable.V1.t list
        ; computed_band : Risk.Band.Stable.V1.t
        ; override : Risk.Override.Stable.V1.t option
        ; computed_at : Timestamp.Stable.V1.t
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  (** Kept as computed, like {!Risk.Assessment}. *)
  type t = Stable.V1.t =
    { inputs : Inputs.t
    ; model_version : string
    ; score : int
    ; floors : Edd_trigger.t list
    ; computed_band : Risk.Band.t
    ; override : Risk.Override.t option
    ; computed_at : Timestamp.t
    }
  [@@deriving compare, equal, hash, sexp]

  val band : t -> Risk.Band.t

  (** Score in 0-100, band consistent with score and floors, override with a reason and
      not below a floor. *)
  val validate : t -> unit Or_error.t
end

val assess : Model.t -> Inputs.t -> at:Timestamp.t -> Assessment.t

val override
  :  Assessment.t
  -> band:Risk.Band.t
  -> reason:string
  -> by:Operator_id.t
  -> at:Timestamp.t
  -> Assessment.t Or_error.t
