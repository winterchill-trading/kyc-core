(** Risk-based customer risk scoring (FATF R.1 and R.10; EBA/GL/2021/02).

    Inputs cover the four EBA factor groups: customer (sector, PEP status,
    adverse media), country (residence, nationality), product (products,
    expected volume, self-hosted wallet exposure) and delivery channel. A
    versioned {!Model} turns inputs into points and a score from 0 to 100;
    the score gives a {!Band}. Hard floors that weighting cannot remove (EBA
    3.4-3.7): an active PEP, a high-risk third country or confirmed adverse
    media make the band [High]. An operator may override the band with a
    reason, but never below a hard floor. *)

open! Core

module Band : sig
  (** Risk band. From a score: Low 0-29, Medium 30-59, High 60-100 (our cut-offs). *)

  module Stable : sig
    module V1 : sig
      type t =
        | Low
        | Medium
        | High
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Low
    | Medium
    | High
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.S with type t := t

  val of_score : int -> t
end

module Country_risk : sig
  (** From the bank's country list; [High_risk_third_country] comes from the loaded EU list. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Standard
        | Elevated
        | High_risk_third_country (** on the EU list of high-risk third countries *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Standard
    | Elevated
    | High_risk_third_country (** on the EU list of high-risk third countries *)
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Adverse_media : sig
  module Stable : sig
    module V1 : sig
      type t =
        | No_findings
        | Unconfirmed
        | Confirmed
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | No_findings
    | Unconfirmed
    | Confirmed
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Product : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Fiat_account
        | Card
        | Crypto
        | Lightning
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Fiat_account
    | Card
    | Crypto
    | Lightning
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Volume_band : sig
  (** Expected monthly volume, EUR. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Up_to_1k
        | Up_to_10k
        | Up_to_100k
        | Above_100k
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Up_to_1k
    | Up_to_10k
    | Up_to_100k
    | Above_100k
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Channel : sig
  (** How the customer was onboarded. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Face_to_face
        | Eid_high (** electronic identification at eIDAS "high"; does not raise risk (EBA 4.29-4.31) *)
        | Remote_attended
        | Remote_unattended
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Face_to_face
    | Eid_high (** electronic identification at eIDAS "high"; does not raise risk (EBA 4.29-4.31) *)
    | Remote_attended
    | Remote_unattended
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Edd_trigger : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Pep
        | Family_or_close_associate
        | High_risk_third_country
        | Unusual_transaction (** from an escalated transaction-monitoring alert *)
        | Self_hosted_exposure
        | Adverse_media
        | Manual of string (** an operator's own reason *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Pep
    | Family_or_close_associate
    | High_risk_third_country
    | Unusual_transaction
    | Self_hosted_exposure
    | Adverse_media
    | Manual of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Inputs : sig
  module Stable : sig
    module V1 : sig
      type t =
        { sector : Band.Stable.V1.t (** occupation or business sector risk *)
        ; pep : Pep.Stable.V1.t option
        ; adverse_media : Adverse_media.Stable.V1.t
        ; residence : Country_risk.Stable.V1.t
        ; nationality : Country_risk.Stable.V1.t
        ; products : Product.Stable.V1.t list
        ; monthly_volume : Volume_band.Stable.V1.t
        ; channel : Channel.Stable.V1.t
        ; self_hosted_exposure : bool
        ; manual_edd : string list (** operator reasons to apply EDD anyway *)
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { sector : Band.t
    ; pep : Pep.t option
    ; adverse_media : Adverse_media.t
    ; residence : Country_risk.t
    ; nationality : Country_risk.t
    ; products : Product.t list
    ; monthly_volume : Volume_band.t
    ; channel : Channel.t
    ; self_hosted_exposure : bool
    ; manual_edd : string list
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  (** Low-risk retail defaults: no PEP, no findings, standard countries, a
      fiat account, up to 1k a month, face to face. *)
  val baseline : t
end

(** EDD triggers that follow from the inputs on a date (PEP EDD ends at
    [Pep.edd_until]). [Unusual_transaction] comes from monitoring, not from
    here. *)
val edd_triggers : Inputs.t -> on:Date.t -> Edd_trigger.t list

(** The triggers that force [High] whatever the score. *)
val hard_floors : Inputs.t -> on:Date.t -> Edd_trigger.t list

module Model : sig
  (** Points per input value. Versioned: an assessment records the version
      it was scored with, and a change of weights is a new version. *)
  type t [@@deriving compare, equal, sexp]

  (** Our weights, version ["kyc-risk-2026-10"]. *)
  val default : t

  val version : t -> string

  (** Points per factor for the inputs, before capping at 100. *)
  val points : t -> Inputs.t -> on:Date.t -> (string * int) list

  val score : t -> Inputs.t -> on:Date.t -> int
end

module Override : sig
  module Stable : sig
    module V1 : sig
      type t =
        { band : Band.Stable.V1.t
        ; reason : string
        ; by : Operator_id.Stable.V1.t
        ; at : Timestamp.Stable.V1.t
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { band : Band.t
    ; reason : string
    ; by : Operator_id.t
    ; at : Timestamp.t
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Assessment : sig
  module Stable : sig
    module V1 : sig
      type t =
        { inputs : Inputs.Stable.V1.t
        ; model_version : string
        ; score : int
        ; floors : Edd_trigger.Stable.V1.t list
        ; computed_band : Band.Stable.V1.t
        ; override : Override.Stable.V1.t option
        ; computed_at : Timestamp.Stable.V1.t
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  (** What was computed, kept as computed: replaying an old assessment never
      rescored it with a newer model. *)
  type t = Stable.V1.t =
    { inputs : Inputs.t
    ; model_version : string
    ; score : int
    ; floors : Edd_trigger.t list
    ; computed_band : Band.t
    ; override : Override.t option
    ; computed_at : Timestamp.t
    }
  [@@deriving compare, equal, hash, sexp]

  (** The override's band if any, else [computed_band]. *)
  val band : t -> Band.t

  (** Internal consistency of a recorded assessment: score in 0-100,
      [computed_band] matches the score and the floors, and any override has
      a reason and respects the floors. *)
  val validate : t -> unit Or_error.t
end

val assess : Model.t -> Inputs.t -> at:Timestamp.t -> Assessment.t

val override
  :  Assessment.t
  -> band:Band.t
  -> reason:string
  -> by:Operator_id.t
  -> at:Timestamp.t
  -> Assessment.t Or_error.t
