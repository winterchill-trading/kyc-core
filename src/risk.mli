(** Risk-based customer risk rating (FATF Recommendation 1 and 10; PEPs under
    Recommendation 12).

    The bank's methodology scores the country, product and channel factors (from its
    country list, product catalogue and onboarding channel); this module combines them
    with PEP exposure and adverse media into a rating, lists the enhanced due diligence
    (EDD) triggers, and records any operator override with its rationale. *)

open! Core

module Level : sig
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
end

module Pep_exposure : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Not_pep
        | Domestic
        | Foreign
        | International_organisation
        | Family_or_close_associate of t (** of a PEP of the given kind *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Not_pep
    | Domestic
    | Foreign
    | International_organisation
    | Family_or_close_associate of t
  [@@deriving compare, equal, hash, sexp, quickcheck]

  val is_pep : t -> bool

  (** Foreign PEPs (and their family and associates) always need EDD under FATF R.12;
      domestic and international-organisation PEPs need it when the relationship is higher
      risk. *)
  val is_foreign : t -> bool
end

module Factors : sig
  module Stable : sig
    module V1 : sig
      type t =
        { country : Level.Stable.V1.t (** residence and nationality, worst of *)
        ; product : Level.Stable.V1.t
        ; channel : Level.Stable.V1.t (** e.g. non-face-to-face onboarding *)
        ; pep : Pep_exposure.Stable.V1.t
        ; adverse_media : bool
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { country : Level.t
    ; product : Level.t
    ; channel : Level.t
    ; pep : Pep_exposure.t
    ; adverse_media : bool
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Edd_trigger : sig
  module Stable : sig
    module V1 : sig
      type t =
        | High_risk_rating
        | Foreign_pep
        | Pep_in_higher_risk_relationship
        | High_risk_country
        | Adverse_media
        | Monitoring_alert (** an open high-severity monitoring alert *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | High_risk_rating
    | Foreign_pep
    | Pep_in_higher_risk_relationship
    | High_risk_country
    | Adverse_media
    | Monitoring_alert
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

(** The rating the factors give: the worst factor, raised to [High] for any PEP of foreign
    kind, and to at least [Medium] for any other PEP or for adverse media. *)
val rate : Factors.t -> Level.t

(** EDD triggers for factors and a final rating, without monitoring alerts. *)
val edd_triggers : Factors.t -> rating:Level.t -> Edd_trigger.t list

module Override : sig
  module Stable : sig
    module V1 : sig
      type t =
        { rating : Level.Stable.V1.t
        ; rationale : string
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { rating : Level.t
    ; rationale : string
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

(** A recorded assessment. [rating] is [computed] unless an operator overrode it; an
    override may raise the rating freely but lowering it below [computed] is refused for
    foreign PEPs (EDD cannot be waived). *)
type t =
  { assessed_at : Timestamp.t
  ; factors : Factors.t
  ; computed : Level.t
  ; rating : Level.t
  ; override : (Override.t * Operator_id.t) option
  }
[@@deriving compare, equal, hash, sexp]

val assess
  :  assessed_at:Timestamp.t
  -> factors:Factors.t
  -> override:Override.t option
  -> actor:Actor.t
  -> t Or_error.t
