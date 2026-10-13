open! Core

module Level = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Low
        | Medium
        | High
      [@@deriving bin_io, compare, equal, hash, sexp, enumerate]
    end
  end

  type t = Stable.V1.t =
    | Low
    | Medium
    | High
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.Make (Stable.V1)
end

module Pep_exposure = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Not_pep
        | Domestic
        | Foreign
        | International_organisation
        | Family_or_close_associate of t
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

  let rec is_pep = function
    | Not_pep -> false
    | Domestic | Foreign | International_organisation -> true
    | Family_or_close_associate of_ -> is_pep of_
  ;;

  let rec is_foreign = function
    | Foreign -> true
    | Not_pep | Domestic | International_organisation -> false
    | Family_or_close_associate of_ -> is_foreign of_
  ;;
end

module Factors = struct
  module Stable = struct
    module V1 = struct
      type t =
        { country : Level.Stable.V1.t
        ; product : Level.Stable.V1.t
        ; channel : Level.Stable.V1.t
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

module Edd_trigger = struct
  module Stable = struct
    module V1 = struct
      type t =
        | High_risk_rating
        | Foreign_pep
        | Pep_in_higher_risk_relationship
        | High_risk_country
        | Adverse_media
        | Monitoring_alert
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

let rate (f : Factors.t) =
  let worst = Level.max f.country (Level.max f.product f.channel) in
  let floor : Level.t =
    if Pep_exposure.is_foreign f.pep
    then High
    else if Pep_exposure.is_pep f.pep || f.adverse_media
    then Medium
    else Low
  in
  Level.max worst floor
;;

let edd_triggers (f : Factors.t) ~(rating : Level.t) =
  List.filter_opt
    [ Option.some_if (Level.equal rating High) Edd_trigger.High_risk_rating
    ; Option.some_if (Pep_exposure.is_foreign f.pep) Edd_trigger.Foreign_pep
    ; Option.some_if
        (Pep_exposure.is_pep f.pep
         && (not (Pep_exposure.is_foreign f.pep))
         && Level.( >= ) rating Medium)
        (* A domestic or international-organisation PEP needs EDD only in a higher-risk
           relationship; Medium is treated as higher risk here because every PEP already
           lifts the rating to Medium. *)
        Edd_trigger.Pep_in_higher_risk_relationship
    ; Option.some_if (Level.equal f.country High) Edd_trigger.High_risk_country
    ; Option.some_if f.adverse_media Edd_trigger.Adverse_media
    ]
;;

module Override = struct
  module Stable = struct
    module V1 = struct
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

type t =
  { assessed_at : Timestamp.t
  ; factors : Factors.t
  ; computed : Level.t
  ; rating : Level.t
  ; override : (Override.t * Operator_id.t) option
  }
[@@deriving compare, equal, hash, sexp]

let assess ~assessed_at ~(factors : Factors.t) ~(override : Override.t option) ~actor =
  let computed = rate factors in
  match override, (actor : Actor.t) with
  | None, _ -> Ok { assessed_at; factors; computed; rating = computed; override = None }
  | Some _, (System | Provider _) ->
    Or_error.error_s [%message "only an operator may override a risk rating"]
  | Some { rationale; _ }, Operator _ when String.is_empty (String.strip rationale) ->
    Or_error.error_s [%message "a risk override needs a rationale"]
  | Some o, Operator _
    when Pep_exposure.is_foreign factors.pep && Level.( < ) o.rating computed ->
    Or_error.error_s
      [%message
        "a foreign PEP rating cannot be lowered"
          (computed : Level.t)
          ~requested:(o.rating : Level.t)]
  | Some o, Operator operator ->
    Ok
      { assessed_at; factors; computed; rating = o.rating; override = Some (o, operator) }
;;
