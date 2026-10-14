open! Core

module Inputs = struct
  module Stable = struct
    module V1 = struct
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
    { industry : Risk.Band.t
    ; cash_intensive : bool
    ; jurisdiction : Risk.Country_risk.t
    ; places_of_business : Risk.Country_risk.t
    ; owner_countries : Risk.Country_risk.t
    ; ownership : Ownership.Opacity.t
    ; pep : Pep.t option
    ; adverse_media : Risk.Adverse_media.t
    ; products : Risk.Product.t list
    ; monthly_volume : Risk.Volume_band.t
    ; channel : Risk.Channel.t
    ; self_hosted_exposure : bool
    ; manual_edd : string list
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  let baseline =
    { industry = Low
    ; cash_intensive = false
    ; jurisdiction = Standard
    ; places_of_business = Standard
    ; owner_countries = Standard
    ; ownership = Transparent
    ; pep = None
    ; adverse_media = No_findings
    ; products = [ Fiat_account ]
    ; monthly_volume = Up_to_10k
    ; channel = Remote_attended
    ; self_hosted_exposure = false
    ; manual_edd = []
    }
  ;;

  let worst a b = if Risk.Country_risk.compare a b >= 0 then a else b

  let to_customer t : Risk.Inputs.t =
    { sector = t.industry
    ; pep = t.pep
    ; adverse_media = t.adverse_media
    ; residence = t.jurisdiction
    ; nationality = worst t.places_of_business t.owner_countries
    ; products = t.products
    ; monthly_volume = t.monthly_volume
    ; channel = t.channel
    ; self_hosted_exposure = t.self_hosted_exposure
    ; manual_edd = t.manual_edd
    }
  ;;
end

module Edd_trigger = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Customer of Risk.Edd_trigger.Stable.V1.t
        | Opaque_ownership
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Customer of Risk.Edd_trigger.t
    | Opaque_ownership
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

let opaque (i : Inputs.t) =
  Option.some_if
    (Ownership.Opacity.equal i.ownership Opaque)
    Edd_trigger.Opaque_ownership
;;

let customer triggers = List.map triggers ~f:(fun t -> Edd_trigger.Customer t)

let edd_triggers i ~on =
  customer (Risk.edd_triggers (Inputs.to_customer i) ~on) @ Option.to_list (opaque i)
;;

let hard_floors i ~on =
  customer (Risk.hard_floors (Inputs.to_customer i) ~on) @ Option.to_list (opaque i)
;;

module Model = struct
  type t =
    { version : string
    ; customer : Risk.Model.t
    ; ownership : (Ownership.Opacity.t * int) list
    ; cash_intensive : int
    }
  [@@deriving compare, equal, sexp]

  (* Our weights. Opaque ownership is worth 40 like the customer model's hard-floor
     factors, and is a floor too; a complex structure alone stays Low but tips a business
     with other factors into Medium. *)
  let default =
    { version = "kyb-risk-2026-10"
    ; customer = Risk.Model.default
    ; ownership = [ Transparent, 0; Layered, 5; Complex, 15; Opaque, 40 ]
    ; cash_intensive = 15
    }
  ;;

  let points t (i : Inputs.t) ~on =
    Risk.Model.points t.customer (Inputs.to_customer i) ~on
    @ [ ( "ownership"
        , List.Assoc.find t.ownership i.ownership ~equal:Ownership.Opacity.equal
          |> Option.value ~default:0 )
      ; ("cash_intensive", if i.cash_intensive then t.cash_intensive else 0)
      ]
  ;;

  let score t i ~on =
    List.sum (module Int) (points t i ~on) ~f:snd |> Int.clamp_exn ~min:0 ~max:100
  ;;
end

let band_for ~score ~floors =
  if List.is_empty floors then Risk.Band.of_score score else Risk.Band.High
;;

module Assessment = struct
  module Stable = struct
    module V1 = struct
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

  let band t =
    match t.override with
    | Some o -> o.band
    | None -> t.computed_band
  ;;

  let check_override ~floors (o : Risk.Override.t) =
    if String.is_empty (String.strip o.reason)
    then Or_error.error_s [%message "a risk override needs a reason"]
    else if (not (List.is_empty floors)) && Risk.Band.( < ) o.band High
    then
      Or_error.error_s
        [%message
          "a hard floor keeps the band High"
            (floors : Edd_trigger.t list)
            ~requested:(o.band : Risk.Band.t)]
    else Ok ()
  ;;

  let validate t =
    if t.score < 0 || t.score > 100
    then Or_error.error_s [%message "risk score out of range" ~score:(t.score : int)]
    else if not (Risk.Band.equal t.computed_band (band_for ~score:t.score ~floors:t.floors))
    then
      Or_error.error_s
        [%message
          "computed band does not match score and floors"
            ~score:(t.score : int)
            ~floors:(t.floors : Edd_trigger.t list)
            ~computed_band:(t.computed_band : Risk.Band.t)]
    else (
      match t.override with
      | None -> Ok ()
      | Some o -> check_override ~floors:t.floors o)
  ;;
end

let assess (model : Model.t) inputs ~at : Assessment.t =
  let on = Time_ns.to_date at ~zone:Timezone.utc in
  let score = Model.score model inputs ~on in
  let floors = hard_floors inputs ~on in
  { inputs
  ; model_version = model.version
  ; score
  ; floors
  ; computed_band = band_for ~score ~floors
  ; override = None
  ; computed_at = at
  }
;;

let override (a : Assessment.t) ~band ~reason ~by ~at =
  let o : Risk.Override.t = { band; reason; by; at } in
  let%map.Or_error () = Assessment.check_override ~floors:a.floors o in
  { a with override = Some o }
;;
