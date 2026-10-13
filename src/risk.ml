open! Core

module Band = struct
  module Stable = struct
    module V1 = struct
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

  include Comparable.Make (Stable.V1)

  let of_score score = if Int.(score >= 60) then High else if Int.(score >= 30) then Medium else Low
end

module Country_risk = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Standard
        | Elevated
        | High_risk_third_country
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Standard
    | Elevated
    | High_risk_third_country
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Adverse_media = struct
  module Stable = struct
    module V1 = struct
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

module Product = struct
  module Stable = struct
    module V1 = struct
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

module Volume_band = struct
  module Stable = struct
    module V1 = struct
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

module Channel = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Face_to_face
        | Eid_high
        | Remote_attended
        | Remote_unattended
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Face_to_face
    | Eid_high
    | Remote_attended
    | Remote_unattended
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Edd_trigger = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Pep
        | Family_or_close_associate
        | High_risk_third_country
        | Unusual_transaction
        | Self_hosted_exposure
        | Adverse_media
        | Manual of string
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

module Inputs = struct
  module Stable = struct
    module V1 = struct
      type t =
        { sector : Band.Stable.V1.t
        ; pep : Pep.Stable.V1.t option
        ; adverse_media : Adverse_media.Stable.V1.t
        ; residence : Country_risk.Stable.V1.t
        ; nationality : Country_risk.Stable.V1.t
        ; products : Product.Stable.V1.t list
        ; monthly_volume : Volume_band.Stable.V1.t
        ; channel : Channel.Stable.V1.t
        ; self_hosted_exposure : bool
        ; manual_edd : string list
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

  let baseline =
    { sector = Low
    ; pep = None
    ; adverse_media = No_findings
    ; residence = Standard
    ; nationality = Standard
    ; products = [ Fiat_account ]
    ; monthly_volume = Up_to_1k
    ; channel = Face_to_face
    ; self_hosted_exposure = false
    ; manual_edd = []
    }
  ;;
end

let pep_trigger (i : Inputs.t) ~on : Edd_trigger.t option =
  match i.pep with
  | Some pep when Pep.edd_applies pep ~on ->
    (match pep.kind with
     | Pep -> Some Pep
     | Family_member | Close_associate -> Some Family_or_close_associate)
  | Some _ | None -> None
;;

let high_risk_country (i : Inputs.t) =
  List.exists [ i.residence; i.nationality ] ~f:(function
    | High_risk_third_country -> true
    | Standard | Elevated -> false)
;;

let hard_floors (i : Inputs.t) ~on =
  List.filter_opt
    [ pep_trigger i ~on
    ; Option.some_if (high_risk_country i) Edd_trigger.High_risk_third_country
    ; (match i.adverse_media with
       | Confirmed -> Some Edd_trigger.Adverse_media
       | No_findings | Unconfirmed -> None)
    ]
;;

let edd_triggers (i : Inputs.t) ~on =
  List.filter_opt
    [ pep_trigger i ~on
    ; Option.some_if (high_risk_country i) Edd_trigger.High_risk_third_country
    ; Option.some_if i.self_hosted_exposure Edd_trigger.Self_hosted_exposure
    ; (match i.adverse_media with
       | Confirmed -> Some Edd_trigger.Adverse_media
       | No_findings | Unconfirmed -> None)
    ]
  @ List.map i.manual_edd ~f:(fun reason -> Edd_trigger.Manual reason)
;;

module Model = struct
  type t =
    { version : string
    ; sector : (Band.t * int) list
    ; country : (Country_risk.t * int) list (** worst of residence and nationality *)
    ; adverse_media : (Adverse_media.t * int) list
    ; pep : int (** while PEP EDD applies *)
    ; product : (Product.t * int) list (** riskiest product *)
    ; volume : (Volume_band.t * int) list
    ; channel : (Channel.t * int) list
    ; self_hosted : int
    }
  [@@deriving compare, equal, sexp]

  (* Our weights. Any single hard-floor factor is worth 40, so it reaches
     Medium on points alone; the floor then lifts it to High. *)
  let default =
    { version = "kyc-risk-2026-10"
    ; sector = [ Low, 0; Medium, 15; High, 30 ]
    ; country = [ Standard, 0; Elevated, 20; High_risk_third_country, 40 ]
    ; adverse_media = [ No_findings, 0; Unconfirmed, 15; Confirmed, 40 ]
    ; pep = 40
    ; product = [ Fiat_account, 0; Card, 5; Crypto, 20; Lightning, 20 ]
    ; volume = [ Up_to_1k, 0; Up_to_10k, 5; Up_to_100k, 15; Above_100k, 25 ]
    ; channel = [ Face_to_face, 0; Eid_high, 0; Remote_attended, 5; Remote_unattended, 10 ]
    ; self_hosted = 15
    }
  ;;

  let version t = t.version

  let points t (i : Inputs.t) ~on =
    let find table key ~equal = List.Assoc.find table key ~equal |> Option.value ~default:0 in
    let worst table keys ~equal =
      List.fold keys ~init:0 ~f:(fun acc k -> Int.max acc (find table k ~equal))
    in
    [ "sector", find t.sector i.sector ~equal:Band.equal
    ; "country", worst t.country [ i.residence; i.nationality ] ~equal:Country_risk.equal
    ; "adverse_media", find t.adverse_media i.adverse_media ~equal:Adverse_media.equal
    ; "pep", (if Option.is_some (pep_trigger i ~on) then t.pep else 0)
    ; "product", worst t.product i.products ~equal:Product.equal
    ; "volume", find t.volume i.monthly_volume ~equal:Volume_band.equal
    ; "channel", find t.channel i.channel ~equal:Channel.equal
    ; "self_hosted", (if i.self_hosted_exposure then t.self_hosted else 0)
    ]
  ;;

  let score t i ~on =
    List.sum (module Int) (points t i ~on) ~f:snd |> Int.clamp_exn ~min:0 ~max:100
  ;;
end

module Override = struct
  module Stable = struct
    module V1 = struct
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

let band_for ~score ~floors =
  if List.is_empty floors then Band.of_score score else Band.High
;;

module Assessment = struct
  module Stable = struct
    module V1 = struct
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

  let band t =
    match t.override with
    | Some o -> o.band
    | None -> t.computed_band
  ;;

  let check_override ~floors (o : Override.t) =
    if String.is_empty (String.strip o.reason)
    then Or_error.error_s [%message "a risk override needs a reason"]
    else if (not (List.is_empty floors)) && Band.( < ) o.band High
    then
      Or_error.error_s
        [%message
          "a hard floor keeps the band High"
            (floors : Edd_trigger.t list)
            ~requested:(o.band : Band.t)]
    else Ok ()
  ;;

  let validate t =
    if t.score < 0 || t.score > 100
    then Or_error.error_s [%message "risk score out of range" ~score:(t.score : int)]
    else if not (Band.equal t.computed_band (band_for ~score:t.score ~floors:t.floors))
    then
      Or_error.error_s
        [%message
          "computed band does not match score and floors"
            ~score:(t.score : int)
            ~floors:(t.floors : Edd_trigger.t list)
            ~computed_band:(t.computed_band : Band.t)]
    else (
      match t.override with
      | None -> Ok ()
      | Some o -> check_override ~floors:t.floors o)
  ;;
end

let date_of at = Time_ns.to_date at ~zone:Timezone.utc

let assess model inputs ~at : Assessment.t =
  let on = date_of at in
  let score = Model.score model inputs ~on in
  let floors = hard_floors inputs ~on in
  { inputs
  ; model_version = Model.version model
  ; score
  ; floors
  ; computed_band = band_for ~score ~floors
  ; override = None
  ; computed_at = at
  }
;;

let override (a : Assessment.t) ~band ~reason ~by ~at =
  let o : Override.t = { band; reason; by; at } in
  let%map.Or_error () = Assessment.check_override ~floors:a.floors o in
  { a with override = Some o }
;;
