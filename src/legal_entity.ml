open! Core

module Field = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Legal_name
        | Registration_number
        | Jurisdiction
        | Legal_form
        | Incorporation_date
        | Registered_address
        | Business_address
        | Industry
        | Bearer_shares
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Legal_name
    | Registration_number
    | Jurisdiction
    | Legal_form
    | Incorporation_date
    | Registered_address
    | Business_address
    | Industry
    | Bearer_shares
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Stable = struct
  module V1 = struct
    type t =
      { legal_name : string
      ; registration_number : string option
      ; jurisdiction : Country.Stable.V1.t
      ; legal_form : Legal_form.Stable.V1.t
      ; incorporated_on : Date.Stable.V1.t option
      ; registered_address : Address.Stable.V1.t option
      ; business_address : Address.Stable.V1.t option
      ; industry : string option
      ; bearer_shares : bool
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { legal_name : string
  ; registration_number : string option
  ; jurisdiction : Country.t
  ; legal_form : Legal_form.t
  ; incorporated_on : Date.t option
  ; registered_address : Address.t option
  ; business_address : Address.t option
  ; industry : string option
  ; bearer_shares : bool
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

let create ~legal_name ~jurisdiction ~legal_form =
  { legal_name
  ; registration_number = None
  ; jurisdiction
  ; legal_form
  ; incorporated_on = None
  ; registered_address = None
  ; business_address = None
  ; industry = None
  ; bearer_shares = false
  }
;;

let validate t ~today =
  let errors =
    List.filter_opt
      [ (let n = t.legal_name in
         Option.some_if
           (String.is_empty n
            || String.length n > 200
            || (not (String.equal n (String.strip n)))
            (* Bytes above 0x7f are UTF-8 and allowed; only ASCII controls are refused. *)
            || String.exists n ~f:(fun c -> Char.to_int c < 0x20 || Char.to_int c = 0x7f)
           )
           [%message "invalid legal name"])
      ; Option.bind t.registration_number ~f:(fun r ->
          Option.some_if
            (String.is_empty (String.strip r))
            [%message "blank registration number"])
      ; Option.bind t.incorporated_on ~f:(fun d ->
          Option.some_if
            (Date.( > ) d today)
            [%message "incorporation date in the future" (d : Date.t)])
      ]
  in
  match errors with
  | [] -> Ok ()
  | errors -> Or_error.error_s [%message "invalid legal entity" (errors : Sexp.t list)]
;;

let changed_fields a b =
  let diff field eq get = Option.some_if (not (eq (get a) (get b))) field in
  List.filter_opt
    [ diff Field.Legal_name String.equal (fun t -> t.legal_name)
    ; diff Field.Registration_number [%equal: string option] (fun t ->
        t.registration_number)
    ; diff Field.Jurisdiction Country.equal (fun t -> t.jurisdiction)
    ; diff Field.Legal_form Legal_form.equal (fun t -> t.legal_form)
    ; diff Field.Incorporation_date [%equal: Date.t option] (fun t -> t.incorporated_on)
    ; diff Field.Registered_address [%equal: Address.t option] (fun t ->
        t.registered_address)
    ; diff Field.Business_address [%equal: Address.t option] (fun t -> t.business_address)
    ; diff Field.Industry [%equal: string option] (fun t -> t.industry)
    ; diff Field.Bearer_shares Bool.equal (fun t -> t.bearer_shares)
    ]
;;

let missing_fields t =
  List.filter_opt
    [ Option.some_if
        (Option.is_none t.registration_number
         && not (Legal_form.is_legal_arrangement t.legal_form))
        Field.Registration_number
    ; Option.some_if (Option.is_none t.incorporated_on) Field.Incorporation_date
    ; Option.some_if (Option.is_none t.registered_address) Field.Registered_address
    ; Option.some_if (Option.is_none t.industry) Field.Industry
    ]
;;
