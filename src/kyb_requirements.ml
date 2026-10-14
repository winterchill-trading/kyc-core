open! Core
module Kind = Kyb_document.Kind

module Limit = struct
  type t =
    { currency : string
    ; amount_minor : int
    }
  [@@deriving compare, equal, sexp]
end

module Jurisdiction = struct
  type t =
    { country : Country.t
    ; covered : bool
    ; legal_forms : Legal_form.t list
    ; extra_documents : Kind.t list
    ; monthly_limit : Limit.t option
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { by_legal_form : (Legal_form.t * Kind.t list) list
  ; other_forms : Kind.t list
  ; layered_ownership : Kind.t list
  ; per_representative : Kind.t list
  ; jurisdictions : Jurisdiction.t list
  ; unlisted_covered : bool
  }
[@@deriving compare, equal, sexp]

let default =
  let company register =
    [ Kind.Certificate_of_incorporation
    ; Articles_of_association
    ; register
    ; Register_of_directors
    ]
  in
  { by_legal_form =
      [ Sole_trader, [ Register_extract ]
      ; General_partnership, [ Register_extract; Partnership_agreement ]
      ; Limited_partnership, [ Register_extract; Partnership_agreement ]
      ; Private_limited_company, company Shareholder_register
      ; Public_limited_company, company Register_extract
      ; Cooperative, [ Register_extract; Articles_of_association ]
      ; Association, [ Register_extract; Articles_of_association ]
      ; Foundation, [ Register_extract; Foundation_charter ]
      ; Trust, [ Trust_deed ]
      ]
  ; other_forms = [ Register_extract; Articles_of_association ]
  ; layered_ownership = [ Ownership_chart ]
  ; per_representative = [ Power_of_attorney ]
  ; jurisdictions = []
  ; unlisted_covered = true
  }
;;

let jurisdiction t country =
  match List.find t.jurisdictions ~f:(fun j -> Country.equal j.country country) with
  | Some j -> j
  | None ->
    { country
    ; covered = t.unlisted_covered
    ; legal_forms = []
    ; extra_documents = []
    ; monthly_limit = None
    }
;;

let check_entity t (entity : Legal_entity.t) =
  let j = jurisdiction t entity.jurisdiction in
  if not j.covered
  then
    Or_error.error_s
      [%message
        "jurisdiction not covered" ~jurisdiction:(entity.jurisdiction : Country.t)]
  else if (not (List.is_empty j.legal_forms))
          && not (List.mem j.legal_forms entity.legal_form ~equal:Legal_form.equal)
  then
    Or_error.error_s
      [%message
        "legal form not accepted in this jurisdiction"
          ~jurisdiction:(entity.jurisdiction : Country.t)
          ~legal_form:(entity.legal_form : Legal_form.t)]
  else Ok ()
;;

let required t (entity : Legal_entity.t) (parties : Kyb_party.t list) =
  let entity_kinds =
    (match List.Assoc.find t.by_legal_form entity.legal_form ~equal:Legal_form.equal with
     | Some kinds -> kinds
     | None -> t.other_forms)
    @ (jurisdiction t entity.jurisdiction).extra_documents
    @
    if List.exists parties ~f:(fun p ->
         List.exists p.chains ~f:(fun c -> Ownership.Chain.layers c > 0))
    then t.layered_ownership
    else []
  in
  let representatives =
    List.filter parties ~f:(fun p ->
      Kyb_party.has_role p Authorised_representative
      && not (Kyb_party.has_role p Director))
  in
  List.map entity_kinds ~f:(fun kind -> { Kyb_document.Requirement.kind; party = None })
  @ List.concat_map representatives ~f:(fun p ->
    List.map t.per_representative ~f:(fun kind ->
      { Kyb_document.Requirement.kind; party = Some p.id }))
  |> List.dedup_and_sort ~compare:Kyb_document.Requirement.compare
;;
