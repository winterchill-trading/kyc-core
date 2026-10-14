open! Core
open Kyc_core

let entity_id = Entity_id.of_string "ent-1"
let op_a = Actor.Operator (Operator_id.of_string "op-a")
let op_b = Actor.Operator (Operator_id.of_string "op-b")
let provider = Actor.Provider "provider-a"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let counter = ref 0
let reset () = counter := 0

type draft = int -> Kyb_event.t

let number (draft : draft) =
  incr counter;
  draft !counter
;;

let time n = Time_ns.add t0 (Time_ns.Span.of_int_sec (60 * n))

let event ?(actor = Actor.System) ?note ?id kind n : Kyb_event.t =
  let id = Option.value id ~default:(sprintf "kyb-%d" n) |> Event_id.of_string in
  { id; entity_id; at = time n; actor; note; kind }
;;

let status ?(actor = op_a) ?note ?round transition =
  event ~actor ?note (Status_changed { transition; round })
;;

let party_case id status =
  event (Party_case_changed { party = Customer_id.of_string id; status })
;;

let assessed ?(inputs = Kyb_risk.Inputs.baseline) () n =
  event
    ~actor:op_a
    (Risk_assessed (Kyb_risk.assess Kyb_risk.Model.default inputs ~at:(time n)))
    n
;;

let gb = Country.of_string "GB"

let entity : Legal_entity.t =
  { legal_name = "Example Trading Ltd"
  ; registration_number = Some "01234567"
  ; jurisdiction = gb
  ; legal_form = Private_limited_company
  ; incorporated_on = Some (Date.of_string "2019-04-01")
  ; registered_address =
      Some
        { line1 = "1 Example Street"
        ; line2 = None
        ; city = "London"
        ; region = None
        ; postal_code = Some "EC1A 1AA"
        ; country = gb
        }
  ; business_address = None
  ; industry = Some "62.01"
  ; bearer_shares = false
  }
;;

let parties : Kyb_party.t list =
  [ { id = Customer_id.of_string "alice"
    ; roles = [ Director; Shareholder ]
    ; chains = [ Ownership.Chain.direct ~bps:6000 ]
    }
  ; { id = Customer_id.of_string "bob"
    ; roles = [ Shareholder ]
    ; chains =
        [ { stake_bps = 10_000
          ; via =
              [ { name = "Example Holdings SARL"
                ; jurisdiction = Country.of_string "LU"
                ; legal_form = Private_limited_company
                ; registration_number = Some "B000001"
                ; stake_bps = 4000
                ; nominee = false
                ; bearer_shares = false
                }
              ]
          }
        ]
    }
  ; { id = Customer_id.of_string "carol"
    ; roles = [ Authorised_representative ]
    ; chains = []
    }
  ]
;;

let document ?party id kind =
  event
    (Document_added
       { id = Document_id.of_string id
       ; kind
       ; party = Option.map party ~f:Customer_id.of_string
       ; file_name = id ^ ".pdf"
       ; content_type = "application/pdf"
       ; size_bytes = 120_000
       ; sha256 = None
       })
;;

let all_documents =
  [ document "doc-coi" Certificate_of_incorporation
  ; document "doc-aoa" Articles_of_association
  ; document "doc-reg" Shareholder_register
  ; document "doc-dir" Register_of_directors
  ; document "doc-chart" Ownership_chart
  ; document ~party:"carol" "doc-poa" Power_of_attorney
  ]
;;

let source_of_funds : Source_of_funds.t =
  { sources = [ Operating_revenue ]
  ; description = "Software licences sold to business customers"
  ; monthly_volume = Up_to_100k
  ; counterparty_countries = [ gb ]
  }
;;

let apply_all case drafts =
  List.fold drafts ~init:case ~f:(fun case draft ->
    match Kyb_case.apply case (number draft) with
    | Ok case -> case
    | Error err ->
      print_s [%sexp (err : Error.t)];
      case)
;;

let opened () =
  reset ();
  apply_all
    (Kyb_case.create entity_id)
    [ event
        (Opened
           (Legal_entity.create
              ~legal_name:entity.legal_name
              ~jurisdiction:entity.jurisdiction
              ~legal_form:entity.legal_form))
    ]
;;

let complete () =
  apply_all
    (opened ())
    ([ event (Entity_updated entity); event (Parties_set parties) ]
     @ all_documents
     @ [ event (Source_of_funds_declared source_of_funds) ])
;;

let ready_for_approval ?inputs () =
  apply_all
    (complete ())
    [ status Submit
    ; party_case "alice" Approved
    ; party_case "bob" Approved
    ; party_case "carol" Approved
    ; assessed ?inputs ()
    ]
;;

let show_case case =
  print_s
    [%message
      ""
        ~status:(Kyb_case.status case : Kyb_status.t)
        ~round:(Kyb_case.round case : int)
        ~band:(Kyb_case.band case : Risk.Band.t option)
        ~missing:(Kyb_case.missing_for_submission case : Kyb_item.t list)
        ~parties_pending:(Kyb_case.parties_pending case : Customer_id.t list)]
;;

let show_follow_ups case ~now =
  print_s
    [%sexp
      (Kyb_case.follow_ups case ~now:(Time_ns.of_string_with_utc_offset now)
       : Kyb_case.Follow_up.t list)]
;;
