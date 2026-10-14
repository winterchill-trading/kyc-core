open! Core

module Policy = struct
  type t =
    { requirements : Kyb_requirements.t
    ; owner_threshold_bps : int
    ; require_risk_assessment : bool
    ; edd_requires_senior_approval : bool
    ; review_intervals : Kyc_case.Policy.Review_intervals.t
    ; retention : Time_ns.Span.t
    }
  [@@deriving compare, equal, sexp]

  let default =
    { requirements = Kyb_requirements.default
    ; owner_threshold_bps = 2500
    ; require_risk_assessment = true
    ; edd_requires_senior_approval = true
    ; review_intervals = Kyc_case.Policy.default.review_intervals
    ; retention = Kyc_case.Policy.default.retention
    }
  ;;
end

module Decision = struct
  type outcome =
    | Approved
    | More_information of Kyb_item.t list
    | Rejected of string
  [@@deriving compare, equal, sexp]

  type t =
    { round : int
    ; outcome : outcome
    ; at : Timestamp.t
    ; actor : Actor.t
    ; note : string option
    }
  [@@deriving compare, equal, sexp]
end

module Follow_up = struct
  type t =
    | Missing of Kyb_item.t list
    | Awaiting_decision of
        { round : int
        ; since : Timestamp.t
        }
    | Party_cases_pending of Customer_id.t list
    | Risk_assessment_needed
    | Edd_outstanding of
        { triggers : Kyb_risk.Edd_trigger.t list
        ; senior_approval : bool
        }
    | Periodic_review_due of Timestamp.t
    | Retention_ended of Timestamp.t
  [@@deriving compare, equal, sexp]
end

module Applicant_view = struct
  type t =
    { status : Kyc_case.Customer_view.status
    ; round : int
    ; requested : Kyb_item.t list
    ; missing : Kyb_item.t list
    ; parties_pending : Customer_id.t list
    ; reason : string option
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { entity_id : Entity_id.t
  ; policy : Policy.t
  ; entity : Legal_entity.t option
  ; parties : Kyb_party.t list
  ; party_statuses : Status.t Customer_id.Map.t
  ; documents : Kyb_document.t Document_id.Map.t
  ; source_of_funds : Source_of_funds.t option
  ; status : Kyb_status.t
  ; round : int
  ; submitted_at : Timestamp.t option
  ; decisions : Decision.t list (* newest first *)
  ; risk : Kyb_risk.Assessment.t option
  ; senior_approval_at : Timestamp.t option
  ; last_review_at : Timestamp.t option
  ; closed_at : Timestamp.t option
  ; refused_at : Timestamp.t option
  ; applied : Kyb_event.t Event_id.Map.t
  ; audit : Kyb_audit.t
  }
[@@deriving compare, equal, sexp_of, fields ~getters]

let create ?(policy = Policy.default) entity_id =
  { entity_id
  ; policy
  ; entity = None
  ; parties = []
  ; party_statuses = Customer_id.Map.empty
  ; documents = Document_id.Map.empty
  ; source_of_funds = None
  ; status = Draft
  ; round = 0
  ; submitted_at = None
  ; decisions = []
  ; risk = None
  ; senior_approval_at = None
  ; last_review_at = None
  ; closed_at = None
  ; refused_at = None
  ; applied = Event_id.Map.empty
  ; audit = Kyb_audit.empty
  }
;;

let party_status t id = Map.find t.party_statuses id
let documents t = Map.data t.documents
let decisions t = List.rev t.decisions
let has_applied t id = Map.mem t.applied id
let band t = Option.map t.risk ~f:Kyb_risk.Assessment.band
let date_of ts = Time_ns.to_date ts ~zone:Timezone.utc

let legal_form t =
  match t.entity with
  | Some e -> e.legal_form
  | None -> Other "unknown"
;;

let beneficial_owners t =
  Kyb_party.beneficial_owners
    (legal_form t)
    t.parties
    ~threshold_bps:t.policy.owner_threshold_bps
;;

let required_documents t =
  match t.entity with
  | None -> []
  | Some e -> Kyb_requirements.required t.policy.requirements e t.parties
;;

let missing_documents t =
  List.filter (required_documents t) ~f:(fun r ->
    not
      (List.exists (documents t) ~f:(fun document ->
         Kyb_document.Requirement.met_by r ~document)))
;;

(* A trust is run by its trustees; everything else by its directors (for a sole trader,
   the proprietor is recorded as the director). *)
let has_management t =
  let role : Kyb_party.Role.t =
    if Legal_form.is_legal_arrangement (legal_form t) then Trustee else Director
  in
  List.exists t.parties ~f:(fun p -> Kyb_party.has_role p role)
;;

let missing_for_submission t =
  match t.entity with
  | None -> []
  | Some e ->
    List.map (Legal_entity.missing_fields e) ~f:(fun f -> Kyb_item.Entity_field f)
    @ List.filter_opt
        [ Option.some_if (not (has_management t)) Kyb_item.Parties
        ; Option.some_if (List.is_empty (beneficial_owners t)) Kyb_item.Beneficial_owners
        ]
    @ List.map (missing_documents t) ~f:(fun r -> Kyb_item.Document r)
    @ Option.to_list (Option.some_if (Option.is_none t.source_of_funds) Kyb_item.Source_of_funds)
;;

let parties_pending t =
  Kyb_party.needs_approved_case
    (legal_form t)
    t.parties
    ~threshold_bps:t.policy.owner_threshold_bps
  |> List.filter ~f:(fun id ->
    match party_status t id with
    | Some Approved -> false
    | Some _ | None -> true)
;;

let edd_triggers t ~now =
  match t.risk with
  | None -> []
  | Some r -> Kyb_risk.edd_triggers r.inputs ~on:(date_of now)
;;

let cdd_level t ~now : Cdd_level.t =
  match band t, edd_triggers t ~now with
  | Some High, _ | _, _ :: _ -> Enhanced
  | (None | Some (Low | Medium)), [] -> Standard
;;

let senior_approval_current t =
  match t.senior_approval_at, t.risk with
  | Some approved, Some r -> Time_ns.( >= ) approved r.computed_at
  | None, _ | _, None -> false
;;

let edd_outstanding t ~now =
  match cdd_level t ~now with
  | Simplified | Standard -> None
  | Enhanced ->
    if t.policy.edd_requires_senior_approval && not (senior_approval_current t)
    then
      Some
        (Follow_up.Edd_outstanding
           { triggers = edd_triggers t ~now; senior_approval = true })
    else None
;;

let can_approve t ~now =
  match missing_for_submission t, parties_pending t with
  | (_ :: _ as missing), _ ->
    Or_error.error_s [%message "KYB case incomplete" (missing : Kyb_item.t list)]
  | [], (_ :: _ as pending) ->
    Or_error.error_s
      [%message "linked people not verified" (pending : Customer_id.t list)]
  | [], [] ->
    if t.policy.require_risk_assessment && Option.is_none t.risk
    then Or_error.error_s [%message "no risk assessment recorded"]
    else (
      match edd_outstanding t ~now with
      | Some edd ->
        Or_error.error_s
          [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
      | None -> Ok ())
;;

let next_review_due t =
  match t.status, t.last_review_at, band t with
  | Approved, Some last, Some band ->
    Some
      (Time_ns.add
         last
         (Kyc_case.Policy.Review_intervals.for_band t.policy.review_intervals band))
  | _ -> None
;;

let retain_until t =
  Retention.retain_until
    ~ended:t.closed_at
    ~refused:t.refused_at
    ~retention:t.policy.retention
    ~hold:None
;;

let requested t =
  match t.status with
  | More_information items -> items
  | Draft | Submitted | In_review | Approved | Rejected _ | Blocked _ | Closed _ -> []
;;

let follow_ups t ~now =
  let live =
    match t.status with
    | Closed _ | Rejected _ -> false
    | Draft | Submitted | In_review | More_information _ | Approved | Blocked _ -> true
  in
  let list_opt f = function
    | [] -> None
    | xs -> Some (f xs)
  in
  List.filter_opt
    [ (if Kyb_status.is_editable t.status
       then
         list_opt
           (fun items -> Follow_up.Missing items)
           (missing_for_submission t
            @ List.filter (requested t) ~f:(fun i ->
              not (List.mem (missing_for_submission t) i ~equal:Kyb_item.equal)))
       else None)
    ; (match t.status, t.submitted_at with
       | (Submitted | In_review), Some since ->
         Some (Follow_up.Awaiting_decision { round = t.round; since })
       | _ -> None)
    ; (if live && t.round > 0
       then list_opt (fun ids -> Follow_up.Party_cases_pending ids) (parties_pending t)
       else None)
    ; Option.some_if
        (live && t.round > 0 && Option.is_none t.risk)
        Follow_up.Risk_assessment_needed
    ; (if live && t.round > 0 then edd_outstanding t ~now else None)
    ; Option.bind (next_review_due t) ~f:(fun due ->
        Option.some_if (Time_ns.( <= ) due now) (Follow_up.Periodic_review_due due))
    ; Option.bind (retain_until t) ~f:(fun until ->
        Option.some_if (Time_ns.( <= ) until now) (Follow_up.Retention_ended until))
    ]
;;

let applicant_view t : Applicant_view.t =
  let missing = missing_for_submission t in
  let status : Kyc_case.Customer_view.status =
    match t.status with
    | Draft -> if List.is_empty missing then In_progress else Action_required
    | More_information _ -> Action_required
    | Submitted | In_review -> Under_review
    | Approved -> Active
    (* Never say why: a block may come from a sanctions match or a report. *)
    | Blocked _ -> Under_review
    | Rejected _ -> Declined
    | Closed _ -> Closed
  in
  let blocked = Kyb_status.(function Blocked _ -> true | _ -> false) t.status in
  { status
  ; round = t.round
  ; requested = (if blocked then [] else requested t)
  ; missing = (if Kyb_status.is_editable t.status then missing else [])
  ; parties_pending =
      (if blocked
       then []
       else
         (* A person whose own case is blocked is not named: that would tip them off
            through the business. *)
         List.filter (parties_pending t) ~f:(fun id ->
           match party_status t id with
           | Some (Blocked _) -> false
           | _ -> true))
  ; reason =
      (match t.status with
       | Rejected reason -> Some reason
       | _ -> None)
  }
;;

let need_operator (actor : Actor.t) ~what =
  match actor with
  | Operator id -> Ok id
  | System | Provider _ ->
    Or_error.error_s
      [%message "only an operator may do this" (what : string) (actor : Actor.t)]
;;

let need_editable t ~what =
  if Kyb_status.is_editable t.status
  then Ok ()
  else
    Or_error.error_s
      [%message
        "the case cannot be changed now" (what : string) ~status:(t.status : Kyb_status.t)]
;;

let check_round t (transition : Kyb_status.Transition.t) round =
  match round, transition with
  | None, _ -> Ok ()
  | Some r, Submit when r = t.round + 1 -> Ok ()
  | Some r, Submit ->
    Or_error.error_s
      [%message "a submission opens the next round" ~round:(r : int) ~current:(t.round : int)]
  | Some r, _ when r = t.round -> Ok ()
  | Some r, _ ->
    Or_error.error_s
      [%message
        "event for another submission round" ~round:(r : int) ~current:(t.round : int)]
;;

let step_event t (event : Kyb_event.t) : (t * Kyb_audit.Change.t list) Or_error.t =
  let open Or_error.Let_syntax in
  let one (t, change) = t, [ change ] in
  let today = date_of event.at in
  match event.kind, t.entity, t.status with
  | Opened entity, None, _ ->
    let%bind () = Legal_entity.validate entity ~today in
    let%map () = Kyb_requirements.check_entity t.policy.requirements entity in
    one ({ t with entity = Some entity }, Kyb_audit.Change.Opened)
  | Opened _, Some _, _ -> Or_error.error_s [%message "case already opened"]
  | _, None, _ -> Or_error.error_s [%message "case not opened yet"]
  | Status_changed { transition; round }, Some _, _ ->
    let%bind () = check_round t transition round in
    (* Status first, so a decision on a closed case reports the status. *)
    let%bind to_ = Kyb_status.apply t.status transition ~actor:event.actor in
    let%map () =
      match transition with
      | Submit ->
        (match missing_for_submission t with
         | [] -> Ok ()
         | missing ->
           Or_error.error_s [%message "KYB case incomplete" (missing : Kyb_item.t list)])
      | Approve -> can_approve t ~now:event.at
      | Start_review | Request_information _ | Reject _ | Block _ | Unblock | Close _ ->
        Ok ()
    in
    let decision outcome =
      { Decision.round = t.round; outcome; at = event.at; actor = event.actor; note = event.note }
      :: t.decisions
    in
    let t' =
      match transition with
      | Submit -> { t with round = t.round + 1; submitted_at = Some event.at }
      | Approve ->
        { t with last_review_at = Some event.at; decisions = decision Approved }
      | Request_information items -> { t with decisions = decision (More_information items) }
      | Reject reason ->
        { t with refused_at = Some event.at; decisions = decision (Rejected reason) }
      | Close _ -> { t with closed_at = Some event.at }
      | Start_review | Block _ | Unblock -> t
    in
    one
      ( { t' with status = to_ }
      , Kyb_audit.Change.Status { from = t.status; to_; round = t'.round } )
  (* Everything else is frozen once the relationship has ended. *)
  | _, Some _, Closed _ ->
    Or_error.error_s [%message "case is closed" ~status:(t.status : Kyb_status.t)]
  | Entity_updated entity, Some old, _ ->
    let%bind () = need_editable t ~what:"update the entity" in
    let%bind () = Legal_entity.validate entity ~today in
    let%bind () = Kyb_requirements.check_entity t.policy.requirements entity in
    (match Legal_entity.changed_fields old entity with
     | [] -> Or_error.error_s [%message "entity update changes nothing"]
     | fields ->
       if t.round > 0 && List.mem fields Jurisdiction ~equal:Legal_entity.Field.equal
       then
         Or_error.error_s [%message "the jurisdiction is fixed after the first submission"]
       else Ok (one ({ t with entity = Some entity }, Kyb_audit.Change.Entity_updated fields)))
  | Parties_set parties, Some _, _ ->
    let%bind () = need_editable t ~what:"change the parties" in
    let%bind () = Kyb_party.validate_all parties in
    if [%equal: Kyb_party.t list] parties t.parties
    then Or_error.error_s [%message "parties unchanged"]
    else (
      let ids = List.map parties ~f:(fun p -> p.id) in
      let keep = Customer_id.Set.of_list ids in
      Ok
        (one
           ( { t with
               parties
             ; party_statuses = Map.filter_keys t.party_statuses ~f:(Set.mem keep)
             }
           , Kyb_audit.Change.Parties_set ids )))
  | Party_case_changed { party; status }, Some _, _ ->
    if not (List.exists t.parties ~f:(fun p -> Customer_id.equal p.id party))
    then Or_error.error_s [%message "not a party to this case" (party : Customer_id.t)]
    else if [%equal: Status.t option] (party_status t party) (Some status)
    then Or_error.error_s [%message "party case status unchanged"]
    else
      Ok
        (one
           ( { t with party_statuses = Map.set t.party_statuses ~key:party ~data:status }
           , Kyb_audit.Change.Party_case_changed { party; status } ))
  | Document_added document, Some _, _ ->
    let%bind () = need_editable t ~what:"add a document" in
    let%bind () = Kyb_document.validate document in
    if Map.mem t.documents document.id
    then Or_error.error_s [%message "document already recorded" ~id:(document.id : Document_id.t)]
    else if Option.exists document.party ~f:(fun id ->
              not (List.exists t.parties ~f:(fun p -> Customer_id.equal p.id id)))
    then Or_error.error_s [%message "document about someone who is not a party"]
    else
      Ok
        (one
           ( { t with documents = Map.set t.documents ~key:document.id ~data:document }
           , Kyb_audit.Change.Document_added
               { document_id = document.id; kind = document.kind; party = document.party } ))
  | Document_removed id, Some _, _ ->
    let%bind () = need_editable t ~what:"remove a document" in
    if not (Map.mem t.documents id)
    then Or_error.error_s [%message "unknown document" (id : Document_id.t)]
    else
      Ok
        (one
           ( { t with documents = Map.remove t.documents id }
           , Kyb_audit.Change.Document_removed id ))
  | Source_of_funds_declared sof, Some _, _ ->
    let%bind () = need_editable t ~what:"declare the source of funds" in
    let%bind () = Source_of_funds.validate sof in
    if [%equal: Source_of_funds.t option] t.source_of_funds (Some sof)
    then Or_error.error_s [%message "source of funds unchanged"]
    else
      Ok
        (one
           ( { t with source_of_funds = Some sof }
           , Kyb_audit.Change.Source_of_funds_declared ))
  | Risk_assessed assessment, Some _, _ ->
    let%bind () = Kyb_risk.Assessment.validate assessment in
    let%map () =
      match assessment.override, event.actor with
      | None, _ -> Ok ()
      | Some o, Operator id when Operator_id.equal o.by id -> Ok ()
      | Some _, _ ->
        Or_error.error_s
          [%message "a risk override must be recorded by the operator who made it"]
    in
    one
      ( { t with risk = Some assessment }
      , Kyb_audit.Change.Risk_assessed
          { score = assessment.score
          ; computed_band = assessment.computed_band
          ; band = Kyb_risk.Assessment.band assessment
          ; model_version = assessment.model_version
          } )
  | Senior_approval, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"give senior approval" in
    let%bind () =
      match event.note with
      | Some note when not (String.is_empty (String.strip note)) -> Ok ()
      | _ -> Or_error.error_s [%message "senior approval needs a rationale in the note"]
    in
    (match cdd_level t ~now:event.at with
     | Enhanced ->
       Ok
         (one
            ( { t with senior_approval_at = Some event.at }
            , Kyb_audit.Change.Senior_approval ))
     | Simplified | Standard ->
       Or_error.error_s [%message "no enhanced due diligence applies"])
  | Periodic_review_completed, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"complete a periodic review" in
    let%bind () =
      match t.status with
      | Approved -> Ok ()
      | _ ->
        Or_error.error_s
          [%message "only an approved case is reviewed" ~status:(t.status : Kyb_status.t)]
    in
    let%bind () =
      match t.risk, t.last_review_at with
      | Some r, Some last when Time_ns.( >= ) r.computed_at last -> Ok ()
      | _ ->
        Or_error.error_s
          [%message "a review needs a risk assessment since the last review"]
    in
    let%bind () =
      match parties_pending t with
      | [] -> Ok ()
      | pending ->
        Or_error.error_s
          [%message "linked people not verified" (pending : Customer_id.t list)]
    in
    let%map () =
      match edd_outstanding t ~now:event.at with
      | None -> Ok ()
      | Some edd ->
        Or_error.error_s
          [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
    in
    one
      ( { t with last_review_at = Some event.at }
      , Kyb_audit.Change.Periodic_review_completed )
;;

let apply t (event : Kyb_event.t) =
  match Map.find t.applied event.id with
  | Some seen when Kyb_event.equal seen event -> Ok t
  | Some _ ->
    Or_error.error_s
      [%message
        "event id already applied with different content" ~id:(event.id : Event_id.t)]
  | None ->
    if not (Entity_id.equal event.entity_id t.entity_id)
    then
      Or_error.error_s
        [%message
          "event is for another entity"
            ~case:(t.entity_id : Entity_id.t)
            ~event:(event.entity_id : Entity_id.t)]
    else (
      match step_event t event with
      | Error e -> Error (Error.tag_s e ~tag:[%message "" ~event:(event.id : Event_id.t)])
      | Ok (t, changes) ->
        let audit =
          List.fold changes ~init:t.audit ~f:(fun audit change ->
            Kyb_audit.add
              audit
              { at = event.at
              ; actor = event.actor
              ; event_id = event.id
              ; change
              ; why = event.note
              })
        in
        Ok { t with applied = Map.set t.applied ~key:event.id ~data:event; audit })
;;

let of_events ?policy entity_id events =
  List.fold_result events ~init:(create ?policy entity_id) ~f:apply
;;
