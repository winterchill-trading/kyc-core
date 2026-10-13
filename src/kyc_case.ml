open! Core
module Step = Verification.Step
module State = Verification.State
module Alert_id = Monitoring.Alert_id
module Investigation_id = Investigation.Investigation_id
module Request_id = Investigation.Request_id

module Policy = struct
  module Review_intervals = struct
    type t =
      { low : Time_ns.Span.t
      ; medium : Time_ns.Span.t
      ; high : Time_ns.Span.t
      }
    [@@deriving compare, equal, sexp]

    let for_band t : Risk.Band.t -> _ = function
      | Low -> t.low
      | Medium -> t.medium
      | High -> t.high
    ;;
  end

  type t =
    { rules : Tier.Rules.t
    ; approval_tier : Tier.t
    ; edd_tier : Tier.t
    ; require_screening : bool
    ; require_risk_assessment : bool
    ; edd_requires_senior_approval : bool
    ; allow_simplified : bool
    ; review_intervals : Review_intervals.t
    ; retention : Time_ns.Span.t
    }
  [@@deriving compare, equal, sexp]

  (* Calendar years would need a zone and leap-day rules; 365-day years are
     slightly early, which is the safe side for reviews and close enough for
     retention given that erasure is a separate, audited job. *)
  let years n = Time_ns.Span.of_day (Float.of_int (365 * n))

  let default =
    { rules = Tier.Rules.default
    ; approval_tier = T2
    ; edd_tier = T3
    ; require_screening = true
    ; require_risk_assessment = true
    ; edd_requires_senior_approval = true
    ; allow_simplified = true
    ; review_intervals = { low = years 5; medium = years 3; high = years 1 }
    ; retention = years 5
    }
  ;;
end

module Follow_up = struct
  type t =
    | Risk_assessment_needed
    | Edd_outstanding of
        { triggers : Risk.Edd_trigger.t list
        ; missing_steps : Step.Set.t
        ; senior_approval : bool
        }
    | Screening_review of Screening_id.t list
    | Open_alerts of Alert_id.t list
    | Open_investigations of Investigation_id.t list
    | Fiu_request_due of (Request_id.t * Date.t) list
    | Periodic_review_due of Timestamp.t
    | Steps_lapsed of Step.t list
    | Retention_ended of Timestamp.t
  [@@deriving compare, equal, sexp]
end

module Customer_view = struct
  type status =
    | In_progress
    | Action_required
    | Under_review
    | Active
    | Declined
    | Closed
  [@@deriving compare, equal, sexp]

  type t =
    { status : status
    ; tier : Tier.t
    ; steps_to_complete : Step.t list
    ; fields_to_resubmit : Person.Field.t list
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { customer_id : Customer_id.t
  ; policy : Policy.t
  ; person : Person.t option
  ; status : Status.t
  ; steps : State.t Step.Map.t
  ; screenings : Screening.t Screening_id.Map.t
  ; risk : Risk.Assessment.t option
  ; senior_approval_at : Timestamp.t option
  ; alerts : Monitoring.Alert.t Alert_id.Map.t
  ; investigations : Investigation.t Investigation_id.Map.t
  ; fiu_requests : Investigation.Fiu_request.t Request_id.Map.t
  ; last_review_at : Timestamp.t option
  ; closed_at : Timestamp.t option
  ; refused_at : Timestamp.t option
  ; legal_hold : Retention.Legal_hold.t option
  ; applied : Event.t Event_id.Map.t
  ; audit : Audit.t
  }
[@@deriving compare, equal, sexp_of, fields ~getters]

let create ?(policy = Policy.default) customer_id =
  { customer_id
  ; policy
  ; person = None
  ; status = Created
  ; steps = Step.Map.of_alist_exn (List.map Step.all ~f:(fun step -> step, State.Pending))
  ; screenings = Screening_id.Map.empty
  ; risk = None
  ; senior_approval_at = None
  ; alerts = Alert_id.Map.empty
  ; investigations = Investigation_id.Map.empty
  ; fiu_requests = Request_id.Map.empty
  ; last_review_at = None
  ; closed_at = None
  ; refused_at = None
  ; legal_hold = None
  ; applied = Event_id.Map.empty
  ; audit = Audit.empty
  }
;;

let step t step = Map.find_exn t.steps step
let steps t = Map.to_alist t.steps
let screenings t = Map.data t.screenings
let alerts t = Map.data t.alerts
let investigations t = Map.data t.investigations
let fiu_requests t = Map.data t.fiu_requests
let has_applied t id = Map.mem t.applied id
let band t = Option.map t.risk ~f:Risk.Assessment.band
let verified_steps t = Map.filter t.steps ~f:State.is_verified |> Map.key_set
let verified_tier t = Tier.Rules.tier_of t.policy.rules ~verified:(verified_steps t)

let tier t =
  match t.status with
  | Rejected _ | Blocked _ | Closed _ -> Tier.T0
  | Approved -> verified_tier t
  | Created | Verifying | Needs_resubmission _ -> Tier.min (verified_tier t) T1
;;

let required_for t tier = Tier.Rules.required_for t.policy.rules tier
let missing_for t tier = Set.diff (required_for t tier) (verified_steps t)

let missing_steps t =
  match List.find Tier.all ~f:(fun tier -> Tier.( > ) tier (verified_tier t)) with
  | Some next -> missing_for t next
  | None -> Step.Set.empty
;;

let date_of ts = Time_ns.to_date ts ~zone:Timezone.utc

let edd_triggers t ~now =
  let from_risk =
    match t.risk with
    | None -> []
    | Some r -> Risk.edd_triggers r.inputs ~on:(date_of now)
  in
  let unusual =
    List.exists (alerts t) ~f:(fun a ->
      match a.source, a.state with
      | Transaction_monitoring, Escalated _ -> true
      | _ -> false)
  in
  from_risk @ if unusual then [ Risk.Edd_trigger.Unusual_transaction ] else []
;;

let cdd_level t ~now =
  match t.risk with
  | None -> Cdd_level.Standard
  | Some r ->
    Cdd_level.decide
      ~band:(Risk.Assessment.band r)
      ~triggers:(edd_triggers t ~now)
      ~products:r.inputs.products
      ~allow_simplified:t.policy.allow_simplified
;;

(* A sign-off covers the assessment it was given for; a new assessment needs
   a new one. *)
let senior_approval_current t =
  match t.senior_approval_at, t.risk with
  | Some approved, Some r -> Time_ns.( >= ) approved r.computed_at
  | None, _ | _, None -> false
;;

(* [None] when EDD does not apply or is complete. *)
let edd_outstanding t ~now =
  match cdd_level t ~now with
  | Simplified | Standard -> None
  | Enhanced ->
    let missing_steps = missing_for t t.policy.edd_tier in
    let senior_approval =
      t.policy.edd_requires_senior_approval && not (senior_approval_current t)
    in
    if Set.is_empty missing_steps && not senior_approval
    then None
    else
      Some
        (Follow_up.Edd_outstanding
           { triggers = edd_triggers t ~now; missing_steps; senior_approval })
;;

let can_approve t ~now =
  let missing = missing_for t t.policy.approval_tier in
  let blocking = List.filter (screenings t) ~f:Screening.blocks_approval in
  if not (Set.is_empty missing)
  then
    Or_error.error_s
      [%message
        "steps not verified for approval"
          ~approval_tier:(t.policy.approval_tier : Tier.t)
          (missing : Step.Set.t)]
  else if t.policy.require_screening && Map.is_empty t.screenings
  then Or_error.error_s [%message "no screening recorded"]
  else if not (List.is_empty blocking)
  then
    Or_error.error_s
      [%message
        "screening hits not discounted"
          ~screenings:(List.map blocking ~f:(fun s -> s.id) : Screening_id.t list)]
  else if t.policy.require_risk_assessment && Option.is_none t.risk
  then Or_error.error_s [%message "no risk assessment recorded"]
  else (
    match edd_outstanding t ~now with
    | Some edd ->
      Or_error.error_s [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
    | None -> Ok ())
;;

let next_review_due t =
  match t.status, t.last_review_at, band t with
  | Approved, Some last, Some band ->
    Some (Time_ns.add last (Policy.Review_intervals.for_band t.policy.review_intervals band))
  | _ -> None
;;

let retain_until t =
  Retention.retain_until
    ~ended:t.closed_at
    ~refused:t.refused_at
    ~retention:t.policy.retention
    ~hold:t.legal_hold
;;

let retention_base t =
  Retention.retain_until
    ~ended:t.closed_at
    ~refused:t.refused_at
    ~retention:t.policy.retention
    ~hold:None
;;

(* Confirmed screening matches that the latest risk inputs do not reflect. *)
let risk_misses_screening t =
  match t.risk with
  | None -> false
  | Some r ->
    List.exists (screenings t) ~f:(fun s ->
      List.exists (Screening.confirmed s) ~f:(fun (h : Screening.Hit.t) ->
        match h.source with
        | Pep_list -> Option.is_none r.inputs.pep
        | Adverse_media ->
          (match r.inputs.adverse_media with
           | Confirmed -> false
           | No_findings | Unconfirmed -> true)
        | Eu_fsf | Un_sc | Ofac_sdn | Uk_sanctions_list -> false))
;;

let follow_ups t ~now =
  let live =
    match t.status with
    | Closed _ | Rejected _ -> false
    | Created | Verifying | Approved | Needs_resubmission _ | Blocked _ -> true
  in
  let pending_reviews =
    List.filter_map (screenings t) ~f:(fun s ->
      Option.some_if (not (List.is_empty (Screening.pending s))) s.id)
  in
  let lapsed =
    match t.status with
    | Approved ->
      let needed =
        match cdd_level t ~now with
        | Enhanced -> t.policy.edd_tier
        | Simplified | Standard -> t.policy.approval_tier
      in
      Set.to_list (required_for t needed)
      |> List.filter ~f:(fun s ->
        match step t s with
        | Expired | Failed _ -> true
        | Pending | Sent | Verified _ -> false)
    | _ -> []
  in
  let list_opt f = function
    | [] -> None
    | xs -> Some (f xs)
  in
  List.filter_opt
    [ Option.some_if
        (live
         && ((Option.is_none t.risk
              &&
              match t.status with
              | Created -> false
              | _ -> true)
             || risk_misses_screening t))
        Follow_up.Risk_assessment_needed
    ; (if live then edd_outstanding t ~now else None)
    ; list_opt (fun ids -> Follow_up.Screening_review ids) pending_reviews
    ; list_opt
        (fun ids -> Follow_up.Open_alerts ids)
        (List.filter_map (alerts t) ~f:(fun a ->
           Option.some_if (Monitoring.Alert.is_open a) a.id))
    ; list_opt
        (fun ids -> Follow_up.Open_investigations ids)
        (List.filter_map (investigations t) ~f:(fun i ->
           Option.some_if (not (Investigation.is_closed i)) i.id))
    ; list_opt
        (fun rs -> Follow_up.Fiu_request_due rs)
        (List.filter_map (fiu_requests t) ~f:(fun r ->
           Option.some_if (Option.is_none r.answered_on) (r.id, r.due)))
    ; Option.bind (next_review_due t) ~f:(fun due ->
        Option.some_if (Time_ns.( <= ) due now) (Follow_up.Periodic_review_due due))
    ; list_opt (fun steps -> Follow_up.Steps_lapsed steps) lapsed
    ; Option.bind (retain_until t) ~f:(fun until ->
        Option.some_if (Time_ns.( <= ) until now) (Follow_up.Retention_ended until))
    ]
;;

let customer_view t : Customer_view.t =
  let status : Customer_view.status =
    match t.status with
    | Created -> if Set.is_empty (missing_steps t) then In_progress else Action_required
    | Verifying -> In_progress
    | Needs_resubmission _ -> Action_required
    | Approved -> Active
    (* Never say why: a block may come from a sanctions match or a report. *)
    | Blocked _ -> Under_review
    | Rejected _ -> Declined
    | Closed _ -> Closed
  in
  let steps_to_complete =
    match t.status with
    | Created | Verifying | Needs_resubmission _ ->
      Set.to_list (missing_for t t.policy.approval_tier)
    | Approved | Blocked _ | Rejected _ | Closed _ -> []
  in
  { status
  ; tier = tier t
  ; steps_to_complete
  ; fields_to_resubmit =
      (match t.status with
       | Needs_resubmission fields -> fields
       | _ -> [])
  }
;;

let limits (type l) (module L : Tier.Limits with type limits = l) t = L.limits (tier t)

let need_operator (actor : Actor.t) ~what =
  match actor with
  | Operator id -> Ok id
  | System | Provider _ ->
    Or_error.error_s [%message "only an operator may do this" (what : string) (actor : Actor.t)]
;;

let find map key ~what ~sexp_of_key =
  match Map.find map key with
  | Some v -> Ok v
  | None -> Or_error.error_s [%message "unknown" (what : string) ~id:(sexp_of_key key : Sexp.t)]
;;

(* The new case and the audit changes for an event not seen before. *)
let step_event t (event : Event.t) : (t * Audit.Change.t list) Or_error.t =
  let open Or_error.Let_syntax in
  let one (t, change) = t, [ change ] in
  let closed () = Or_error.error_s [%message "case is closed" ~status:(t.status : Status.t)] in
  match event.kind, t.person, t.status with
  | Opened person, None, _ -> Ok ({ t with person = Some person }, [ Audit.Change.Opened ])
  | Opened _, Some _, _ -> Or_error.error_s [%message "case already opened"]
  | _, None, _ -> Or_error.error_s [%message "case not opened yet"]
  | Status_changed transition, Some _, _ ->
    (* Status first, so an approval of a rejected case reports the status,
       not missing steps. *)
    let%bind to_ = Status.apply t.status transition ~actor:event.actor in
    let%map () =
      match transition with
      | Approve -> can_approve t ~now:event.at
      | Submit | Reject _ | Request_resubmission _ | Block _ | Unblock | Close _ -> Ok ()
    in
    let t' =
      match transition with
      | Approve -> { t with last_review_at = Some event.at }
      | Close _ -> { t with closed_at = Some event.at }
      | Reject _ -> { t with refused_at = Some event.at }
      | Submit | Request_resubmission _ | Block _ | Unblock -> t
    in
    one ({ t' with status = to_ }, Audit.Change.Status { from = t.status; to_ })
  | Hit_reviewed { screening_id; entry_id; action }, Some _, _ ->
    let%bind screening =
      find t.screenings screening_id ~what:"screening" ~sexp_of_key:Screening_id.sexp_of_t
    in
    let%map reviewed, state = Screening.review screening ~entry_id ~action ~actor:event.actor in
    let t = { t with screenings = Map.set t.screenings ~key:screening_id ~data:reviewed } in
    let change = Audit.Change.Hit_reviewed { screening_id; entry_id; action } in
    let sanctions_confirmed =
      match state with
      | Confirmed _ ->
        List.exists reviewed.hits ~f:(fun ((h : Screening.Hit.t), _) ->
          String.equal h.entry_id entry_id && Screening.List_source.is_sanctions h.source)
      | Open | Under_review _ | Discounted _ | Escalated _ -> false
    in
    (* A confirmed sanctions match blocks the customer at once (FATF R.6:
       freeze without delay). The reviewer is the actor of the block. *)
    (match sanctions_confirmed, t.status with
     | true, ((Created | Verifying | Approved | Rejected _ | Needs_resubmission _) as from)
       ->
       let to_ = Status.Blocked { reason = "confirmed sanctions match"; previous = from } in
       { t with status = to_ }, [ change; Audit.Change.Status { from; to_ } ]
     | true, (Blocked _ | Closed _) | false, _ -> t, [ change ])
  | Alert_decided { alert_id; decision }, Some _, _ ->
    let%bind by = need_operator event.actor ~what:"decide an alert" in
    let%bind alert = find t.alerts alert_id ~what:"alert" ~sexp_of_key:Alert_id.sexp_of_t in
    let%map alert = Monitoring.Alert.decide alert decision ~by ~at:event.at in
    one
      ( { t with alerts = Map.set t.alerts ~key:alert_id ~data:alert }
      , Audit.Change.Alert_decided { alert_id; decision } )
  | Investigation_opened { investigation_id; alerts = ids }, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"open an investigation" in
    let%bind () =
      if Map.mem t.investigations investigation_id
      then Or_error.error_s [%message "investigation already opened"]
      else Ok ()
    in
    let%map () =
      List.map ids ~f:(fun id ->
        let%bind a = find t.alerts id ~what:"alert" ~sexp_of_key:Alert_id.sexp_of_t in
        match a.state with
        | Escalated _ -> Ok ()
        | Open | Closed_no_action _ ->
          Or_error.error_s [%message "only escalated alerts are investigated" (id : Alert_id.t)])
      |> Or_error.combine_errors_unit
    in
    let i = Investigation.open_ ~id:investigation_id ~at:event.at ~alerts:ids in
    one
      ( { t with investigations = Map.set t.investigations ~key:investigation_id ~data:i }
      , Audit.Change.Investigation_opened investigation_id )
  | Investigation_moved { investigation_id; state }, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"move an investigation" in
    let%bind i =
      find t.investigations investigation_id ~what:"investigation"
        ~sexp_of_key:Investigation_id.sexp_of_t
    in
    let%map i = Investigation.move i state in
    one
      ( { t with investigations = Map.set t.investigations ~key:investigation_id ~data:i }
      , Audit.Change.Investigation_moved { investigation_id; state } )
  | Report_changed { investigation_id; state }, Some _, _ ->
    let%bind by = need_operator event.actor ~what:"change a report" in
    let%bind i =
      find t.investigations investigation_id ~what:"investigation"
        ~sexp_of_key:Investigation_id.sexp_of_t
    in
    let%map i = Investigation.report i state ~by in
    one
      ( { t with investigations = Map.set t.investigations ~key:investigation_id ~data:i }
      , Audit.Change.Report_changed { investigation_id; state } )
  | Fiu_request_received { request_id; received_on }, Some _, _ ->
    if Map.mem t.fiu_requests request_id
    then Or_error.error_s [%message "FIU request already recorded"]
    else (
      let r = Investigation.Fiu_request.create ~id:request_id ~received_on () in
      Ok
        (one
           ( { t with fiu_requests = Map.set t.fiu_requests ~key:request_id ~data:r }
           , Audit.Change.Fiu_request_received { request_id; due = r.due } )))
  | Fiu_request_answered { request_id; answered_on }, Some _, _ ->
    let%bind r =
      find t.fiu_requests request_id ~what:"FIU request" ~sexp_of_key:Request_id.sexp_of_t
    in
    (match r.answered_on with
     | Some _ -> Or_error.error_s [%message "FIU request already answered"]
     | None ->
       let r = { r with answered_on = Some answered_on } in
       Ok
         (one
            ( { t with fiu_requests = Map.set t.fiu_requests ~key:request_id ~data:r }
            , Audit.Change.Fiu_request_answered request_id )))
  | Legal_hold_set hold, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"set a legal hold" in
    let%map () = Retention.check_hold hold ~base:(retention_base t) in
    one
      ( { t with legal_hold = Some hold }
      , Audit.Change.Legal_hold_set { until = hold.until } )
  | Legal_hold_released, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"release a legal hold" in
    (match t.legal_hold with
     | None -> Or_error.error_s [%message "no legal hold"]
     | Some _ -> Ok (one ({ t with legal_hold = None }, Audit.Change.Legal_hold_released)))
  (* Monitoring, reporting and retention continue after closure (above);
     everything else is frozen for the retention period. *)
  | ( ( Person_updated _
      | Step_changed _
      | Screened _
      | Risk_assessed _
      | Senior_approval
      | Periodic_review_completed
      | Alert_raised _ )
    , Some _
    , Closed _ ) -> closed ()
  | Person_updated person, Some old, _ ->
    (match t.status with
     | Created | Needs_resubmission _ ->
       (match Person.changed_fields old person with
        | [] -> Or_error.error_s [%message "person update changes nothing"]
        | fields ->
          Ok (one ({ t with person = Some person }, Audit.Change.Person_updated fields)))
     | Verifying | Approved | Rejected _ | Blocked _ | Closed _ ->
       Or_error.error_s
         [%message
           "person can only change before submission or when resubmission is requested"
             ~status:(t.status : Status.t)])
  | Step_changed { step = s; state }, Some _, _ ->
    let from = step t s in
    let%map to_ =
      Or_error.tag_s
        (Verification.transition ~from ~to_:state)
        ~tag:[%message "" ~step:(s : Step.t)]
    in
    one
      ( { t with steps = Map.set t.steps ~key:s ~data:to_ }
      , Audit.Change.Step { step = s; from; to_ } )
  | Screened { screening_id; reason; hits }, Some _, _ ->
    if Map.mem t.screenings screening_id
    then
      Or_error.error_s
        [%message "screening already recorded" (screening_id : Screening_id.t)]
    else (
      let%map screening =
        Screening.create ~id:screening_id ~reason ~screened_at:event.at ~hits
      in
      one
        ( { t with screenings = Map.set t.screenings ~key:screening_id ~data:screening }
        , Audit.Change.Screened { screening_id; reason; hits = List.length hits } ))
  | Risk_assessed assessment, Some _, _ ->
    let%bind () = Risk.Assessment.validate assessment in
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
      , Audit.Change.Risk_assessed
          { score = assessment.score
          ; computed_band = assessment.computed_band
          ; band = Risk.Assessment.band assessment
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
       Ok (one ({ t with senior_approval_at = Some event.at }, Audit.Change.Senior_approval))
     | Simplified | Standard ->
       Or_error.error_s [%message "no enhanced due diligence applies"])
  | Periodic_review_completed, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"complete a periodic review" in
    let%bind () =
      match t.status with
      | Approved -> Ok ()
      | _ ->
        Or_error.error_s
          [%message "only an approved case is reviewed" ~status:(t.status : Status.t)]
    in
    let%bind () =
      match t.risk, t.last_review_at with
      | Some r, Some last when Time_ns.( >= ) r.computed_at last -> Ok ()
      | _ ->
        Or_error.error_s [%message "a review needs a risk assessment since the last review"]
    in
    let%map () =
      match edd_outstanding t ~now:event.at with
      | None -> Ok ()
      | Some edd ->
        Or_error.error_s [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
    in
    one ({ t with last_review_at = Some event.at }, Audit.Change.Periodic_review_completed)
  | Alert_raised { alert_id; source; severity; summary }, Some _, _ ->
    if Map.mem t.alerts alert_id
    then Or_error.error_s [%message "alert already raised" (alert_id : Alert_id.t)]
    else (
      let alert : Monitoring.Alert.t =
        { id = alert_id; raised_at = event.at; source; severity; summary; state = Open }
      in
      Ok
        (one
           ( { t with alerts = Map.set t.alerts ~key:alert_id ~data:alert }
           , Audit.Change.Alert_raised { alert_id; severity } )))
;;

let apply t (event : Event.t) =
  match Map.find t.applied event.id with
  | Some seen when Event.equal seen event -> Ok t
  | Some _ ->
    Or_error.error_s
      [%message
        "event id already applied with different content" ~id:(event.id : Event_id.t)]
  | None ->
    if not (Customer_id.equal event.customer_id t.customer_id)
    then
      Or_error.error_s
        [%message
          "event is for another customer"
            ~case:(t.customer_id : Customer_id.t)
            ~event:(event.customer_id : Customer_id.t)]
    else (
      match step_event t event with
      | Error e -> Error (Error.tag_s e ~tag:[%message "" ~event:(event.id : Event_id.t)])
      | Ok (t, changes) ->
        let audit =
          List.fold changes ~init:t.audit ~f:(fun audit change ->
            Audit.add
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

let of_events ?policy customer_id events =
  List.fold_result events ~init:(create ?policy customer_id) ~f:apply
;;
