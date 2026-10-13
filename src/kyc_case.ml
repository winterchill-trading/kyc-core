open! Core
module Step = Verification.Step
module State = Verification.State

module Policy = struct
  module Review_intervals = struct
    type t =
      { low : Time_ns.Span.t
      ; medium : Time_ns.Span.t
      ; high : Time_ns.Span.t
      }
    [@@deriving compare, equal, sexp]

    let for_rating t : Risk.Level.t -> _ = function
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
    ; review_intervals : Review_intervals.t
    ; retention : Time_ns.Span.t
    }
  [@@deriving compare, equal, sexp]

  (* Calendar years would need a zone and leap-day rules; 365-day years are slightly
     early, which is the safe side for reviews and close enough for retention given that
     deletion is a manual, audited job. *)
  let years n = Time_ns.Span.of_day (Float.of_int (365 * n))

  let default =
    { rules = Tier.Rules.default
    ; approval_tier = T2
    ; edd_tier = T3
    ; require_screening = true
    ; require_risk_assessment = true
    ; edd_requires_senior_approval = true
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
    | Open_alerts of Monitoring.Alert_id.t list
    | Periodic_review_due of Timestamp.t
    | Steps_lapsed of Step.t list
    | Retention_ended of Timestamp.t
  [@@deriving compare, equal, sexp]
end

type t =
  { customer_id : Customer_id.t
  ; policy : Policy.t
  ; person : Person.t option
  ; status : Status.t
  ; steps : State.t Step.Map.t
  ; screenings : Screening.t Screening_id.Map.t
  ; risk : Risk.t option
  ; senior_approval_at : Timestamp.t option
  ; alerts : Monitoring.Alert.t Monitoring.Alert_id.Map.t
  ; last_review_at : Timestamp.t option
  ; closed_at : Timestamp.t option
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
  ; alerts = Monitoring.Alert_id.Map.empty
  ; last_review_at = None
  ; closed_at = None
  ; applied = Event_id.Map.empty
  ; audit = Audit.empty
  }
;;

let step t step = Map.find_exn t.steps step
let steps t = Map.to_alist t.steps
let screenings t = Map.data t.screenings
let alerts t = Map.data t.alerts
let has_applied t id = Map.mem t.applied id
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

let open_alerts t = List.filter (alerts t) ~f:Monitoring.Alert.is_open

let edd_triggers t =
  let from_risk =
    match t.risk with
    | None -> []
    | Some r -> Risk.edd_triggers r.factors ~rating:r.rating
  in
  let from_alerts =
    if List.exists (open_alerts t) ~f:(fun a -> Risk.Level.equal a.severity High)
    then [ Risk.Edd_trigger.Monitoring_alert ]
    else []
  in
  from_risk @ from_alerts
;;

(* A sign-off covers the assessment it was given for; a new assessment needs a new one. *)
let senior_approval_current t =
  match t.senior_approval_at, t.risk with
  | Some approved, Some r -> Time_ns.( >= ) approved r.assessed_at
  | None, _ | _, None -> false
;;

(* [None] when EDD does not apply or is complete. *)
let edd_outstanding t =
  match edd_triggers t with
  | [] -> None
  | triggers ->
    let missing_steps = missing_for t t.policy.edd_tier in
    let senior_approval =
      t.policy.edd_requires_senior_approval && not (senior_approval_current t)
    in
    if Set.is_empty missing_steps && not senior_approval
    then None
    else Some (Follow_up.Edd_outstanding { triggers; missing_steps; senior_approval })
;;

let can_approve t =
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
        "screening blocks approval"
          ~screenings:
            (List.map blocking ~f:(fun s -> s.id, s.decision)
             : (Screening_id.t * Screening.Decision.t) list)]
  else if t.policy.require_risk_assessment && Option.is_none t.risk
  then Or_error.error_s [%message "no risk assessment recorded"]
  else (
    match edd_outstanding t with
    | Some edd ->
      Or_error.error_s [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
    | None -> Ok ())
;;

let next_review_due t =
  match t.status, t.last_review_at, t.risk with
  | Approved, Some last, Some r ->
    Some
      (Time_ns.add
         last
         (Policy.Review_intervals.for_rating t.policy.review_intervals r.rating))
  | _ -> None
;;

let retain_until t = Option.map t.closed_at ~f:(fun c -> Time_ns.add c t.policy.retention)

(* Confirmed screening matches that the latest risk factors do not reflect. *)
let risk_misses_screening t =
  match t.risk with
  | None -> false
  | Some r ->
    List.exists (screenings t) ~f:(fun s ->
      match s.decision, s.outcome with
      | True_match _, Hits hits ->
        List.exists hits ~f:(fun (h : Screening.Hit.t) ->
          match h.list_kind with
          | Pep -> not (Risk.Pep_exposure.is_pep r.factors.pep)
          | Adverse_media -> not r.factors.adverse_media
          | Sanctions -> false)
      | _ -> false)
;;

let follow_ups t ~now =
  let open_case =
    match t.status with
    | Closed _ | Rejected _ -> false
    | Created | Verifying | Approved | Needs_resubmission _ | Blocked _ -> true
  in
  let pending_reviews =
    List.filter_map (screenings t) ~f:(fun s ->
      match s.decision with
      | Pending_review -> Some s.id
      | Not_required | False_positive _ | True_match _ -> None)
  in
  let lapsed =
    match t.status with
    | Approved ->
      let needed =
        if List.is_empty (edd_triggers t)
        then t.policy.approval_tier
        else t.policy.edd_tier
      in
      Set.to_list (required_for t needed)
      |> List.filter ~f:(fun s ->
        match step t s with
        | Expired | Failed _ -> true
        | Pending | Sent | Verified _ -> false)
    | _ -> []
  in
  List.filter_opt
    [ Option.some_if
        (open_case
         && ((Option.is_none t.risk
              &&
              match t.status with
              | Created -> false
              | _ -> true)
             || risk_misses_screening t))
        Follow_up.Risk_assessment_needed
    ; (if open_case then edd_outstanding t else None)
    ; Option.some_if
        (not (List.is_empty pending_reviews))
        (Follow_up.Screening_review pending_reviews)
    ; (match open_alerts t with
       | [] -> None
       | alerts ->
         Some
           (Follow_up.Open_alerts
              (List.map alerts ~f:(fun (a : Monitoring.Alert.t) -> a.id))))
    ; Option.bind (next_review_due t) ~f:(fun due ->
        Option.some_if (Time_ns.( <= ) due now) (Follow_up.Periodic_review_due due))
    ; Option.some_if (not (List.is_empty lapsed)) (Follow_up.Steps_lapsed lapsed)
    ; Option.bind (retain_until t) ~f:(fun until ->
        Option.some_if (Time_ns.( <= ) until now) (Follow_up.Retention_ended until))
    ]
;;

let limits (type l) (module L : Tier.Limits with type limits = l) t = L.limits (tier t)

let need_operator (actor : Actor.t) ~what =
  match actor with
  | Operator id -> Ok id
  | System | Provider _ ->
    Or_error.error_s
      [%message "only an operator may do this" (what : string) (actor : Actor.t)]
;;

(* The new case and the audit change for an event not seen before. *)
let step_event t (event : Event.t) : (t * Audit.Change.t) Or_error.t =
  let open Or_error.Let_syntax in
  let closed =
    Or_error.error_s [%message "case is closed" ~status:(t.status : Status.t)]
  in
  match event.kind, t.person, t.status with
  | Opened person, None, _ -> Ok ({ t with person = Some person }, Audit.Change.Opened)
  | Opened _, Some _, _ -> Or_error.error_s [%message "case already opened"]
  | _, None, _ -> Or_error.error_s [%message "case not opened yet"]
  | Status_changed transition, Some _, _ ->
    (* Status first, so an approval of a rejected case reports the status, not missing
       steps. *)
    let%bind to_ = Status.apply t.status transition ~actor:event.actor in
    let%map () =
      match transition with
      | Approve -> can_approve t
      | Submit | Reject _ | Request_resubmission _ | Block _ | Unblock | Close _ -> Ok ()
    in
    let t =
      match transition with
      | Approve -> { t with last_review_at = Some event.at }
      | Close _ -> { t with closed_at = Some event.at }
      | Submit | Reject _ | Request_resubmission _ | Block _ | Unblock -> t
    in
    { t with status = to_ }, Audit.Change.Status { from = t.status; to_ }
  | Screening_reviewed { screening_id; decision }, Some _, _ ->
    let%bind screening =
      match Map.find t.screenings screening_id with
      | Some s -> Ok s
      | None ->
        Or_error.error_s [%message "unknown screening" (screening_id : Screening_id.t)]
    in
    let%map reviewed = Screening.review screening ~decision ~actor:event.actor in
    ( { t with screenings = Map.set t.screenings ~key:screening_id ~data:reviewed }
    , Audit.Change.Screening_reviewed { screening_id; decision } )
  | Alert_closed { alert_id; resolution }, Some _, _ ->
    let%bind operator = need_operator event.actor ~what:"close an alert" in
    let%bind alert =
      match Map.find t.alerts alert_id with
      | Some a when Monitoring.Alert.is_open a -> Ok a
      | Some _ ->
        Or_error.error_s
          [%message "alert already closed" (alert_id : Monitoring.Alert_id.t)]
      | None ->
        Or_error.error_s [%message "unknown alert" (alert_id : Monitoring.Alert_id.t)]
    in
    let alert = { alert with closed = Some (resolution, operator, event.at) } in
    Ok
      ( { t with alerts = Map.set t.alerts ~key:alert_id ~data:alert }
      , Audit.Change.Alert_closed { alert_id; resolution } )
  (* After closure only open work may be finished (above); records are kept unchanged for
     the retention period. *)
  | ( ( Person_updated _
      | Step_changed _
      | Screened _
      | Risk_assessed _
      | Senior_approval
      | Periodic_review_completed
      | Alert_raised _ )
    , Some _
    , Closed _ ) -> closed
  | Person_updated person, Some old, _ ->
    (match t.status with
     | Created | Needs_resubmission _ ->
       (match Person.changed_fields old person with
        | [] -> Or_error.error_s [%message "person update changes nothing"]
        | fields ->
          Ok ({ t with person = Some person }, Audit.Change.Person_updated fields))
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
    ( { t with steps = Map.set t.steps ~key:s ~data:to_ }
    , Audit.Change.Step { step = s; from; to_ } )
  | Screened { screening_id; outcome }, Some _, _ ->
    if Map.mem t.screenings screening_id
    then
      Or_error.error_s
        [%message "screening already recorded" (screening_id : Screening_id.t)]
    else (
      let screening = Screening.create ~id:screening_id ~screened_at:event.at ~outcome in
      let hits =
        match outcome with
        | Clear -> 0
        | Hits hits -> List.length hits
      in
      Ok
        ( { t with screenings = Map.set t.screenings ~key:screening_id ~data:screening }
        , Audit.Change.Screened { screening_id; hits } ))
  | Risk_assessed { factors; override }, Some _, _ ->
    let%map risk =
      Risk.assess ~assessed_at:event.at ~factors ~override ~actor:event.actor
    in
    ( { t with risk = Some risk }
    , Audit.Change.Risk_assessed { computed = risk.computed; rating = risk.rating } )
  | Senior_approval, Some _, _ ->
    let%bind _ = need_operator event.actor ~what:"give senior approval" in
    let%bind () =
      match event.note with
      | Some note when not (String.is_empty (String.strip note)) -> Ok ()
      | _ -> Or_error.error_s [%message "senior approval needs a rationale in the note"]
    in
    if List.is_empty (edd_triggers t)
    then Or_error.error_s [%message "no enhanced due diligence applies"]
    else Ok ({ t with senior_approval_at = Some event.at }, Audit.Change.Senior_approval)
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
      | Some r, Some last when Time_ns.( >= ) r.assessed_at last -> Ok ()
      | _ ->
        Or_error.error_s
          [%message "a review needs a risk assessment since the last review"]
    in
    let%map () =
      match edd_outstanding t with
      | None -> Ok ()
      | Some edd ->
        Or_error.error_s
          [%message "enhanced due diligence outstanding" (edd : Follow_up.t)]
    in
    { t with last_review_at = Some event.at }, Audit.Change.Periodic_review_completed
  | Alert_raised { alert_id; source; severity; summary }, Some _, _ ->
    if Map.mem t.alerts alert_id
    then
      Or_error.error_s
        [%message "alert already raised" (alert_id : Monitoring.Alert_id.t)]
    else (
      let alert : Monitoring.Alert.t =
        { id = alert_id; raised_at = event.at; source; severity; summary; closed = None }
      in
      Ok
        ( { t with alerts = Map.set t.alerts ~key:alert_id ~data:alert }
        , Audit.Change.Alert_raised { alert_id; severity } ))
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
      | Ok (t, change) ->
        let entry : Audit.Entry.t =
          { at = event.at
          ; actor = event.actor
          ; event_id = event.id
          ; change
          ; why = event.note
          }
        in
        Ok
          { t with
            applied = Map.set t.applied ~key:event.id ~data:event
          ; audit = Audit.add t.audit entry
          })
;;

let of_events ?policy customer_id events =
  List.fold_result events ~init:(create ?policy customer_id) ~f:apply
;;
