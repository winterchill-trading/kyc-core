open! Core
module Step = Verification.Step
module State = Verification.State

module Policy = struct
  type t =
    { rules : Tier.Rules.t
    ; approval_tier : Tier.t
    ; require_screening : bool
    }
  [@@deriving compare, equal, sexp]

  let default =
    { rules = Tier.Rules.default; approval_tier = T2; require_screening = true }
  ;;
end

type t =
  { customer_id : Customer_id.t
  ; policy : Policy.t
  ; person : Person.t option
  ; status : Status.t
  ; steps : State.t Step.Map.t
  ; screenings : Screening.t Screening_id.Map.t
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
  ; applied = Event_id.Map.empty
  ; audit = Audit.empty
  }
;;

let step t step = Map.find_exn t.steps step
let steps t = Map.to_alist t.steps
let screenings t = Map.data t.screenings
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
  else Ok ()
;;

let limits (type l) (module L : Tier.Limits with type limits = l) t = L.limits (tier t)

(* The new case and the audit change for an event not seen before. *)
let step_event t (event : Event.t) : (t * Audit.Change.t) Or_error.t =
  let open Or_error.Let_syntax in
  match event.kind, t.person with
  | Opened person, None -> Ok ({ t with person = Some person }, Audit.Change.Opened)
  | Opened _, Some _ -> Or_error.error_s [%message "case already opened"]
  | _, None -> Or_error.error_s [%message "case not opened yet"]
  | Person_updated person, Some old ->
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
  | Step_changed { step = s; state }, Some _ ->
    let from = step t s in
    let%map to_ =
      Or_error.tag_s
        (Verification.transition ~from ~to_:state)
        ~tag:[%message "" ~step:(s : Step.t)]
    in
    ( { t with steps = Map.set t.steps ~key:s ~data:to_ }
    , Audit.Change.Step { step = s; from; to_ } )
  | Status_changed transition, Some _ ->
    (* Status first, so an approval of a rejected case reports the status, not missing
       steps. *)
    let%bind to_ = Status.apply t.status transition ~actor:event.actor in
    let%map () =
      match transition with
      | Approve -> can_approve t
      | Submit | Reject _ | Request_resubmission _ | Block _ | Unblock | Close _ -> Ok ()
    in
    { t with status = to_ }, Audit.Change.Status { from = t.status; to_ }
  | Screened { screening_id; outcome }, Some _ ->
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
  | Screening_reviewed { screening_id; decision }, Some _ ->
    let%bind screening =
      match Map.find t.screenings screening_id with
      | Some s -> Ok s
      | None ->
        Or_error.error_s [%message "unknown screening" (screening_id : Screening_id.t)]
    in
    let%map reviewed = Screening.review screening ~decision ~actor:event.actor in
    ( { t with screenings = Map.set t.screenings ~key:screening_id ~data:reviewed }
    , Audit.Change.Screening_reviewed { screening_id; decision } )
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
