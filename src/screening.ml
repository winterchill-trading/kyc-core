open! Core

module List_source = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Eu_fsf
        | Un_sc
        | Ofac_sdn
        | Uk_sanctions_list
        | Pep_list
        | Adverse_media
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Eu_fsf
    | Un_sc
    | Ofac_sdn
    | Uk_sanctions_list
    | Pep_list
    | Adverse_media
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  let is_sanctions = function
    | Eu_fsf | Un_sc | Ofac_sdn | Uk_sanctions_list -> true
    | Pep_list | Adverse_media -> false
  ;;
end

module Screen_reason = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Onboarding
        | List_update
        | Data_change
        | Periodic
        | Payment
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Onboarding
    | List_update
    | Data_change
    | Periodic
    | Payment
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Hit = struct
  module Stable = struct
    module V1 = struct
      type t =
        { source : List_source.Stable.V1.t
        ; list_version : string
        ; entry_id : string
        ; matched_name : string
        ; score : int
        ; algorithm : string
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { source : List_source.t
    ; list_version : string
    ; entry_id : string
    ; matched_name : string
    ; score : int
    ; algorithm : string
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Review_action = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Take
        | Propose_discount of string
        | Confirm_discount
        | Confirm of string
        | Escalate of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Take
    | Propose_discount of string
    | Confirm_discount
    | Confirm of string
    | Escalate of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Hit_state = struct
  type t =
    | Open
    | Under_review of
        { by : Operator_id.t
        ; discount_proposal : string option
        }
    | Discounted of
        { reason : string
        ; proposed_by : Operator_id.t
        ; confirmed_by : Operator_id.t
        }
    | Confirmed of
        { by : Operator_id.t
        ; reason : string
        }
    | Escalated of
        { by : Operator_id.t
        ; reason : string
        }
  [@@deriving compare, equal, hash, sexp]

  let is_final = function
    | Discounted _ | Confirmed _ -> true
    | Open | Under_review _ | Escalated _ -> false
  ;;
end

type t =
  { id : Screening_id.t
  ; reason : Screen_reason.t
  ; screened_at : Timestamp.t
  ; hits : (Hit.t * Hit_state.t) list
  }
[@@deriving compare, equal, hash, sexp]

let create ~id ~reason ~screened_at ~hits =
  match
    List.find_a_dup hits ~compare:(fun (a : Hit.t) (b : Hit.t) ->
      String.compare a.entry_id b.entry_id)
  with
  | Some hit ->
    Or_error.error_s [%message "duplicate hit in screening" ~entry_id:(hit.entry_id : string)]
  | None ->
    Ok { id; reason; screened_at; hits = List.map hits ~f:(fun h -> h, Hit_state.Open) }
;;

let hit_state t ~entry_id =
  List.find_map t.hits ~f:(fun ((h : Hit.t), s) ->
    Option.some_if (String.equal h.entry_id entry_id) s)
;;

let non_empty what reason =
  if String.is_empty (String.strip reason)
  then Or_error.error_s [%message "a reason is required" (what : string)]
  else Ok ()
;;

let next (state : Hit_state.t) (action : Review_action.t) ~(by : Operator_id.t) =
  let open Or_error.Let_syntax in
  let illegal () =
    Or_error.error_s
      [%message "illegal hit review" ~state:(state : Hit_state.t) (action : Review_action.t)]
  in
  match state, action with
  | (Discounted _ | Confirmed _), _ -> illegal ()
  | (Open | Escalated _), Take -> Ok (Hit_state.Under_review { by; discount_proposal = None })
  | (Open | Under_review _ | Escalated _), Propose_discount reason ->
    let%map () = non_empty "discount" reason in
    Hit_state.Under_review { by; discount_proposal = Some reason }
  | Under_review { by = proposer; discount_proposal = Some reason }, Confirm_discount ->
    if Operator_id.equal proposer by
    then
      Or_error.error_s
        [%message "discounting needs a second operator" ~proposed_by:(proposer : Operator_id.t)]
    else Ok (Hit_state.Discounted { reason; proposed_by = proposer; confirmed_by = by })
  | (Open | Under_review _ | Escalated _), Confirm reason ->
    let%map () = non_empty "confirmation" reason in
    Hit_state.Confirmed { by; reason }
  | (Open | Under_review _), Escalate reason ->
    let%map () = non_empty "escalation" reason in
    Hit_state.Escalated { by; reason }
  | Under_review _, Take
  | (Open | Escalated _ | Under_review { discount_proposal = None; _ }), Confirm_discount
  | Escalated _, Escalate _ -> illegal ()
;;

let review t ~entry_id ~action ~(actor : Actor.t) =
  let open Or_error.Let_syntax in
  let%bind by =
    match actor with
    | Operator id -> Ok id
    | System | Provider _ ->
      Or_error.error_s [%message "only an operator may review a hit" (actor : Actor.t)]
  in
  let%bind state =
    match hit_state t ~entry_id with
    | Some s -> Ok s
    | None ->
      Or_error.error_s
        [%message "unknown hit" ~screening:(t.id : Screening_id.t) (entry_id : string)]
  in
  let%map state = next state action ~by in
  ( { t with
      hits =
        List.map t.hits ~f:(fun ((h : Hit.t), s) ->
          if String.equal h.entry_id entry_id then h, state else h, s)
    }
  , state )
;;

let blocks_approval t =
  List.exists t.hits ~f:(fun (_, s) ->
    match s with
    | Discounted _ -> false
    | Open | Under_review _ | Confirmed _ | Escalated _ -> true)
;;

let pending t =
  List.filter_map t.hits ~f:(fun (h, s) -> Option.some_if (not (Hit_state.is_final s)) h)
;;

let confirmed t =
  List.filter_map t.hits ~f:(fun (h, s) ->
    match s with
    | Confirmed _ -> Some h
    | Open | Under_review _ | Discounted _ | Escalated _ -> None)
;;
