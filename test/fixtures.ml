open! Core
open Kyc_core

let customer = Customer_id.of_string "cus-1"
let op_a = Actor.Operator (Operator_id.of_string "op-a")
let op_b = Actor.Operator (Operator_id.of_string "op-b")
let provider = Actor.Provider "provider-a"
let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let counter = ref 0
let reset () = counter := 0

type draft = int -> Event.t

let number (draft : draft) =
  incr counter;
  draft !counter
;;

let time n = Time_ns.add t0 (Time_ns.Span.of_int_sec (60 * n))

let event ?(actor = provider) ?note ?id kind n : Event.t =
  let id = Option.value id ~default:(sprintf "evt-%d" n) |> Event_id.of_string in
  { id; customer_id = customer; at = time n; actor; note; kind }
;;

let verified step = event (Step_changed { step; state = Verified t0 })

let status ?(actor = op_a) ?note transition =
  event ~actor ?note (Status_changed transition)
;;

let assessed ?(inputs = Risk.Inputs.baseline) ?override () n =
  let at = time n in
  let a = Risk.assess Risk.Model.default inputs ~at in
  let a =
    match override with
    | None -> a
    | Some (band, reason) ->
      Or_error.ok_exn
        (Risk.override a ~band ~reason ~by:(Operator_id.of_string "op-a") ~at)
  in
  event ~actor:op_a (Risk_assessed a) n
;;

let screened ?(reason = Screening.Screen_reason.Onboarding) id hits =
  event
    ~actor:System
    (Screened { screening_id = Screening_id.of_string id; reason; hits })
;;

let hit source entry_id : Screening.Hit.t =
  { source
  ; list_version = "2026-10-13"
  ; entry_id
  ; matched_name = "A. Example"
  ; score = 92
  ; algorithm = "jaro-winkler-1"
  }
;;

let review ?(actor = op_a) id entry_id action =
  event
    ~actor
    (Hit_reviewed { screening_id = Screening_id.of_string id; entry_id; action })
;;

let apply_all case drafts =
  List.fold drafts ~init:case ~f:(fun case draft ->
    match Kyc_case.apply case (number draft) with
    | Ok case -> case
    | Error err ->
      print_s [%sexp (err : Error.t)];
      case)
;;

let opened () =
  reset ();
  apply_all (Kyc_case.create customer) [ event (Opened Test_person.person) ]
;;

let ready_for_approval ?inputs () =
  apply_all
    (opened ())
    [ verified Email
    ; verified Mobile
    ; status Submit
    ; verified Identity_document
    ; verified Liveness
    ; screened "scr-1" []
    ; assessed ?inputs ()
    ]
;;

let show_case case =
  print_s
    [%message
      ""
        ~status:(Kyc_case.status case : Status.t)
        ~tier:(Kyc_case.tier case : Tier.t)
        ~band:(Kyc_case.band case : Risk.Band.t option)
        ~missing:(Kyc_case.missing_steps case : Verification.Step.Set.t)]
;;

let show_follow_ups case ~now =
  print_s
    [%sexp
      (Kyc_case.follow_ups case ~now:(Time_ns.of_string_with_utc_offset now)
       : Kyc_case.Follow_up.t list)]
;;
