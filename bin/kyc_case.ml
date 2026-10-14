open! Core
open Kyc_core

let print_case case ~now =
  print_s
    [%message
      ""
        ~customer:(Kyc_case.customer_id case : Customer_id.t)
        ~status:(Kyc_case.status case : Status.t)
        ~tier:(Kyc_case.tier case : Tier.t)
        ~band:(Kyc_case.band case : Risk.Band.t option)
        ~cdd:(Kyc_case.cdd_level case ~now : Cdd_level.t)
        ~edd:(Kyc_case.edd_triggers case ~now : Risk.Edd_trigger.t list)
        ~missing_for_next_tier:(Kyc_case.missing_steps case : Verification.Step.Set.t)
        ~next_review_due:(Kyc_case.next_review_due case : Timestamp.t option)
        ~retain_until:(Kyc_case.retain_until case : Timestamp.t option)
        ~follow_ups:(Kyc_case.follow_ups case ~now : Kyc_case.Follow_up.t list)]
;;

let audience_arg =
  Command.Arg_type.of_alist_exn
    [ "customer", Visibility.Audience.Customer
    ; "support", Support
    ; "compliance", Compliance
    ]
;;

let replay =
  Command.basic
    ~summary:"Apply a file of events (sexps) and print the case"
    ~readme:(fun () ->
      "Each sexp in FILE is an Event.t. PII fields are read in clear and printed redacted.\n\
       Illegal events are reported and skipped, as a consumer would. -as customer prints \
       only\n\
       what the customer may see (no risk, screening, alerts or reports).")
    (let%map_open.Command file = anon ("FILE" %: string)
     and strict = flag "strict" no_arg ~doc:" stop at the first illegal event"
     and audit = flag "audit" no_arg ~doc:" also print the audit trail"
     and audience =
       flag_optional_with_default_doc_sexp
         "as"
         audience_arg
         Visibility.Audience.sexp_of_t
         ~default:Visibility.Audience.Compliance
         ~doc:"WHO customer, support or compliance"
     and now =
       flag
         "now"
         (optional Time_ns.arg_type)
         ~doc:"TIME evaluate dates at TIME (default: the last event's time)"
     in
     fun () ->
       let events = Sexp.load_sexps_conv_exn file [%of_sexp: Event.t] in
       match events with
       | [] -> eprintf "%s: no events\n" file
       | first :: _ ->
         let case =
           List.fold events ~init:(Kyc_case.create first.customer_id) ~f:(fun case e ->
             match Kyc_case.apply case e with
             | Ok case -> case
             | Error err when strict -> Error.raise err
             | Error err ->
               print_s [%message "skipped" (err : Error.t)];
               case)
         in
         let now = Option.value now ~default:(List.last_exn events).Event.at in
         (match audience with
          | Customer ->
            print_s [%sexp (Kyc_case.customer_view case : Kyc_case.Customer_view.t)]
          | Support | Compliance -> print_case case ~now);
         if audit
         then
           List.iter
             (Audit.for_audience (Kyc_case.audit case) audience)
             ~f:(fun entry -> print_s [%sexp (entry : Audit.Entry.t)]))
;;

let policy =
  Command.basic
    ~summary:"Print the default case policy and risk model"
    (Command.Param.return (fun () ->
       print_s [%sexp (Kyc_case.Policy.default : Kyc_case.Policy.t)];
       print_s [%sexp (Risk.Model.default : Risk.Model.t)]))
;;

let assess =
  Command.basic
    ~summary:"Score risk inputs (a Risk.Inputs.t sexp in FILE) with the default model"
    (let%map_open.Command file = anon ("FILE" %: string) in
     fun () ->
       let inputs = Sexp.load_sexp_conv_exn file [%of_sexp: Risk.Inputs.t] in
       let at = Time_ns.now () in
       let a = Risk.assess Risk.Model.default inputs ~at in
       print_s
         [%message
           ""
             ~points:
               (Risk.Model.points
                  Risk.Model.default
                  inputs
                  ~on:(Date.today ~zone:Timezone.utc)
                : (string * int) list)
             ~score:(a.score : int)
             ~floors:(a.floors : Risk.Edd_trigger.t list)
             ~band:(Risk.Assessment.band a : Risk.Band.t)])
;;

let match_names =
  Command.basic
    ~summary:"Score two names with the screening name matcher"
    (let%map_open.Command a = anon ("NAME" %: string)
     and b = anon ("NAME" %: string) in
     fun () ->
       printf
         "%s | %s | %.3f%s\n"
         (Name_match.normalize a)
         (Name_match.normalize b)
         (Name_match.score a b)
         (if Name_match.is_candidate a b then " candidate" else ""))
;;

let print_kyb_case case ~now =
  print_s
    [%message
      ""
        ~entity:(Kyb_case.entity_id case : Entity_id.t)
        ~status:(Kyb_case.status case : Kyb_status.t)
        ~round:(Kyb_case.round case : int)
        ~band:(Kyb_case.band case : Risk.Band.t option)
        ~cdd:(Kyb_case.cdd_level case ~now : Cdd_level.t)
        ~beneficial_owners:
          (List.map (Kyb_case.beneficial_owners case) ~f:(fun (p, basis) -> p.id, basis)
           : (Customer_id.t * Kyb_party.Basis.t list) list)
        ~missing:(Kyb_case.missing_for_submission case : Kyb_item.t list)
        ~parties_pending:(Kyb_case.parties_pending case : Customer_id.t list)
        ~decisions:(Kyb_case.decisions case : Kyb_case.Decision.t list)
        ~next_review_due:(Kyb_case.next_review_due case : Timestamp.t option)
        ~retain_until:(Kyb_case.retain_until case : Timestamp.t option)
        ~follow_ups:(Kyb_case.follow_ups case ~now : Kyb_case.Follow_up.t list)]
;;

let kyb_replay =
  Command.basic
    ~summary:"Apply a file of business-case events (sexps) and print the case"
    ~readme:(fun () ->
      "Each sexp in FILE is a Kyb_event.t. Illegal events are reported and skipped, as a\n\
       consumer would. -as customer prints only what the applicant may see (no risk,\n\
       linked people's case states or block reasons).")
    (let%map_open.Command file = anon ("FILE" %: string)
     and strict = flag "strict" no_arg ~doc:" stop at the first illegal event"
     and audit = flag "audit" no_arg ~doc:" also print the audit trail"
     and audience =
       flag_optional_with_default_doc_sexp
         "as"
         audience_arg
         Visibility.Audience.sexp_of_t
         ~default:Visibility.Audience.Compliance
         ~doc:"WHO customer, support or compliance"
     and now =
       flag
         "now"
         (optional Time_ns.arg_type)
         ~doc:"TIME evaluate dates at TIME (default: the last event's time)"
     in
     fun () ->
       let events = Sexp.load_sexps_conv_exn file [%of_sexp: Kyb_event.t] in
       match events with
       | [] -> eprintf "%s: no events\n" file
       | first :: _ ->
         let case =
           List.fold events ~init:(Kyb_case.create first.entity_id) ~f:(fun case e ->
             match Kyb_case.apply case e with
             | Ok case -> case
             | Error err when strict -> Error.raise err
             | Error err ->
               print_s [%message "skipped" (err : Error.t)];
               case)
         in
         let now = Option.value now ~default:(List.last_exn events).Kyb_event.at in
         (match audience with
          | Customer ->
            print_s [%sexp (Kyb_case.applicant_view case : Kyb_case.Applicant_view.t)]
          | Support | Compliance -> print_kyb_case case ~now);
         if audit
         then
           List.iter
             (Kyb_audit.for_audience (Kyb_case.audit case) audience)
             ~f:(fun entry -> print_s [%sexp (entry : Kyb_audit.Entry.t)]))
;;

let kyb_policy =
  Command.basic
    ~summary:"Print the default business-case policy and risk model"
    (Command.Param.return (fun () ->
       print_s [%sexp (Kyb_case.Policy.default : Kyb_case.Policy.t)];
       print_s [%sexp (Kyb_risk.Model.default : Kyb_risk.Model.t)]))
;;

let kyb_assess =
  Command.basic
    ~summary:"Score business risk inputs (a Kyb_risk.Inputs.t sexp in FILE)"
    (let%map_open.Command file = anon ("FILE" %: string) in
     fun () ->
       let inputs = Sexp.load_sexp_conv_exn file [%of_sexp: Kyb_risk.Inputs.t] in
       let at = Time_ns.now () in
       let a = Kyb_risk.assess Kyb_risk.Model.default inputs ~at in
       print_s
         [%message
           ""
             ~points:
               (Kyb_risk.Model.points
                  Kyb_risk.Model.default
                  inputs
                  ~on:(Date.today ~zone:Timezone.utc)
                : (string * int) list)
             ~score:(a.score : int)
             ~floors:(a.floors : Kyb_risk.Edd_trigger.t list)
             ~band:(Kyb_risk.Assessment.band a : Risk.Band.t)])
;;

let kyb_requirements =
  Command.basic
    ~summary:"List the documents the default policy needs for a legal form"
    (let%map_open.Command jurisdiction =
       flag "jurisdiction" (required string) ~doc:"CC country of registration (ISO alpha-2)"
     and legal_form =
       flag
         "legal-form"
         (required (Command.Arg_type.create (fun s -> Or_error.ok_exn (Legal_form.of_string s))))
         ~doc:"FORM e.g. private_limited_company, trust, other:NAME"
     in
     fun () ->
       let rules = Kyb_case.Policy.default.requirements in
       let entity =
         Legal_entity.create
           ~legal_name:"-"
           ~jurisdiction:(Country.of_string jurisdiction)
           ~legal_form
       in
       print_s
         [%message
           ""
             ~coverage:(Kyb_requirements.check_entity rules entity : unit Or_error.t)
             ~documents:
               (Kyb_requirements.required rules entity []
                |> List.map ~f:(fun (r : Kyb_document.Requirement.t) -> r.kind)
                : Kyb_document.Kind.t list)])
;;

let kyb =
  Command.group
    ~summary:"Inspect business (KYB) cases"
    [ "replay", kyb_replay
    ; "policy", kyb_policy
    ; "assess", kyb_assess
    ; "requirements", kyb_requirements
    ]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Inspect provider-neutral KYC and KYB cases"
       [ "replay", replay
       ; "policy", policy
       ; "assess", assess
       ; "match", match_names
       ; "kyb", kyb
       ])
;;
