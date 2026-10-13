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

let () =
  Command_unix.run
    (Command.group
       ~summary:"Inspect provider-neutral KYC cases"
       [ "replay", replay; "policy", policy; "assess", assess; "match", match_names ])
;;
