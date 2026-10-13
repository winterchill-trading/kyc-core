open! Core
open Kyc_core

let print_case case =
  print_s
    [%message
      ""
        ~customer:(Kyc_case.customer_id case : Customer_id.t)
        ~status:(Kyc_case.status case : Status.t)
        ~tier:(Kyc_case.tier case : Tier.t)
        ~verified_tier:(Kyc_case.verified_tier case : Tier.t)
        ~missing_for_next_tier:(Kyc_case.missing_steps case : Verification.Step.Set.t)
        ~can_approve:(Kyc_case.can_approve case : unit Or_error.t)]
;;

let replay =
  Command.basic
    ~summary:"Apply a file of events (sexps) and print the case and its audit trail"
    ~readme:(fun () ->
      "Each sexp in FILE is an Event.t. PII fields are read in clear and printed redacted.\n\
       Illegal events are reported and skipped, as a consumer would.")
    (let%map_open.Command file = anon ("FILE" %: string)
     and strict = flag "strict" no_arg ~doc:" stop at the first illegal event"
     and audit = flag "audit" no_arg ~doc:" also print the audit trail" in
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
         print_case case;
         if audit
         then
           List.iter
             (Audit.entries (Kyc_case.audit case))
             ~f:(fun entry -> print_s [%sexp (entry : Audit.Entry.t)]))
;;

let rules =
  Command.basic
    ~summary:"Print the default tier rule table"
    (Command.Param.return (fun () ->
       print_s [%sexp (Kyc_case.Policy.default : Kyc_case.Policy.t)]))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Inspect provider-neutral KYC cases"
       [ "replay", replay; "rules", rules ])
;;
