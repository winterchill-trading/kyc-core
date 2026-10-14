open! Core
open Kyc_core

let at = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

let event kind : Kyb_event.t =
  { id = Event_id.of_string "kyb-1"
  ; entity_id = Entity_id.of_string "ent-1"
  ; at
  ; actor = Provider "provider-a"
  ; note = None
  ; kind
  }
;;

let%test_unit "bin_io round trip" =
  let kinds : Kyb_event.Kind.t list =
    [ Opened Kyb_fixtures.entity
    ; Parties_set Kyb_fixtures.parties
    ; Party_case_changed
        { party = Customer_id.of_string "alice"
        ; status = Blocked { reason = "r"; previous = Approved }
        }
    ; Source_of_funds_declared Kyb_fixtures.source_of_funds
    ; Status_changed
        { transition = Request_information [ Source_of_funds; Explanation "why" ]
        ; round = Some 1
        }
    ; Risk_assessed (Kyb_risk.assess Kyb_risk.Model.default Kyb_risk.Inputs.baseline ~at)
    ; Document_removed (Document_id.of_string "doc-1")
    ]
  in
  List.iter kinds ~f:(fun kind ->
    let e = event kind in
    [%test_result: Kyb_event.t]
      (Binable.of_string
         (module Kyb_event.Stable.V1)
         (Binable.to_string (module Kyb_event.Stable.V1) e))
      ~expect:e)
;;

(* A change here means old KYB event logs no longer read: add a V2 instead. *)
let%expect_test "stable digests" =
  print_endline [%bin_digest: Kyb_event.Stable.V1.t];
  print_endline [%bin_digest: Kyb_audit.Entry.Stable.V1.t];
  print_endline [%bin_digest: Entity_id.Stable.V1.t];
  print_endline [%bin_digest: Document_id.Stable.V1.t];
  print_endline [%bin_digest: Legal_entity.Stable.V1.t];
  print_endline [%bin_digest: Kyb_party.Stable.V1.t];
  print_endline [%bin_digest: Kyb_risk.Assessment.Stable.V1.t];
  [%expect {|
    0fe5e688ded47f4cec552eda7a8e2fbd
    5cd040d35ae54568274b06ac38e98a56
    48303bce60ee41187f4111e24fb5ce82
    bc22d6ed75e70cfa193185f0effdff6c
    15b40159f9868b4789db534a6eafe4b5
    307c9ffa8b904cf0c7476ec2ca86cb13
    2f65ddd37acc2cc354ba81556999a914
    |}]
;;
