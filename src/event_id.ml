open! Core

(* The ox Core has no [String_id.Make] functor; [make] takes a caller identity that tags
   the bin_shape, so each id type gets its own digest. Never change the uuid: it is part
   of the persisted format. *)
include
  (val String_id.make
         ~module_name:"Kyc_core.Event_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "cad108cf-85a4-41f8-860a-3b083ee57a48")
         ())
