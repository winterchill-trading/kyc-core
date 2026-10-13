open! Core

(* The ox Core has no [String_id.Make] functor; [make] takes a caller identity that tags
   the bin_shape, so each id type gets its own digest. Never change the uuid: it is part
   of the persisted format. *)
include
  (val String_id.make
         ~module_name:"Kyc_core.Operator_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "382ad6a2-6266-48af-bd4a-1e7108b55967")
         ())
