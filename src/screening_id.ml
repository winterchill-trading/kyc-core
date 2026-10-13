open! Core

(* The ox Core has no [String_id.Make] functor; [make] takes a caller identity that tags
   the bin_shape, so each id type gets its own digest. Never change the uuid: it is part
   of the persisted format. *)
include
  (val String_id.make
         ~module_name:"Kyc_core.Screening_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "62ac6915-7f9d-4d3e-bd1a-0dbe50b5fb63")
         ())
