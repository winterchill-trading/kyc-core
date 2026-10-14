open! Core

(* See customer_id.ml: the uuid tags the bin_shape and is part of the persisted format. *)
include
  (val String_id.make
         ~module_name:"Kyc_core.Document_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "31fac4f7-de9e-4d31-a615-03837f2b20bc")
         ())
