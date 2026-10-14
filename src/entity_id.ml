open! Core

(* See customer_id.ml: the uuid tags the bin_shape and is part of the persisted format. *)
include
  (val String_id.make
         ~module_name:"Kyc_core.Entity_id"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "558ab03c-8c7f-48c9-bea5-9a7b098f4448")
         ())
