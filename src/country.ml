open! Core

let validate s =
  if String.length s = 2 && String.for_all s ~f:Char.is_uppercase
  then Ok ()
  else Or_error.error_s [%message "expected an ISO 3166-1 alpha-2 code" (s : string)]
;;

(* See [Customer_id] for the caller identity. *)
include
  (val String_id.make
         ~validate
         ~module_name:"Kyc_core.Country"
         ~include_default_validation:true
         ~caller_identity:
           (Bin_prot.Shape.Uuid.of_string "5e0c1a6e-3b1f-4c55-9a4e-2f7b1d0c9e11")
         ())

(* The default generator filters random strings through [validate], which almost never
   produces two capitals; build valid codes directly. *)
let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map a = Char.gen_uppercase
  and b = Char.gen_uppercase in
  of_string (String.of_char_list [ a; b ])
;;
