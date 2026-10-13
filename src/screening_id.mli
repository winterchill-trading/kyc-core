(** Identifier of one sanctions/PEP screening run.

    Non-empty, no surrounding whitespace (checked by [of_string] and [t_of_sexp], not by
    bin_io). *)

open! Core
include String_id.S
