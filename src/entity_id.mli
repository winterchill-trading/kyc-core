(** Our identifier for a legal entity (a business applicant), independent of any
    provider's reference and of the ids of the people linked to it.

    Non-empty, no surrounding whitespace (checked by [of_string] and [t_of_sexp], not by
    bin_io). *)

open! Core
include String_id.S
