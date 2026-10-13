(** Identifier of an applied event. For provider events this is the provider's own event
    or webhook delivery id, so a redelivered webhook is recognised and applied once.

    Non-empty, no surrounding whitespace (checked by [of_string] and [t_of_sexp], not by
    bin_io). *)

open! Core
include String_id.S
