(** Identifier of a document record in a business verification case. The file itself is
    stored elsewhere; the case keeps only metadata under this id.

    Non-empty, no surrounding whitespace (checked by [of_string] and [t_of_sexp], not by
    bin_io). *)

open! Core
include String_id.S
