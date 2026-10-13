(** ISO 3166-1 alpha-2 country code, e.g. [DE]. Only the shape (two ASCII capital letters)
    is checked; whether a code is assigned, or allowed for onboarding, is policy for the
    caller. *)

open! Core
include String_id.S
