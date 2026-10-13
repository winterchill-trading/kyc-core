(** Name matching for sanctions, PEP and adverse-media screening.

    [normalize] casefolds, folds Latin diacritics to their base letters (ä -> a, ß -> ss,
    ø -> o, ...), turns punctuation into spaces and drops honorifics and legal-form tokens
    ("mr", "dr", "gmbh", "ltd", ...). The score is the larger of Jaro-Winkler on the full
    normalized names and on the token-sorted names, so "Example, Ada" matches "Ada
    Example".

    Not covered: transliteration of non-Latin scripts (Cyrillic, Arabic, CJK...); such
    names are compared as-is after casefolding of ASCII letters. A matcher only proposes
    hits: whether a hit is the customer is an operator's decision. *)

open! Core

val normalize : string -> string
val tokens : string -> string list

(** Jaro similarity, 0.0 to 1.0. *)
val jaro : string -> string -> float

(** Jaro-Winkler with prefix scale 0.1 over at most 4 characters. *)
val jaro_winkler : string -> string -> float

(** [max (jaro_winkler full) (jaro_winkler token_sorted)] of the normalized names. *)
val score : string -> string -> float

(** 0.85; tune per list against reviewed outcomes. *)
val default_threshold : float

val is_candidate : ?threshold:float -> string -> string -> bool
