(** Politically exposed person status (FATF R.12; EU AMLR Arts. 42-46).

    A PEP, a family member or a close associate of one. EDD continues for at least 12
    months after the public function ends ([edd_until]); staff may extend that date but
    never shorten it. *)

open! Core

module Kind : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Pep
        | Family_member
        | Close_associate
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Pep
    | Family_member
    | Close_associate
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Scope : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Domestic
        | Foreign
        | Intl_org (** senior role at an international organisation *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Domestic
    | Foreign
    | Intl_org
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Stable : sig
  module V1 : sig
    type t = private
      { kind : Kind.Stable.V1.t
      ; scope : Scope.Stable.V1.t
      ; function_ : string (** the public function, e.g. "member of parliament" *)
      ; started : Date.Stable.V1.t
      ; ended : Date.Stable.V1.t option
      ; edd_until : Date.Stable.V1.t option (** [None] while the function is held *)
      }
    [@@deriving bin_io, compare, equal, hash, sexp_of]

    (** Reading checks the same invariant as {!create}. *)
    val t_of_sexp : Sexp.t -> t
  end
end

type t = Stable.V1.t = private
  { kind : Kind.t
  ; scope : Scope.t
  ; function_ : string
  ; started : Date.t
  ; ended : Date.t option
  ; edd_until : Date.t option
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** [edd_until] defaults to [ended] plus 12 months; a given [edd_until] earlier than that,
    or an [ended] before [started], is an error. *)
val create
  :  kind:Kind.t
  -> scope:Scope.t
  -> function_:string
  -> started:Date.t
  -> ?ended:Date.t
  -> ?edd_until:Date.t
  -> unit
  -> t Or_error.t

(** Moves [edd_until] later; refuses to move it earlier or to set it while the function is
    still held. *)
val extend_edd : t -> until:Date.t -> t Or_error.t

(** Whether PEP EDD applies on a date: while the function is held and until [edd_until]. *)
val edd_applies : t -> on:Date.t -> bool

(** months *)
val minimum_edd_after_end : int
