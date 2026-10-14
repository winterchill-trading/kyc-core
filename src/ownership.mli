(** Ownership chains from a natural person to a business applicant, and the effective
    share each chain gives.

    A {!Chain} reads from the person outwards: the person holds [stake_bps] of the first
    intermediate in [via], which holds its own [stake_bps] of the next, and so on; the
    last one holds its [stake_bps] of the applicant. With [via = []] the person holds
    [stake_bps] of the applicant directly. A person's effective share is the sum over
    their chains of the product of the stakes along each (the multiplication approach of
    AMLR Art. 52 and FATF R.24 guidance for indirect ownership). *)

open! Core

module Intermediate : sig
  (** A legal entity or arrangement between the person and the applicant. *)

  module Stable : sig
    module V1 : sig
      type t =
        { name : string
        ; jurisdiction : Country.Stable.V1.t
        ; legal_form : Legal_form.Stable.V1.t
        ; registration_number : string option
        ; stake_bps : int
        ; nominee : bool
        ; bearer_shares : bool
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { name : string
    ; jurisdiction : Country.t
    ; legal_form : Legal_form.t
    ; registration_number : string option
    ; stake_bps : int
    (** its stake in the next entity towards the applicant, 1 to 10000 *)
    ; nominee : bool (** it holds as a nominee for someone not named *)
    ; bearer_shares : bool (** it has issued bearer shares *)
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Chain : sig
  module Stable : sig
    module V1 : sig
      type t =
        { stake_bps : int
        ; via : Intermediate.Stable.V1.t list
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { stake_bps : int (** the person's own stake, 1 to 10000 *)
    ; via : Intermediate.t list (** nearest the person first; empty when held directly *)
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  val direct : bps:int -> t

  (** Number of intermediates. *)
  val layers : t -> int

  (** Stakes in 1 to 10000, non-blank intermediate names, at most {!max_layers}
      intermediates. *)
  val validate : t -> unit Or_error.t
end

(** 10: deeper structures are refused rather than modelled. *)
val max_layers : int

module Share : sig
  (** An ownership fraction in parts per 10[{^12}]. Products of whole basis points are
      exact through two intermediates; deeper chains round down by at most one part per
      layer, which only matters within 10[{^-12}] of a threshold. *)
  type t = private int [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t

  val zero : t
  val whole : t
  val of_bps : int -> t

  (** Rounded down. *)
  val to_bps : t -> int

  val ( + ) : t -> t -> t

  (** E.g. ["25.5%"], at most four decimals, rounded down. *)
  val to_percent_string : t -> string
end

val effective : Chain.t -> Share.t

module Opacity : sig
  (** How hard the ownership is to see through. Our scale, from EBA/GL/2021/02 2.3 and
      AMLD Annex III (an unusual or excessively complex ownership structure, nominee
      shareholders and bearer shares are higher-risk factors):

      - [Transparent]: owners hold the applicant directly.
      - [Layered]: at least one intermediate.
      - [Complex]: a chain of three or more intermediates, a trust or foundation in a
        chain, or intermediates in two or more countries other than the applicant's.
      - [Opaque]: bearer shares (the applicant's or an intermediate's) or a nominee
        holder. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Transparent
        | Layered
        | Complex
        | Opaque
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Transparent
    | Layered
    | Complex
    | Opaque
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.S with type t := t
end

val opacity : Legal_entity.t -> Chain.t list -> Opacity.t
