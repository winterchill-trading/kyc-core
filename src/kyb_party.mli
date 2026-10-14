(** A natural person linked to a business applicant: a director, an authorised
    representative, an owner, someone in control by other means, a senior managing
    official, or a party to a trust. Each party is identified by the {!Customer_id.t} of
    their own {!Kyc_case}, where their identity, screening and PEP status live; the
    business case only records the link, the roles and the ownership chains. *)

open! Core

module Role : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Director
        | Authorised_representative
        | Shareholder
        | Control_by_other_means of string
        | Senior_managing_official
        | Settlor
        | Trustee
        | Protector
        | Beneficiary
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Director (** member of the management body *)
    | Authorised_representative (** may act for the entity, e.g. under a power of attorney *)
    | Shareholder (** holds shares or voting rights, directly or through chains *)
    | Control_by_other_means of string
    (** e.g. a veto right or the power to appoint directors; the string says how *)
    | Senior_managing_official
    (** recorded as beneficial owner when no owner or controller can be identified *)
    | Settlor
    | Trustee
    | Protector
    | Beneficiary
  [@@deriving compare, equal, hash, sexp, quickcheck]

  (** Settlor, trustee, protector or beneficiary. *)
  val is_trust_role : t -> bool
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Customer_id.Stable.V1.t
      ; roles : Role.Stable.V1.t list
      ; chains : Ownership.Chain.Stable.V1.t list
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : Customer_id.t (** the person's own KYC case *)
  ; roles : Role.t list
  ; chains : Ownership.Chain.t list (** only for a [Shareholder] *)
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** Sum of the effective shares of the chains. *)
val effective_share : t -> Ownership.Share.t

(** Why someone is a beneficial owner. *)
module Basis : sig
  type t =
    | Ownership of Ownership.Share.t (** at or above the threshold *)
    | Control of string
    | Trust_role of Role.t
    | Senior_managing_official (** no owner or controller identified *)
  [@@deriving compare, equal, sexp_of]
end

(** Beneficial owners, in party order, with every basis that applies:

    - ownership at or above [threshold_bps] (2500 under AMLR Art. 52) summed over chains;
    - control by other means, whatever the share;
    - for a trust, every settlor, trustee, protector and beneficiary;
    - only when none of those exists, the senior managing officials (AMLD Art. 3(6)(a)(ii)). *)
val beneficial_owners : Legal_form.t -> t list -> threshold_bps:int -> (t * Basis.t list) list

(** At least one role, no repeated role, chains exactly when the party is a
    [Shareholder], valid chains. *)
val validate : t -> unit Or_error.t

(** Every party valid, at most {!max_parties}, no id twice, and effective shares summing
    to at most 100%. *)
val validate_all : t list -> unit Or_error.t

(** 50. *)
val max_parties : int

(** Parties whose own KYC case must be approved before the business case is: beneficial
    owners, directors and authorised representatives. *)
val needs_approved_case : Legal_form.t -> t list -> threshold_bps:int -> Customer_id.t list

val has_role : t -> Role.t -> bool
