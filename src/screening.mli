(** Sanctions, PEP and adverse-media screening records.

    A screening run has a reason and a list of candidate hits; each hit is
    reviewed on its own:

    {v
      Open | Escalated ------ Take ------------> Under_review
      Open | Under_review | Escalated
             ---------------- Propose_discount -> Under_review (with a proposal)
      Under_review (proposal by A)
             ---------------- Confirm_discount by B <> A -> Discounted
      Open | Under_review | Escalated
             ---------------- Confirm ----------> Confirmed
      Open | Under_review --- Escalate ---------> Escalated
    v}

    Discounting needs two different operators (four eyes). [Discounted] and
    [Confirmed] are final. A screening blocks approval until every hit is
    discounted; a confirmed sanctions hit also blocks the customer (see
    {!Kyc_case}). *)

open! Core

module List_source : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Eu_fsf (** EU consolidated financial sanctions list *)
        | Un_sc (** UN Security Council consolidated list *)
        | Ofac_sdn
        | Uk_sanctions_list
        | Pep_list
        | Adverse_media
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Eu_fsf (** EU consolidated financial sanctions list *)
    | Un_sc (** UN Security Council consolidated list *)
    | Ofac_sdn
    | Uk_sanctions_list
    | Pep_list
    | Adverse_media
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  (** The four sanctions lists; a confirmed hit on one blocks the customer. *)
  val is_sanctions : t -> bool
end

module Screen_reason : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Onboarding
        | List_update (** re-screen of the base against a list delta *)
        | Data_change
        | Periodic
        | Payment (** counterparty of a payment *)
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Onboarding
    | List_update (** re-screen of the base against a list delta *)
    | Data_change
    | Periodic
    | Payment (** counterparty of a payment *)
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Hit : sig
  (** One candidate match. Identified within its screening by [entry_id]. *)

  module Stable : sig
    module V1 : sig
      type t =
        { source : List_source.Stable.V1.t
        ; list_version : string (** the list publication the hit came from *)
        ; entry_id : string (** the list's own id for the entry *)
        ; matched_name : string (** the entry name (public list data) *)
        ; score : int (** 0 to 100 *)
        ; algorithm : string (** e.g. "jaro-winkler-1" *)
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { source : List_source.t
    ; list_version : string
    ; entry_id : string
    ; matched_name : string
    ; score : int
    ; algorithm : string
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Review_action : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Take (** an operator starts reviewing *)
        | Propose_discount of string (** not the customer; reason *)
        | Confirm_discount (** second operator agrees *)
        | Confirm of string (** it is the customer; reason *)
        | Escalate of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Take (** an operator starts reviewing *)
    | Propose_discount of string (** not the customer; reason *)
    | Confirm_discount (** second operator agrees *)
    | Confirm of string (** it is the customer; reason *)
    | Escalate of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module Hit_state : sig
  type t =
    | Open
    | Under_review of
        { by : Operator_id.t
        ; discount_proposal : string option
        }
    | Discounted of
        { reason : string
        ; proposed_by : Operator_id.t
        ; confirmed_by : Operator_id.t
        }
    | Confirmed of
        { by : Operator_id.t
        ; reason : string
        }
    | Escalated of
        { by : Operator_id.t
        ; reason : string
        }
  [@@deriving compare, equal, hash, sexp]

  val is_final : t -> bool
end

type t =
  { id : Screening_id.t
  ; reason : Screen_reason.t
  ; screened_at : Timestamp.t
  ; hits : (Hit.t * Hit_state.t) list
  }
[@@deriving compare, equal, hash, sexp]

(** All hits start [Open]. Duplicate [entry_id]s are an error. *)
val create
  :  id:Screening_id.t
  -> reason:Screen_reason.t
  -> screened_at:Timestamp.t
  -> hits:Hit.t list
  -> t Or_error.t

val review
  :  t
  -> entry_id:string
  -> action:Review_action.t
  -> actor:Actor.t
  -> (t * Hit_state.t) Or_error.t

val hit_state : t -> entry_id:string -> Hit_state.t option

(** Some hit is not [Discounted]. *)
val blocks_approval : t -> bool

(** Hits not yet final. *)
val pending : t -> Hit.t list

val confirmed : t -> Hit.t list
