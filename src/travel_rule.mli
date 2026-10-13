(** Travel-rule data for crypto-asset transfers (EU TFR 2023/1113 Art. 14;
    FATF R.16 via R.15). Between providers there is no threshold: every
    transfer carries originator and beneficiary data. For a self-hosted
    address above EUR 1 000 the provider must assess that the customer owns
    or controls it (Art. 14(5) outgoing, Art. 16(2) incoming). Records are
    kept 5 years (Art. 26). *)

open! Core

module Originator : sig
  module Stable : sig
    module V1 : sig
      type t =
        { name : string Pii.Stable.V1.t
        ; dlt_address : string option
        ; account_number : string option
        ; address : Address.Stable.V1.t Pii.Stable.V1.t option
        ; official_document_number : string Pii.Stable.V1.t option
        ; customer_number : string option
        ; date_and_place_of_birth : (Date.Stable.V1.t * string) Pii.Stable.V1.t option
        ; lei : string option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { name : string Pii.t
    ; dlt_address : string option
    ; account_number : string option
    ; address : Address.t Pii.t option
    ; official_document_number : string Pii.t option
    ; customer_number : string option
    ; date_and_place_of_birth : (Date.t * string) Pii.t option
    ; lei : string option
    }
  [@@deriving compare, equal, hash, sexp]
end

module Beneficiary : sig
  module Stable : sig
    module V1 : sig
      type t =
        { name : string Pii.Stable.V1.t
        ; dlt_address : string option
        ; account_number : string option
        ; lei : string option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { name : string Pii.t
    ; dlt_address : string option
    ; account_number : string option
    ; lei : string option
    }
  [@@deriving compare, equal, hash, sexp]
end

module Counterparty : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Provider of string (** the other crypto-asset service provider *)
        | Self_hosted of { ownership_verified : bool }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Provider of string
    | Self_hosted of { ownership_verified : bool }
  [@@deriving compare, equal, hash, sexp]
end

module Stable : sig
  module V1 : sig
    type t =
      { transfer_ref : string
      ; outgoing : bool
      ; amount_eur_minor : int (** EUR value in cents at the time of transfer *)
      ; originator : Originator.Stable.V1.t
      ; beneficiary : Beneficiary.Stable.V1.t
      ; counterparty : Counterparty.Stable.V1.t
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { transfer_ref : string
  ; outgoing : bool
  ; amount_eur_minor : int
  ; originator : Originator.t
  ; beneficiary : Beneficiary.t
  ; counterparty : Counterparty.t
  }
[@@deriving compare, equal, hash, sexp]

(** Names of required fields that are missing: originator name, a DLT
    address or account number, and either (address, official document
    number and customer number) or date and place of birth; beneficiary name
    and a DLT address or account number. *)
val missing : t -> string list

(** EUR 1 000.00 in cents. *)
val self_hosted_threshold_eur_minor : int

val ownership_check_required : t -> bool

module Action : sig
  type t =
    | Proceed
    | Await_ownership_proof
    | Missing_data of string list
    (** execute, reject, return or suspend on a risk basis (TFR Art. 17) *)
  [@@deriving compare, equal, sexp]
end

val decide : t -> Action.t
