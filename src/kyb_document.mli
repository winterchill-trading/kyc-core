(** Metadata of a document supplied for a business verification: what it is, whom it is
    about, and the file's name, type, size and hash. The file itself is stored elsewhere
    (an object store or the provider); the case never holds content. *)

open! Core

module Kind : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Certificate_of_incorporation
        | Register_extract
        | Articles_of_association
        | Shareholder_register
        | Register_of_directors
        | Ownership_chart
        | Proof_of_registered_address
        | Partnership_agreement
        | Trust_deed
        | Foundation_charter
        | Board_resolution
        | Power_of_attorney
        | Financial_statements
        | Source_of_funds_evidence
        | Bank_statement
        | Other of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Certificate_of_incorporation
    | Register_extract (** recent extract from the commercial or company register *)
    | Articles_of_association
    | Shareholder_register
    | Register_of_directors
    | Ownership_chart (** structure chart down to the natural persons *)
    | Proof_of_registered_address
    | Partnership_agreement
    | Trust_deed
    | Foundation_charter
    | Board_resolution (** authorising the relationship and its representatives *)
    | Power_of_attorney (** for an authorised representative *)
    | Financial_statements
    | Source_of_funds_evidence
    | Bank_statement
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]

  include Comparable.S with type t := t
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Document_id.Stable.V1.t
      ; kind : Kind.Stable.V1.t
      ; party : Customer_id.Stable.V1.t option
      ; file_name : string
      ; content_type : string
      ; size_bytes : int
      ; sha256 : string option
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : Document_id.t
  ; kind : Kind.t
  ; party : Customer_id.t option (** the linked person it is about, if any *)
  ; file_name : string
  ; content_type : string (** e.g. ["application/pdf"] *)
  ; size_bytes : int
  ; sha256 : string option (** lowercase hex digest of the stored file *)
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

(** [application/pdf], [image/png], [image/jpeg]. *)
val accepted_content_types : string list

(** 20 MiB. *)
val max_size_bytes : int

(** Non-blank file name of at most 255 bytes without path separators, an accepted content
    type, size from 1 byte to {!max_size_bytes}, and a 64-character lowercase hex hash
    when given. *)
val validate : t -> unit Or_error.t

type document = t

module Requirement : sig
  (** A document the case needs: a kind, about the entity ([party = None]) or about one
      linked person. *)
  module Stable : sig
    module V1 : sig
      type t =
        { kind : Kind.Stable.V1.t
        ; party : Customer_id.Stable.V1.t option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { kind : Kind.t
    ; party : Customer_id.t option
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  include Comparable.S with type t := t

  val met_by : t -> document:document -> bool
end
