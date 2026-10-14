(** The audit trail of a business case: one entry per change an event caused, with who,
    when and why. Each change has a {!Visibility.Class.t} and {!for_audience} filters the
    trail; the applicant ([Customer] audience) never sees risk, linked people's case
    states or block reasons (tipping-off, AMLR Art. 73). *)

open! Core

module Change : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Opened
        | Entity_updated of Legal_entity.Field.Stable.V1.t list
        | Parties_set of Customer_id.Stable.V1.t list
        | Party_case_changed of
            { party : Customer_id.Stable.V1.t
            ; status : Status.Stable.V1.t
            }
        | Document_added of
            { document_id : Document_id.Stable.V1.t
            ; kind : Kyb_document.Kind.Stable.V1.t
            ; party : Customer_id.Stable.V1.t option
            }
        | Document_removed of Document_id.Stable.V1.t
        | Source_of_funds_declared
        | Status of
            { from : Kyb_status.Stable.V1.t
            ; to_ : Kyb_status.Stable.V1.t
            ; round : int
            }
        | Risk_assessed of
            { score : int
            ; computed_band : Risk.Band.Stable.V1.t
            ; band : Risk.Band.Stable.V1.t
            ; model_version : string
            }
        | Senior_approval
        | Periodic_review_completed
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Opened
    | Entity_updated of Legal_entity.Field.t list
    | Parties_set of Customer_id.t list
    | Party_case_changed of
        { party : Customer_id.t
        ; status : Status.t
        }
    | Document_added of
        { document_id : Document_id.t
        ; kind : Kyb_document.Kind.t
        ; party : Customer_id.t option
        }
    | Document_removed of Document_id.t
    | Source_of_funds_declared
    | Status of
        { from : Kyb_status.t
        ; to_ : Kyb_status.t
        ; round : int (** the round after the change *)
        }
    | Risk_assessed of
        { score : int
        ; computed_band : Risk.Band.t
        ; band : Risk.Band.t
        ; model_version : string
        }
    | Senior_approval
    | Periodic_review_completed
  [@@deriving compare, equal, hash, sexp]

  val visibility : t -> Visibility.Class.t
end

module Entry : sig
  module Stable : sig
    module V1 : sig
      type t =
        { at : Timestamp.Stable.V1.t
        ; actor : Actor.Stable.V1.t
        ; event_id : Event_id.Stable.V1.t
        ; change : Change.Stable.V1.t
        ; why : string option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { at : Timestamp.t
    ; actor : Actor.t
    ; event_id : Event_id.t
    ; change : Change.t
    ; why : string option
    }
  [@@deriving compare, equal, hash, sexp]
end

type t [@@deriving compare, equal, sexp_of]

val empty : t
val add : t -> Entry.t -> t

(** Oldest first. *)
val entries : t -> Entry.t list

(** Oldest first, only what the audience may see; the applicant's copy has no [why]
    notes. *)
val for_audience : t -> Visibility.Audience.t -> Entry.t list

val length : t -> int
