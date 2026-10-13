(** Record retention (FATF R.11; AMLR Art. 77): records are kept for the retention period
    after the relationship ends or the application is refused; an authority may extend
    that with a legal hold of at most five more years. After [retain_until] personal data
    is erased; with PII encrypted under a per-customer key, erasure is destroying that
    key. *)

open! Core

module Legal_hold : sig
  module Stable : sig
    module V1 : sig
      type t =
        { until : Timestamp.Stable.V1.t
        ; reason : string
        ; authority : string (** who required it *)
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { until : Timestamp.t
    ; reason : string
    ; authority : string
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

(** 5 years of 365 days. *)
val max_hold_extension : Time_ns.Span.t

(** The later of [ended] and [refused] plus [retention], or the hold's [until] if later;
    [None] while the relationship is live. *)
val retain_until
  :  ended:Timestamp.t option
  -> refused:Timestamp.t option
  -> retention:Time_ns.Span.t
  -> hold:Legal_hold.t option
  -> Timestamp.t option

(** A hold must end after the base retention date and at most {!max_hold_extension} after
    it, and name a reason and an authority. Without a base date (relationship still live)
    only the text is checked. *)
val check_hold : Legal_hold.t -> base:Timestamp.t option -> unit Or_error.t
