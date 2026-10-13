(** Event builders shared by the case tests. Drafts are numbered when applied, so ids and
    times follow application order (OCaml evaluates list literals right to left). *)

open! Core
open Kyc_core

val customer : Customer_id.t
val op_a : Actor.t
val op_b : Actor.t
val provider : Actor.t
val t0 : Time_ns.t
val reset : unit -> unit

type draft = int -> Event.t

val number : draft -> Event.t
val event : ?actor:Actor.t -> ?note:string -> ?id:string -> Event.Kind.t -> draft
val verified : Verification.Step.t -> draft
val status : ?actor:Actor.t -> ?note:string -> Status.Transition.t -> draft
val assessed : ?inputs:Risk.Inputs.t -> ?override:Risk.Band.t * string -> unit -> draft

val screened
  :  ?reason:Screening.Screen_reason.t
  -> string
  -> Screening.Hit.t list
  -> draft

val hit : Screening.List_source.t -> string -> Screening.Hit.t
val review : ?actor:Actor.t -> string -> string -> Screening.Review_action.t -> draft

(** Applies drafts, printing errors and keeping the case on error. *)
val apply_all : Kyc_case.t -> draft list -> Kyc_case.t

(** An opened case (and counters reset). *)
val opened : unit -> Kyc_case.t

(** Verified to T2, screened clear, assessed; ready to [Approve]. *)
val ready_for_approval : ?inputs:Risk.Inputs.t -> unit -> Kyc_case.t

val show_case : Kyc_case.t -> unit
val show_follow_ups : Kyc_case.t -> now:string -> unit
