(** Business case builders shared by the KYB tests. Drafts are numbered when applied, so
    ids and times follow application order. *)

open! Core
open Kyc_core

val entity_id : Entity_id.t
val op_a : Actor.t
val op_b : Actor.t
val provider : Actor.t
val t0 : Time_ns.t
val reset : unit -> unit

type draft = int -> Kyb_event.t

val number : draft -> Kyb_event.t
val event : ?actor:Actor.t -> ?note:string -> ?id:string -> Kyb_event.Kind.t -> draft

val status
  :  ?actor:Actor.t
  -> ?note:string
  -> ?round:int
  -> Kyb_status.Transition.t
  -> draft

val party_case : string -> Status.t -> draft
val assessed : ?inputs:Kyb_risk.Inputs.t -> unit -> draft

(** A UK private company, registered, with an address and an industry. *)
val entity : Legal_entity.t

(** alice: director owning 60 %; bob: 40 % through a Luxembourg holding; carol: authorised
    representative. *)
val parties : Kyb_party.t list

val document : ?party:string -> string -> Kyb_document.Kind.t -> draft

(** Every document {!entity} and {!parties} need. *)
val all_documents : draft list

val source_of_funds : Source_of_funds.t
val apply_all : Kyb_case.t -> draft list -> Kyb_case.t

(** Opened with only the three opening fields. *)
val opened : unit -> Kyb_case.t

(** Complete draft, ready to submit. *)
val complete : unit -> Kyb_case.t

(** Submitted, the three people approved and a baseline assessment recorded. *)
val ready_for_approval : ?inputs:Kyb_risk.Inputs.t -> unit -> Kyb_case.t

val show_case : Kyb_case.t -> unit
val show_follow_ups : Kyb_case.t -> now:string -> unit
