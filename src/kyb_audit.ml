open! Core

module Change  = struct
  module Stable  = struct
    module V1  = struct
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
        ; round : int 
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

  let visibility : t -> Visibility.Class.t = function
    (* A block's reason can reveal a sanctions match or a report. *)
    | Status { from = Blocked _; _ } | Status { to_ = Blocked _; _ } -> Restricted
    | Party_case_changed { status = Blocked _; _ } -> Restricted
    | Opened
    | Entity_updated _
    | Parties_set _
    | Document_added _
    | Document_removed _
    | Source_of_funds_declared
    | Status _ -> Public
    (* A linked person's own verification outcome is theirs, not the applicant's. *)
    | Party_case_changed _ | Risk_assessed _ | Senior_approval | Periodic_review_completed
      -> Internal
  ;;
end

module Entry  = struct
  module Stable  = struct
    module V1  = struct
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

(* Newest first, so [add] is O(1); [entries] reverses. *)
type t = Entry.t list [@@deriving compare, equal, sexp_of]

let empty = []
let add t entry = entry :: t
let entries t = List.rev t

let for_audience t (audience : Visibility.Audience.t) =
  List.filter_map (entries t) ~f:(fun (e : Entry.t) ->
    if not (Visibility.can_see audience (Change.visibility e.change))
    then None
    else (
      match audience with
      (* Staff notes are internal even on public changes. *)
      | Customer -> Some { e with why = None }
      | Support | Compliance -> Some e))
;;

let length = List.length
