open! Core

module Originator = struct
  module Stable = struct
    module V1 = struct
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

  include Stable.V1
end

module Beneficiary = struct
  module Stable = struct
    module V1 = struct
      type t =
        { name : string Pii.Stable.V1.t
        ; dlt_address : string option
        ; account_number : string option
        ; lei : string option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  include Stable.V1
end

module Counterparty = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Provider of string
        | Self_hosted of { ownership_verified : bool }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { transfer_ref : string
      ; outgoing : bool
      ; amount_eur_minor : int
      ; originator : Originator.Stable.V1.t
      ; beneficiary : Beneficiary.Stable.V1.t
      ; counterparty : Counterparty.Stable.V1.t
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

include Stable.V1

let blank s = String.is_empty (String.strip s)

let missing t =
  let o = t.originator
  and b = t.beneficiary in
  let has_ref dlt acct = Option.is_some dlt || Option.is_some acct in
  let identified =
    (Option.is_some o.address
     && Option.is_some o.official_document_number
     && Option.is_some o.customer_number)
    || Option.is_some o.date_and_place_of_birth
  in
  List.filter_opt
    [ Option.some_if (blank (Pii.reveal o.name)) "originator.name"
    ; Option.some_if
        (not (has_ref o.dlt_address o.account_number))
        "originator.dlt_address_or_account"
    ; Option.some_if
        (not identified)
        "originator.address_document_and_customer_number_or_birth_date_and_place"
    ; Option.some_if (blank (Pii.reveal b.name)) "beneficiary.name"
    ; Option.some_if
        (not (has_ref b.dlt_address b.account_number))
        "beneficiary.dlt_address_or_account"
    ]
;;

let self_hosted_threshold_eur_minor = 100_000

let ownership_check_required t =
  match t.counterparty with
  | Self_hosted { ownership_verified } ->
    t.amount_eur_minor > self_hosted_threshold_eur_minor && not ownership_verified
  | Provider _ -> false
;;

module Action = struct
  type t =
    | Proceed
    | Await_ownership_proof
    | Missing_data of string list
  [@@deriving compare, equal, sexp]
end

let decide t : Action.t =
  match missing t with
  | _ :: _ as fields -> Missing_data fields
  | [] -> if ownership_check_required t then Await_ownership_proof else Proceed
;;
