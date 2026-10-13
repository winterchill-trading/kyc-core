open! Core

module Step = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Email
        | Mobile
        | Identity_document
        | Liveness
        | Address_proof
        | Source_of_funds
      [@@deriving bin_io, compare, equal, hash, sexp, enumerate]
    end
  end

  type t = Stable.V1.t =
    | Email
    | Mobile
    | Identity_document
    | Liveness
    | Address_proof
    | Source_of_funds
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

  include Comparable.Make (Stable.V1)
end

module State = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Pending
        | Sent
        | Verified of Timestamp.Stable.V1.t
        | Failed of string
        | Expired
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Pending
    | Sent
    | Verified of Timestamp.t
    | Failed of string
    | Expired
  [@@deriving compare, equal, hash, sexp, quickcheck]

  let is_verified = function
    | Verified _ -> true
    | Pending | Sent | Failed _ | Expired -> false
  ;;
end

let transition ~(from : State.t) ~(to_ : State.t) =
  let legal =
    match from, to_ with
    | Pending, (Sent | Verified _ | Failed _) -> true
    | Sent, (Sent | Verified _ | Failed _ | Expired) -> true
    | Verified _, (Expired | Failed _) -> true
    | (Failed _ | Expired), (Pending | Sent) -> true
    | Pending, (Pending | Expired)
    | Verified _, (Pending | Sent | Verified _)
    | Sent, Pending
    | (Failed _ | Expired), (Verified _ | Failed _ | Expired) -> false
  in
  if legal
  then Ok to_
  else
    Or_error.error_s
      [%message "illegal verification transition" (from : State.t) (to_ : State.t)]
;;
