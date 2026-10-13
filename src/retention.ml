open! Core

module Legal_hold = struct
  module Stable = struct
    module V1 = struct
      type t =
        { until : Timestamp.Stable.V1.t
        ; reason : string
        ; authority : string
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

let max_hold_extension = Time_ns.Span.of_day (Float.of_int (5 * 365))

let base ~ended ~refused ~retention =
  match List.filter_opt [ ended; refused ] with
  | [] -> None
  | ts ->
    List.max_elt ts ~compare:Time_ns.compare
    |> Option.map ~f:(fun t -> Time_ns.add t retention)
;;

let retain_until ~ended ~refused ~retention ~(hold : Legal_hold.t option) =
  match base ~ended ~refused ~retention, hold with
  | None, _ -> None
  | Some b, None -> Some b
  | Some b, Some h -> Some (Time_ns.max b h.until)
;;

let check_hold (h : Legal_hold.t) ~base =
  if String.is_empty (String.strip h.reason) || String.is_empty (String.strip h.authority)
  then Or_error.error_s [%message "a legal hold needs a reason and an authority"]
  else (
    match base with
    | None -> Ok ()
    | Some b when Time_ns.( <= ) h.until b ->
      Or_error.error_s
        [%message
          "a legal hold must end after the retention date"
            ~retention_ends:(b : Timestamp.t)]
    | Some b when Time_ns.( > ) h.until (Time_ns.add b max_hold_extension) ->
      Or_error.error_s
        [%message
          "a legal hold extends retention by at most 5 years"
            ~retention_ends:(b : Timestamp.t)
            ~until:(h.until : Timestamp.t)]
    | Some _ -> Ok ())
;;
