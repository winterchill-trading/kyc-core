open! Core

module Intermediate = struct
  module Stable = struct
    module V1 = struct
      type t =
        { name : string
        ; jurisdiction : Country.Stable.V1.t
        ; legal_form : Legal_form.Stable.V1.t
        ; registration_number : string option
        ; stake_bps : int
        ; nominee : bool
        ; bearer_shares : bool
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { name : string
    ; jurisdiction : Country.t
    ; legal_form : Legal_form.t
    ; registration_number : string option
    ; stake_bps : int
    ; nominee : bool
    ; bearer_shares : bool
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

let max_layers = 10

module Chain = struct
  module Stable = struct
    module V1 = struct
      type t =
        { stake_bps : int
        ; via : Intermediate.Stable.V1.t list
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    { stake_bps : int
    ; via : Intermediate.t list
    }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  let direct ~bps = { stake_bps = bps; via = [] }
  let layers t = List.length t.via
  let stake_ok bps = bps >= 1 && bps <= 10_000

  let validate t =
    let errors =
      List.filter_opt
        ([ Option.some_if
             (not (stake_ok t.stake_bps))
             [%message "stake out of range" ~stake_bps:(t.stake_bps : int)]
         ; Option.some_if
             (layers t > max_layers)
             [%message "too many intermediates" ~layers:(layers t : int)]
         ]
         @ List.concat_map t.via ~f:(fun (i : Intermediate.t) ->
           [ Option.some_if
               (not (stake_ok i.stake_bps))
               [%message
                 "intermediate stake out of range"
                   ~name:(i.name : string)
                   ~stake_bps:(i.stake_bps : int)]
           ; Option.some_if
               (String.is_empty (String.strip i.name))
               [%message "intermediate without a name"]
           ]))
    in
    match errors with
    | [] -> Ok ()
    | errors ->
      Or_error.error_s [%message "invalid ownership chain" (errors : Sexp.t list)]
  ;;
end

module Share = struct
  let whole = 1_000_000_000_000
  let zero = 0

  let to_percent_string t =
    (* 1% = 10^10 parts; keep four decimals. *)
    let units = t / 1_000_000 in
    let whole = units / 10_000
    and frac = units mod 10_000 in
    if frac = 0
    then sprintf "%d%%" whole
    else (
      let digits = sprintf "%04d" frac in
      sprintf "%d.%s%%" whole (String.rstrip digits ~drop:(Char.equal '0')))
  ;;

  module T = struct
    type t = int [@@deriving compare, equal, hash]

    (* Printed as a percentage: the raw parts-per-10^12 count is unreadable. *)
    let sexp_of_t t = Sexp.Atom (to_percent_string t)
  end

  include T
  include Comparable.Make_plain (T)

  (* 1 bps = 10^8 parts. Stakes are at most 10^4, so a share times a stake stays below
     10^16, far inside 63 bits. *)
  let of_bps bps = bps * 100_000_000
  let to_bps t = t / 100_000_000
  let ( + ) = Int.( + )
  let scale t ~bps = t * bps / 10_000
end

let effective (c : Chain.t) =
  List.fold c.via ~init:(Share.of_bps c.stake_bps) ~f:(fun share i ->
    Share.scale share ~bps:i.stake_bps)
;;

module Opacity = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Transparent
        | Layered
        | Complex
        | Opaque
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  module T = struct
    type t = Stable.V1.t =
      | Transparent
      | Layered
      | Complex
      | Opaque
    [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
  end

  include T
  include Comparable.Make (T)
end

let opacity (entity : Legal_entity.t) chains =
  let intermediates = List.concat_map chains ~f:(fun (c : Chain.t) -> c.via) in
  let foreign =
    List.filter_map intermediates ~f:(fun i ->
      Option.some_if
        (not (Country.equal i.jurisdiction entity.jurisdiction))
        i.jurisdiction)
    |> List.dedup_and_sort ~compare:Country.compare
  in
  if entity.bearer_shares
     || List.exists intermediates ~f:(fun i -> i.nominee || i.bearer_shares)
  then Opacity.Opaque
  else if List.exists chains ~f:(fun c -> Chain.layers c >= 3)
          || List.exists intermediates ~f:(fun i ->
            match i.legal_form with
            | Trust | Foundation -> true
            | _ -> false)
          || List.length foreign >= 2
  then Complex
  else if not (List.is_empty intermediates)
  then Layered
  else Transparent
;;
