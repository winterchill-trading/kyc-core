open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Pep
        | Family_member
        | Close_associate
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Pep
    | Family_member
    | Close_associate
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

module Scope = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Domestic
        | Foreign
        | Intl_org
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Domestic
    | Foreign
    | Intl_org
  [@@deriving compare, equal, hash, sexp, quickcheck, enumerate]
end

(* AMLR Art. 45: at least 12 months after the person leaves the function. *)
let minimum_edd_after_end = 12

let validate
  ~kind
  ~scope
  ~function_
  ~started
  ~(ended : Date.t option)
  ~(edd_until : Date.t option)
  =
  match ended, edd_until with
  | Some e, _ when Date.( < ) e started ->
    Or_error.error_s
      [%message "PEP function ends before it starts" (started : Date.t) (e : Date.t)]
  | None, Some _ ->
    Or_error.error_s [%message "edd_until is only set once the function has ended"]
  | Some e, None ->
    Ok
      ( kind
      , scope
      , function_
      , started
      , ended
      , Some (Date.add_months e minimum_edd_after_end) )
  | Some e, Some u ->
    let minimum = Date.add_months e minimum_edd_after_end in
    if Date.( < ) u minimum
    then
      Or_error.error_s
        [%message
          "edd_until is earlier than 12 months after the end"
            (u : Date.t)
            (minimum : Date.t)]
    else Ok (kind, scope, function_, started, ended, edd_until)
  | None, None -> Ok (kind, scope, function_, started, ended, edd_until)
;;

module Stable = struct
  module V1 = struct
    type t =
      { kind : Kind.Stable.V1.t
      ; scope : Scope.Stable.V1.t
      ; function_ : string
      ; started : Date.Stable.V1.t
      ; ended : Date.Stable.V1.t option
      ; edd_until : Date.Stable.V1.t option
      }
    [@@deriving bin_io, compare, equal, hash, sexp]

    let t_of_sexp sexp =
      let { kind; scope; function_; started; ended; edd_until } = t_of_sexp sexp in
      match validate ~kind ~scope ~function_ ~started ~ended ~edd_until with
      | Ok (kind, scope, function_, started, ended, edd_until) ->
        { kind; scope; function_; started; ended; edd_until }
      | Error e -> of_sexp_error (Error.to_string_hum e) sexp
    ;;
  end
end

type t = Stable.V1.t =
  { kind : Kind.t
  ; scope : Scope.t
  ; function_ : string
  ; started : Date.t
  ; ended : Date.t option
  ; edd_until : Date.t option
  }
[@@deriving compare, equal, hash, sexp_of]

let t_of_sexp = Stable.V1.t_of_sexp

let create ~kind ~scope ~function_ ~started ?ended ?edd_until () =
  let%map.Or_error kind, scope, function_, started, ended, edd_until =
    validate ~kind ~scope ~function_ ~started ~ended ~edd_until
  in
  { kind; scope; function_; started; ended; edd_until }
;;

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind kind = Kind.quickcheck_generator
  and scope = Scope.quickcheck_generator
  and function_ = String.quickcheck_generator
  and started = Date.gen_incl (Date.of_string "1970-01-01") (Date.of_string "2030-01-01")
  and length = Int.gen_incl (-1) 5000 in
  let ended = Option.some_if (length >= 0) (Date.add_days started (Int.max length 0)) in
  match create ~kind ~scope ~function_ ~started ?ended () with
  | Ok t -> return t
  | Error e -> Error.raise e
;;

let quickcheck_observer =
  Quickcheck.Observer.unmap [%quickcheck.observer: string * Date.t] ~f:(fun t ->
    t.function_, t.started)
;;

let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

let extend_edd t ~until =
  match t.edd_until with
  | None -> Or_error.error_s [%message "the function is still held; nothing to extend"]
  | Some current when Date.( < ) until current ->
    Or_error.error_s
      [%message "EDD can be extended, not shortened" (current : Date.t) (until : Date.t)]
  | Some _ -> Ok { t with edd_until = Some until }
;;

let edd_applies t ~on =
  match t.edd_until with
  | None -> Date.( >= ) on t.started
  | Some until -> Date.( >= ) on t.started && Date.( <= ) on until
;;
