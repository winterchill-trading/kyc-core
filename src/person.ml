open! Core

module Field = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Given_name
        | Family_name
        | Date_of_birth
        | Nationality
        | Residential_address
        | Email
        | Mobile
      [@@deriving bin_io, compare, equal, hash, sexp, quickcheck, enumerate]
    end
  end

  include Stable.V1
  include Comparable.Make (Stable.V1)
end

module Stable = struct
  module V1 = struct
    (* Generators for wrapped fields come from the unversioned modules: the String_id
       stable modules carry no quickcheck functions, and the types are equal. *)
    type t =
      { given_name : string Pii.Stable.V1.t
      ; family_name : string Pii.Stable.V1.t
      ; date_of_birth :
          (Date.Stable.V1.t Pii.Stable.V1.t
          [@quickcheck.generator [%quickcheck.generator: Date.t Pii.t]]
          [@quickcheck.observer [%quickcheck.observer: Date.t Pii.t]]
          [@quickcheck.shrinker [%quickcheck.shrinker: Date.t Pii.t]])
      ; nationality :
          (Country.Stable.V1.t Pii.Stable.V1.t
          [@quickcheck.generator [%quickcheck.generator: Country.t Pii.t]]
          [@quickcheck.observer [%quickcheck.observer: Country.t Pii.t]]
          [@quickcheck.shrinker [%quickcheck.shrinker: Country.t Pii.t]])
      ; residential_address : Address.Stable.V1.t Pii.Stable.V1.t
      ; email : string Pii.Stable.V1.t
      ; mobile : string Pii.Stable.V1.t
      }
    [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
  end
end

include Stable.V1

let changed_fields a b =
  let differs eq get = not (eq (get a) (get b)) in
  List.filter_map Field.all ~f:(fun (field : Field.t) ->
    let changed =
      match field with
      | Given_name -> differs [%equal: string Pii.t] (fun p -> p.given_name)
      | Family_name -> differs [%equal: string Pii.t] (fun p -> p.family_name)
      | Date_of_birth -> differs [%equal: Date.t Pii.t] (fun p -> p.date_of_birth)
      | Nationality -> differs [%equal: Country.t Pii.t] (fun p -> p.nationality)
      | Residential_address ->
        differs [%equal: Address.t Pii.t] (fun p -> p.residential_address)
      | Email -> differs [%equal: string Pii.t] (fun p -> p.email)
      | Mobile -> differs [%equal: string Pii.t] (fun p -> p.mobile)
    in
    Option.some_if changed field)
;;

let age t ~on =
  let dob = Pii.reveal t.date_of_birth in
  let years = Date.year on - Date.year dob in
  let had_birthday =
    Month.( > ) (Date.month on) (Date.month dob)
    || (Month.equal (Date.month on) (Date.month dob) && Date.day on >= Date.day dob)
  in
  if had_birthday then years else years - 1
;;
