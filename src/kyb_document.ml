open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Certificate_of_incorporation
        | Register_extract
        | Articles_of_association
        | Shareholder_register
        | Register_of_directors
        | Ownership_chart
        | Proof_of_registered_address
        | Partnership_agreement
        | Trust_deed
        | Foundation_charter
        | Board_resolution
        | Power_of_attorney
        | Financial_statements
        | Source_of_funds_evidence
        | Bank_statement
        | Other of string
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  module T = struct
    type t = Stable.V1.t =
      | Certificate_of_incorporation
      | Register_extract
      | Articles_of_association
      | Shareholder_register
      | Register_of_directors
      | Ownership_chart
      | Proof_of_registered_address
      | Partnership_agreement
      | Trust_deed
      | Foundation_charter
      | Board_resolution
      | Power_of_attorney
      | Financial_statements
      | Source_of_funds_evidence
      | Bank_statement
      | Other of string
    [@@deriving compare, equal, hash, sexp, quickcheck]
  end

  include T
  include Comparable.Make (T)
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Document_id.Stable.V1.t
      ; kind : Kind.Stable.V1.t
      ; party : Customer_id.Stable.V1.t option
      ; file_name : string
      ; content_type : string
      ; size_bytes : int
      ; sha256 : string option
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : Document_id.t
  ; kind : Kind.t
  ; party : Customer_id.t option
  ; file_name : string
  ; content_type : string
  ; size_bytes : int
  ; sha256 : string option
  }
[@@deriving compare, equal, hash, sexp, quickcheck]

let accepted_content_types = [ "application/pdf"; "image/png"; "image/jpeg" ]
let max_size_bytes = 20 * 1024 * 1024

let validate t =
  let name = t.file_name in
  let errors =
    List.filter_opt
      [ Option.some_if
          (String.is_empty (String.strip name)
           || String.length name > 255
           || String.exists name ~f:(fun c ->
             Char.equal c '/' || Char.equal c '\\' || Char.to_int c < 0x20))
          [%message "invalid file name"]
      ; Option.some_if
          (not (List.mem accepted_content_types t.content_type ~equal:String.equal))
          [%message "content type not accepted" ~content_type:(t.content_type : string)]
      ; Option.some_if
          (t.size_bytes < 1 || t.size_bytes > max_size_bytes)
          [%message "size out of range" ~size_bytes:(t.size_bytes : int)]
      ; Option.bind t.sha256 ~f:(fun h ->
          Option.some_if
            (String.length h <> 64
             || String.exists h ~f:(fun c ->
               not (Char.is_digit c || Char.between c ~low:'a' ~high:'f')))
            [%message "invalid sha256"])
      ]
  in
  match errors with
  | [] -> Ok ()
  | errors ->
    Or_error.error_s
      [%message "invalid document" ~id:(t.id : Document_id.t) (errors : Sexp.t list)]
;;

type document = t

module Requirement = struct
  module Stable = struct
    module V1 = struct
      type t =
        { kind : Kind.Stable.V1.t
        ; party : Customer_id.Stable.V1.t option
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  module T = struct
    type t = Stable.V1.t =
      { kind : Kind.t
      ; party : Customer_id.t option
      }
    [@@deriving compare, equal, hash, sexp, quickcheck]
  end

  include T
  include Comparable.Make (T)

  let met_by t ~(document : document) =
    Kind.equal t.kind document.kind
    && [%equal: Customer_id.t option] t.party document.party
  ;;
end
