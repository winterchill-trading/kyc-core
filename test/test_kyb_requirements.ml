open! Core
open Kyc_core
module R = Kyb_requirements

let entity ?(country = "DE") form =
  Legal_entity.create
    ~legal_name:"Example GmbH"
    ~jurisdiction:(Country.of_string country)
    ~legal_form:form
;;

let party ?(chains = []) id roles : Kyb_party.t =
  { id = Customer_id.of_string id; roles; chains }
;;

let show ?(rules = R.default) ?(parties = []) e =
  print_s [%sexp (R.required rules e parties : Kyb_document.Requirement.t list)]
;;

let%expect_test "documents by legal form" =
  List.iter Legal_form.standard ~f:(fun form ->
    print_s
      [%message
        ""
          ~_:(form : Legal_form.t)
          ~_:
            (R.required R.default (entity form) []
             |> List.map ~f:(fun (r : Kyb_document.Requirement.t) -> r.kind)
             : Kyb_document.Kind.t list)]);
  [%expect
    {|
    (Sole_trader (Register_extract))
    (General_partnership (Register_extract Partnership_agreement))
    (Limited_partnership (Register_extract Partnership_agreement))
    (Private_limited_company
     (Certificate_of_incorporation Articles_of_association Shareholder_register
      Register_of_directors))
    (Public_limited_company
     (Certificate_of_incorporation Register_extract Articles_of_association
      Register_of_directors))
    (Cooperative (Register_extract Articles_of_association))
    (Association (Register_extract Articles_of_association))
    (Foundation (Register_extract Foundation_charter))
    (Trust (Trust_deed))
    |}]
;;

let rules =
  { R.default with
    jurisdictions =
      [ { country = Country.of_string "GB"
        ; covered = true
        ; legal_forms = [ Private_limited_company; Public_limited_company ]
        ; extra_documents = [ Proof_of_registered_address ]
        ; monthly_limit = Some { currency = "GBP"; amount_minor = 150_000_000 }
        }
      ; { country = Country.of_string "XX"
        ; covered = false
        ; legal_forms = []
        ; extra_documents = []
        ; monthly_limit = None
        }
      ]
  ; unlisted_covered = false
  }
;;

let%expect_test "jurisdiction extras, representatives and layered ownership" =
  let layered : Ownership.Chain.t =
    { stake_bps = 10_000
    ; via =
        [ { name = "Holdco"
          ; jurisdiction = Country.of_string "GB"
          ; legal_form = Private_limited_company
          ; registration_number = None
          ; stake_bps = 10_000
          ; nominee = false
          ; bearer_shares = false
          }
        ]
    }
  in
  show
    ~rules
    ~parties:
      [ party "rep" [ Authorised_representative ]
      ; party "dir" [ Authorised_representative; Director ]
      ; party "own" [ Shareholder ] ~chains:[ layered ]
      ]
    (entity ~country:"GB" Private_limited_company);
  [%expect
    {|
    (((kind Certificate_of_incorporation) (party ()))
     ((kind Articles_of_association) (party ()))
     ((kind Shareholder_register) (party ()))
     ((kind Register_of_directors) (party ()))
     ((kind Ownership_chart) (party ()))
     ((kind Proof_of_registered_address) (party ()))
     ((kind Power_of_attorney) (party (rep))))
    |}]
;;

let%expect_test "coverage" =
  let check country form =
    print_s [%sexp (R.check_entity rules (entity ~country form) : unit Or_error.t)]
  in
  check "GB" Private_limited_company;
  check "GB" Trust;
  check "XX" Private_limited_company;
  check "DE" Private_limited_company;
  [%expect
    {|
    (Ok ())
    (Error
     ("legal form not accepted in this jurisdiction" (jurisdiction GB)
      (legal_form Trust)))
    (Error ("jurisdiction not covered" (jurisdiction XX)))
    (Error ("jurisdiction not covered" (jurisdiction DE)))
    |}]
;;

let%expect_test "document metadata validation" =
  let doc file_name content_type size_bytes sha256 : Kyb_document.t =
    { id = Document_id.of_string "doc-1"
    ; kind = Register_extract
    ; party = None
    ; file_name
    ; content_type
    ; size_bytes
    ; sha256
    }
  in
  let check d = print_s [%sexp (Kyb_document.validate d : unit Or_error.t)] in
  check (doc "extract.pdf" "application/pdf" 1024 (Some (String.make 64 'a')));
  check (doc "../x.pdf" "text/html" 0 (Some "ABC"));
  check (doc "big.png" "image/png" (Kyb_document.max_size_bytes + 1) None);
  [%expect
    {|
    (Ok ())
    (Error
     ("invalid document" (id doc-1)
      (errors
       ("invalid file name"
        ("content type not accepted" (content_type text/html))
        ("size out of range" (size_bytes 0)) "invalid sha256"))))
    (Error
     ("invalid document" (id doc-1)
      (errors (("size out of range" (size_bytes 20971521))))))
    |}]
;;

let%expect_test "source of funds validation" =
  let sof sources description : Source_of_funds.t =
    { sources; description; monthly_volume = Up_to_100k; counterparty_countries = [] }
  in
  let check s = print_s [%sexp (Source_of_funds.validate s : unit Or_error.t)] in
  check (sof [ Operating_revenue ] "Software licences sold to EU customers");
  check (sof [] "x");
  check (sof [ Owner_equity; Owner_equity ] "x");
  check (sof [ Owner_equity ] "  ");
  [%expect
    {|
    (Ok ())
    (Error "no source of funds declared")
    (Error "repeated source of funds")
    (Error "description must be 1 to 2000 characters")
    |}]
;;
