;; A UK private company with two owners (one through a Luxembourg holding) and an
;; authorised representative. The reviewer asks for source-of-funds evidence in round 1;
;; round 2 is approved once the three people's own KYC cases are approved.
((id kyb-1) (entity_id ent-1) (at "2026-10-13 09:00:00Z") (actor System) (note ())
 (kind
  (Opened
   ((legal_name "Example Trading Ltd") (registration_number ()) (jurisdiction GB)
    (legal_form Private_limited_company) (incorporated_on ())
    (registered_address ()) (business_address ()) (industry ())
    (bearer_shares false)))))
((id kyb-2) (entity_id ent-1) (at "2026-10-13 09:05:00Z") (actor System) (note ())
 (kind
  (Entity_updated
   ((legal_name "Example Trading Ltd") (registration_number (01234567))
    (jurisdiction GB) (legal_form Private_limited_company)
    (incorporated_on (2019-04-01))
    (registered_address
     (((line1 "1 Example Street") (line2 ()) (city London) (region ())
       (postal_code ("EC1A 1AA")) (country GB))))
    (business_address ()) (industry (62.01)) (bearer_shares false)))))
((id kyb-3) (entity_id ent-1) (at "2026-10-13 09:06:00Z") (actor System) (note ())
 (kind
  (Parties_set
   (((id alice) (roles (Director Shareholder))
     (chains (((stake_bps 6000) (via ())))))
    ((id bob) (roles (Shareholder))
     (chains
      (((stake_bps 10000)
        (via
         (((name "Example Holdings SARL") (jurisdiction LU)
           (legal_form Private_limited_company) (registration_number (B000001))
           (stake_bps 4000) (nominee false) (bearer_shares false))))))))
    ((id carol) (roles (Authorised_representative)) (chains ()))))))
((id kyb-4) (entity_id ent-1) (at "2026-10-13 09:07:00Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-coi) (kind Certificate_of_incorporation) (party ())
    (file_name coi.pdf) (content_type application/pdf) (size_bytes 120000)
    (sha256 ())))))
((id kyb-5) (entity_id ent-1) (at "2026-10-13 09:07:10Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-aoa) (kind Articles_of_association) (party ()) (file_name aoa.pdf)
    (content_type application/pdf) (size_bytes 230000) (sha256 ())))))
((id kyb-6) (entity_id ent-1) (at "2026-10-13 09:07:20Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-reg) (kind Shareholder_register) (party ()) (file_name register.pdf)
    (content_type application/pdf) (size_bytes 80000) (sha256 ())))))
((id kyb-7) (entity_id ent-1) (at "2026-10-13 09:07:30Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-dir) (kind Register_of_directors) (party ()) (file_name directors.pdf)
    (content_type application/pdf) (size_bytes 60000) (sha256 ())))))
((id kyb-8) (entity_id ent-1) (at "2026-10-13 09:07:40Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-chart) (kind Ownership_chart) (party ()) (file_name chart.png)
    (content_type image/png) (size_bytes 40000) (sha256 ())))))
((id kyb-9) (entity_id ent-1) (at "2026-10-13 09:07:50Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-poa) (kind Power_of_attorney) (party (carol)) (file_name poa.pdf)
    (content_type application/pdf) (size_bytes 90000) (sha256 ())))))
((id kyb-10) (entity_id ent-1) (at "2026-10-13 09:08:00Z") (actor System) (note ())
 (kind
  (Source_of_funds_declared
   ((sources (Operating_revenue))
    (description "Software licences sold to business customers")
    (monthly_volume Up_to_100k) (counterparty_countries (GB))))))
((id kyb-11) (entity_id ent-1) (at "2026-10-13 09:10:00Z") (actor System) (note ())
 (kind (Status_changed (transition Submit) (round ()))))
((id evt-20) (entity_id ent-1) (at "2026-10-13 09:10:03Z")
 (actor (Provider provider-a)) (note ())
 (kind (Status_changed (transition Start_review) (round (1)))))
((id evt-21) (entity_id ent-1) (at "2026-10-13 09:40:00Z")
 (actor (Provider provider-a)) (note ("Please evidence the licence revenue"))
 (kind
  (Status_changed
   (transition
    (Request_information
     ((Document ((kind Source_of_funds_evidence) (party ())))
      (Document ((kind Bank_statement) (party ()))))))
   (round (1)))))
;; Redelivered webhook: the same id and content is a no-op.
((id evt-21) (entity_id ent-1) (at "2026-10-13 09:40:00Z")
 (actor (Provider provider-a)) (note ("Please evidence the licence revenue"))
 (kind
  (Status_changed
   (transition
    (Request_information
     ((Document ((kind Source_of_funds_evidence) (party ())))
      (Document ((kind Bank_statement) (party ()))))))
   (round (1)))))
((id kyb-12) (entity_id ent-1) (at "2026-10-14 10:00:00Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-sof) (kind Source_of_funds_evidence) (party ())
    (file_name invoices.pdf) (content_type application/pdf) (size_bytes 300000)
    (sha256 ())))))
((id kyb-13) (entity_id ent-1) (at "2026-10-14 10:01:00Z") (actor System) (note ())
 (kind
  (Document_added
   ((id doc-bank) (kind Bank_statement) (party ()) (file_name statement.pdf)
    (content_type application/pdf) (size_bytes 150000) (sha256 ())))))
((id kyb-14) (entity_id ent-1) (at "2026-10-14 10:02:00Z") (actor System) (note ())
 (kind (Status_changed (transition Submit) (round (2)))))
;; A late provider event about round 1 is refused and skipped.
((id evt-22) (entity_id ent-1) (at "2026-10-14 10:03:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Status_changed (transition (Reject outside_risk_appetite)) (round (1)))))
((id kyb-15) (entity_id ent-1) (at "2026-10-14 10:05:00Z") (actor System) (note ())
 (kind (Party_case_changed (party alice) (status Approved))))
((id kyb-16) (entity_id ent-1) (at "2026-10-14 10:06:00Z") (actor System) (note ())
 (kind (Party_case_changed (party bob) (status Approved))))
((id kyb-17) (entity_id ent-1) (at "2026-10-14 10:07:00Z") (actor System) (note ())
 (kind (Party_case_changed (party carol) (status Approved))))
((id kyb-18) (entity_id ent-1) (at "2026-10-14 10:10:00Z")
 (actor (Operator op-a)) (note ("onboarding"))
 (kind
  (Risk_assessed
   ((inputs
     ((industry Low) (cash_intensive false) (jurisdiction Standard)
      (places_of_business Standard) (owner_countries Standard)
      (ownership Layered) (pep ()) (adverse_media No_findings)
      (products (Fiat_account)) (monthly_volume Up_to_100k)
      (channel Remote_attended) (self_hosted_exposure false) (manual_edd ())))
    (model_version kyb-risk-2026-10) (score 25) (floors ()) (computed_band Low)
    (override ()) (computed_at "2026-10-14 10:10:00Z")))))
((id evt-23) (entity_id ent-1) (at "2026-10-14 11:00:00Z")
 (actor (Provider provider-a)) (note ("All checks passed"))
 (kind (Status_changed (transition Approve) (round (2)))))
