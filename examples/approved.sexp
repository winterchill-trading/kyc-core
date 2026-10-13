;; A customer verified to T2, rated Low and approved. PII is written in clear here and
;; printed as <redacted> by kyc-case.
((id evt-1) (customer_id cus-1) (at "2026-10-13 09:00:00Z") (actor System)
 (note ())
 (kind
  (Opened
   ((given_name Ada) (family_name Example) (date_of_birth 1990-06-15)
    (nationality DE)
    (residential_address
     ((line1 "1 Example Street") (line2 ()) (city Berlin) (region ())
      (postal_code (10115)) (country DE)))
    (email ada@example.com) (mobile +4930000000)))))
((id evt-2) (customer_id cus-1) (at "2026-10-13 09:01:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Step_changed (step Email) (state (Verified "2026-10-13 09:01:00Z")))))
((id evt-3) (customer_id cus-1) (at "2026-10-13 09:02:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Step_changed (step Mobile) (state (Verified "2026-10-13 09:02:00Z")))))
((id evt-4) (customer_id cus-1) (at "2026-10-13 09:03:00Z") (actor System)
 (note ()) (kind (Status_changed Submit)))
((id evt-5) (customer_id cus-1) (at "2026-10-13 09:10:00Z")
 (actor (Provider provider-a)) (note ())
 (kind
  (Step_changed (step Identity_document)
   (state (Verified "2026-10-13 09:10:00Z")))))
((id evt-6) (customer_id cus-1) (at "2026-10-13 09:11:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Step_changed (step Liveness) (state (Verified "2026-10-13 09:11:00Z")))))
;; Onboarding screening with one PEP-list candidate, discounted by two operators.
((id evt-7) (customer_id cus-1) (at "2026-10-13 09:12:00Z") (actor System)
 (note ())
 (kind
  (Screened (screening_id scr-1) (reason Onboarding)
   (hits
    (((source Pep_list) (list_version 2026-10-13) (entry_id entry-42)
      (matched_name "A. Example") (score 91) (algorithm jaro-winkler-1)))))))
((id evt-8) (customer_id cus-1) (at "2026-10-13 09:20:00Z")
 (actor (Operator op-a)) (note ("date of birth and nationality differ"))
 (kind
  (Hit_reviewed (screening_id scr-1) (entry_id entry-42)
   (action (Propose_discount "different person")))))
((id evt-9) (customer_id cus-1) (at "2026-10-13 09:25:00Z")
 (actor (Operator op-b)) (note ())
 (kind
  (Hit_reviewed (screening_id scr-1) (entry_id entry-42)
   (action Confirm_discount))))
;; A redelivered webhook: same id, same content, applied once.
((id evt-6) (customer_id cus-1) (at "2026-10-13 09:11:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Step_changed (step Liveness) (state (Verified "2026-10-13 09:11:00Z")))))
((id evt-10) (customer_id cus-1) (at "2026-10-13 09:31:00Z")
 (actor (Operator op-a)) (note ("remote onboarding, fiat account"))
 (kind
  (Risk_assessed
   ((inputs
     ((sector Low) (pep ()) (adverse_media No_findings) (residence Standard)
      (nationality Standard) (products (Fiat_account Card))
      (monthly_volume Up_to_10k) (channel Remote_unattended)
      (self_hosted_exposure false) (manual_edd ())))
    (model_version kyc-risk-2026-10) (score 20) (floors ()) (computed_band Low)
    (override ()) (computed_at "2026-10-13 09:31:00Z")))))
((id evt-11) (customer_id cus-1) (at "2026-10-13 09:32:00Z")
 (actor (Operator op-a)) (note ("documents consistent"))
 (kind (Status_changed Approve)))
