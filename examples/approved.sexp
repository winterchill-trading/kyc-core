;; A customer verified to T2, rated Medium and approved. PII is written in clear here and
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
((id evt-7) (customer_id cus-1) (at "2026-10-13 09:12:00Z") (actor System)
 (note ())
 (kind
  (Screened (screening_id scr-1)
   (outcome
    (Hits
     (((list_kind Pep) (list_name pep-list) (entry_ref entry-42)
       (matched_name "A. Example") (score 81))))))))
((id evt-8) (customer_id cus-1) (at "2026-10-13 09:30:00Z")
 (actor (Operator op-1)) (note ("date of birth and nationality differ"))
 (kind
  (Screening_reviewed (screening_id scr-1)
   (decision (False_positive "different person")))))
;; A redelivered webhook: same id, same content, applied once.
((id evt-6) (customer_id cus-1) (at "2026-10-13 09:11:00Z")
 (actor (Provider provider-a)) (note ())
 (kind (Step_changed (step Liveness) (state (Verified "2026-10-13 09:11:00Z")))))
((id evt-9) (customer_id cus-1) (at "2026-10-13 09:31:00Z")
 (actor (Operator op-1)) (note ("standard retail account, remote onboarding"))
 (kind
  (Risk_assessed
   (factors
    ((country Low) (product Low) (channel Medium) (pep Not_pep)
     (adverse_media false)))
   (override ()))))
((id evt-10) (customer_id cus-1) (at "2026-10-13 09:32:00Z")
 (actor (Operator op-1)) (note ("documents consistent"))
 (kind (Status_changed Approve)))
