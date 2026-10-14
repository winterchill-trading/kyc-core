# kyc-core

Customer due diligence for a bank, independent of any verification or
screening provider: identity verification steps and tiers, a risk-based
customer risk rating with CDD levels and enhanced due diligence (EDD), PEP
records, sanctions/PEP/adverse-media screening with four-eyes discounting,
ongoing monitoring, investigations and FIU reporting, periodic review,
record retention with legal holds, travel-rule data for crypto transfers,
business verification (KYB) for legal entities with their beneficial owners,
and an audit trail of every change with its actor. Pure OCaml over `core`; no
I/O, no Async.

The model follows the FATF Recommendations (R.1, R.6, R.10, R.11, R.12, R.15,
R.16, R.20, R.21), EU AMLR 2024/1624, the EBA risk-factor and remote
onboarding guidelines and EU TFR 2023/1113. The research behind each choice is
in the `specs` repo under `research/banking-infrastructure/kyc-aml.md`.

## Why separate

Every provider integration (document checks, liveness, list screening,
transaction monitoring) speaks its own webhooks and states. Keeping the case
model here means product code, the payment rules, compliance tooling and the
customer-facing API reason about one state machine, and a provider adapter
only translates its webhooks into `Event.t` values. It also keeps PII
handling and the tipping-off rules in one audited place.

## Quick start

```sh
opam exec -- dune build
opam exec -- dune test
opam exec -- dune exec kyc-case -- replay -audit examples/approved.sexp
opam exec -- dune exec kyc-case -- replay -as customer examples/approved.sexp
opam exec -- dune exec kyc-case -- policy
opam exec -- dune exec kyc-case -- match "Jürgen Müller" "MUELLER, Juergen"
opam exec -- dune exec kyc-case -- kyb replay -audit examples/kyb-approved.sexp
opam exec -- dune exec kyc-case -- kyb replay -as customer examples/kyb-approved.sexp
opam exec -- dune exec kyc-case -- kyb requirements -jurisdiction DE -legal-form private_limited_company
opam exec -- dune exec kyc-case -- kyb policy
```

As a library (`(libraries kyc-core)`):

```ocaml
open! Core
open Kyc_core

let on_provider_webhook case ~delivery_id ~at ~step ~state =
  let event : Event.t =
    { id = Event_id.of_string delivery_id (* redelivery = no-op *)
    ; customer_id = Kyc_case.customer_id case
    ; at
    ; actor = Provider "provider-a"
    ; note = None
    ; kind = Step_changed { step; state }
    }
  in
  (* persist [event] on Ok; an Error leaves the case unchanged *)
  Kyc_case.apply case event

let rate case ~operator ~inputs ~at =
  let assessment = Risk.assess Risk.Model.default inputs ~at in
  Kyc_case.apply case
    { id = Event_id.of_string "risk-1"; customer_id = Kyc_case.customer_id case
    ; at; actor = Operator operator; note = Some "onboarding"
    ; kind = Risk_assessed assessment }
```

## Modules

| Module | What it is |
| --- | --- |
| `Pii` | `'a Pii.t`: sexp and `to_string` print `<redacted>`; `Pii.reveal` is the only way to the value; bin_io carries it |
| `Customer_id`, `Event_id`, `Operator_id`, `Screening_id` | string ids |
| `Country`, `Address`, `Person` | minimal personal data, every `Person` field `Pii.t` |
| `Timestamp`, `Actor` | UTC sexp time; `System`, `Provider of name`, `Operator of id` |
| `Verification`, `Tier` | identity steps and their states; T0..T3, the rule table and the `Tier.Limits` hook |
| `Status` | case status machine (Created, Verifying, Approved, Rejected, Needs_resubmission, Blocked, Closed) |
| `Pep` | PEP, family member or close associate; scope; function; dates; `edd_until` at least 12 months after the end |
| `Risk` | inputs per EBA factor group, versioned `Model` (points, score 0-100), `Band`, hard floors, EDD triggers, `Override` |
| `Cdd_level` | `Simplified`, `Standard`, `Enhanced` |
| `Name_match` | folding normalizer and Jaro-Winkler name score, candidate threshold 0.85 |
| `Screening` | list sources, screen reasons, hits (list version, entry id, score, algorithm), per-hit review states |
| `Monitoring` | alerts: `Open`, `Closed_no_action`, `Escalated` |
| `Investigation` | case states, FIU report states, FIU request due dates (5 working days) |
| `Retention` | retain-until dates and legal holds |
| `Visibility` | field classes (`Public`, `Internal`, `Pii`, `Sensitive_pii`, `Restricted`) and audiences |
| `Travel_rule` | TFR Art. 14 originator/beneficiary data, self-hosted ownership checks above EUR 1 000 |
| `Event`, `Audit` | persisted events (with provider event id) and the audit trail |
| `Kyc_case` | the aggregate: `apply`, tiers, `cdd_level`, `can_approve`, `follow_ups`, `customer_view`, review and retention dates |
| `Entity_id`, `Document_id` | string ids for business applicants and document records |
| `Legal_form`, `Legal_entity` | generic legal forms (trusts as legal arrangements); registration number, jurisdiction, addresses, industry, bearer shares |
| `Ownership` | ownership chains through intermediates, effective shares (exact parts per 10^12), opacity |
| `Kyb_party` | directors, representatives, shareholders, controllers, senior managing officials and trust parties, each linked to their own `Kyc_case`; beneficial-owner determination |
| `Kyb_document`, `Kyb_requirements` | document metadata (no content) and the required-documents table by legal form and jurisdiction, with coverage |
| `Source_of_funds` | the applicant's declaration of sources and expected volume |
| `Kyb_item`, `Kyb_status` | missing or requested items; the business case status machine |
| `Kyb_risk` | business risk over `Risk` plus ownership opacity and cash intensity |
| `Kyb_event`, `Kyb_audit` | persisted business events (with provider event id and round) and their audit trail |
| `Kyb_case` | the business aggregate: rounds, decisions, `can_approve`, `follow_ups`, `applicant_view`, review and retention dates |

## Rules

Verification steps: `Pending -> Sent | Verified | Failed`, `Sent -> Sent |
Verified | Failed | Expired`, `Verified -> Expired | Failed`, `Failed | Expired
-> Pending | Sent`.

Case status:

```
Created / Needs_resubmission --Submit--> Verifying
Verifying --Approve--> Approved            (guards below)
Verifying / Needs_resubmission --Reject--> Rejected (final; starts retention)
Verifying / Approved --Request_resubmission fields--> Needs_resubmission
any but Blocked/Closed --Block (operator)--> Blocked { previous }
Blocked --Unblock (operator)--> previous
any but Closed --Close--> Closed (final; starts retention)
```

`Approve` needs: the approval tier's steps (default T2), a screening run with
every hit discounted, a risk assessment, and, under EDD, the EDD tier's steps
(default T3: address proof and source of funds) plus a senior-management
sign-off given after the latest assessment.

Risk (`Risk.Model.default`, version `kyc-risk-2026-10`; points are our
choice): sector 0/15/30, country 0/20/40 (worst of residence and
nationality), adverse media 0/15/40, PEP 40, product 0 (fiat) / 5 (card) /
20 (crypto, Lightning), monthly volume 0/5/15/25, channel 0 (face to face,
eIDAS high) / 5 (remote attended) / 10 (remote unattended), self-hosted
exposure 15. Score capped at 100; bands Low 0-29, Medium 30-59, High 60-100.
Hard floors (an active PEP, a high-risk third country, confirmed adverse
media) force High and cannot be overridden down. An override needs an
operator and a reason; the assessment keeps the computed band, the override
and the model version.

CDD level: Enhanced for any EDD trigger (PEP within its EDD period, family or
close associate, high-risk third country, confirmed adverse media,
self-hosted exposure, an escalated transaction-monitoring alert, a manual
reason) or a High band; Simplified only for Low band, when the policy allows,
and never for crypto or Lightning; otherwise Standard.

Screening hits: `Open -> Under_review`, a proposed discount by operator A
becomes `Discounted` only when operator B confirms it; `Confirmed` and
`Escalated` need a reason. A confirmed hit on a sanctions list (EU FSF, UN
SC, OFAC SDN, UK) blocks the customer in the same event (FATF R.6). A
confirmed PEP or adverse-media hit that the latest risk inputs do not reflect
raises `Risk_assessment_needed`.

Periodic review: due 5 / 3 / 1 years (Low / Medium / High) after approval or
the last completed review; a band change moves the date; completing a review
needs a fresh assessment and no outstanding EDD. A scheduler should raise an
`Overdue_review` alert when `Periodic_review_due` appears in `follow_ups`.

Monitoring and reporting: alerts are closed with no action or escalated; an
investigation groups escalated alerts and moves `Open -> Investigating ->
Pending_decision -> Closed_no_report | Closed_reported`; a report goes `Draft
-> Approved` (by someone other than the drafter) `-> Filed -> Acknowledged |
Rejected_by_fiu`. FIU requests are due 5 working days after receipt.

Tipping-off (FATF R.21, AMLR Art. 73): alerts, investigations, reports, FIU
requests and block reasons are `Restricted` and visible only to compliance.
`Kyc_case.customer_view` shows a blocked customer `Under_review` and nothing
more, and `Audit.for_audience Customer` drops restricted entries and staff
notes. Customer-facing APIs should only ever use those two.

Retention: records are kept 5 years after closure or refusal; a legal hold
(operator, reason, authority) may extend it by at most 5 years. After
`retain_until` the case reports `Retention_ended`: erase PII then (with PII
encrypted under a per-customer key, by destroying the key). Only monitoring,
reporting and retention events are accepted after closure.

Idempotency: an event id that was already applied is a no-op; the same id
with different content is an error. Illegal events return `Error` and leave
the case unchanged; nothing raises. Apply events in the order they happened.

## Business verification (KYB)

A business applicant gets a `Kyb_case`, keyed by an `Entity_id` and rebuilt
from `Kyb_event.t` values under the same idempotency rules as a personal case.
Every director, authorised representative, owner and controller is a
`Kyb_party` that points at the person's own `Kyc_case` by `Customer_id`: their
identity, screening and PEP status are checked there, and the platform reports
each person's case status to the business case with `Party_case_changed`.

```
Draft --Submit--> Submitted (round 1)            (completeness check)
Submitted --Start_review--> In_review
Submitted / In_review --Approve--> Approved       (guards below)
Submitted / In_review --Request_information items--> More_information
More_information --Submit--> Submitted (next round)
Approved --Request_information--> More_information (periodic review)
Submitted / In_review / More_information --Reject reason--> Rejected
any but Blocked/Closed --Block (operator)--> Blocked { previous }
Blocked --Unblock (operator)--> previous
any but Closed --Close--> Closed
```

- Editing (entity, parties, documents, source of funds) is allowed in `Draft`
  and `More_information` only; the jurisdiction is fixed after the first
  submission. Opening and updates check that the jurisdiction is covered and
  accepts the legal form.
- A status event may name the round it is about; a decision about an older
  round is refused, so a late provider event cannot overturn a resubmission.
- `Submit` needs the registration data, at least one director (a trustee for a
  trust), identified beneficial owners, every required document and a
  source-of-funds declaration. `missing_for_submission` lists what is missing.
- Beneficial owners: 25 % or more (policy `owner_threshold_bps`, AMLR Art. 52)
  summed over all chains, where a chain multiplies the stakes through each
  intermediate; anyone with control by other means; every settlor, trustee,
  protector and beneficiary of a trust; and only when none exists, the senior
  managing officials.
- Required documents come from `Kyb_requirements`: a list per legal form, extra
  documents per jurisdiction, an ownership chart when any owner holds through
  an intermediate, and a power of attorney per representative who is not a
  director. Files are never stored, only metadata.
- `Approve` needs everything `Submit` needs, every beneficial owner, director
  and representative approved in their own case, a risk assessment and, under
  EDD, a senior sign-off with a rationale after the latest assessment.
- Risk: `Kyb_risk` maps the entity onto `Risk.Inputs` (industry as sector,
  registration country, worst other country, a PEP among the people) and adds
  ownership opacity (Transparent 0, Layered 5, Complex 15, Opaque 40) and a
  cash-intensive business (15). Opaque ownership (bearer shares, nominees) is
  a hard floor to High, alongside the personal floors.
- Periodic review falls due by band as for persons; completing it needs a
  fresh assessment, the people still approved and no outstanding EDD.
- Tipping-off: `Kyb_case.applicant_view` shows a blocked case as
  `Under_review` with nothing requested, and never names a linked person whose
  own case is blocked; `Kyb_audit.for_audience Customer` drops risk, linked
  people's case states, block reasons and staff notes.

## Stored formats

`Event.Stable.V1` (and the `Stable` modules it is built from, plus
`Audit.Entry.Stable.V1`, `Risk.Assessment.Stable.V1` and
`Travel_rule.Stable.V1`) are the persisted formats; their bin_io digests are
pinned in the tests. Store events and rebuild cases with
`Kyc_case.of_events`; a risk assessment is stored as computed, so replaying
never rescores with a newer model. PII is in clear inside bin_io, so the event
store is PII storage. `Kyb_event.Stable.V1` and `Kyb_audit.Entry.Stable.V1`
are the business-case formats, pinned the same way. A sexp of an event redacts PII and cannot be read back
into the same value; sexps are for logs and hand-written fixtures.

## Environment variables

None. The library does no I/O.

## What is verified

Everything is verified by tests in this repo; there is no live API.

- every verification-state, status, hit-review, alert, investigation and report transition, legal and illegal
- risk scoring: bands, hard floors, overrides, tamper checks on recorded assessments; Quickcheck that floors always give High and that a riskier sector never lowers the score
- PEP EDD tail (12 months, extend only), CDD level decisions
- Jaro-Winkler against reference pairs (MARTHA/MARHTA 0.961, DWAYNE/DUANE 0.840, DIXON/DICKSONX 0.813), folding of Latin diacritics, symmetry (Quickcheck)
- case scenarios: approval guards, EDD with senior sign-off, review dates by band, a confirmed sanctions match blocking silently, monitoring to filed report, retention and legal holds, what each audience sees
- applying the same event twice equals applying it once, and redelivering every accepted event changes nothing, over random streams that reach every status (Quickcheck)
- PII redaction in persons, events and travel-rule records; bin_io round trips keep PII
- the `kyc-case` CLI over `examples/approved.sexp`, as compliance and as the customer
- KYB: every business status transition, legal and illegal; effective ownership through chains, beneficial owners (threshold, control, trusts, senior managing official fallback), opacity; documents by legal form and jurisdiction; rounds and stale decisions; approval guards and EDD; what the applicant sees when blocked
- KYB properties (Quickcheck): applying an event twice equals once, redelivering accepted events and rebuilding with `of_events` give the same case, rounds never go down, a blocked case shows the applicant nothing; random streams reach every status
- `kyc-case kyb replay` over `examples/kyb-approved.sexp`, for staff and for the applicant

## Known gaps

- Name matching folds Latin scripts only; other scripts are not transliterated
  (ICU-style transliteration needs a Unicode library not on the switch).
- The business case does not screen the entity's own name against sanctions
  lists; the linked people are screened in their own cases. Run an entity
  screening outside and block the business case if it confirms.
- Monthly limits per jurisdiction are declared in `Kyb_requirements` for the
  caller to enforce; the library does not track volumes.
- FIU working days skip weekends only; pass the FIU country's holiday calendar
  to `Investigation.Fiu_request.create` (the case uses no holidays).
- Report contents (goAML XML etc.) are out of scope; only report states are tracked.
- Field classes are declared, but per-customer key encryption and the erasure
  job live in the storage layer.
- Years are 365 days for reviews and retention (slightly early, the safe side).
- Re-verification of a still-verified step must go through `Expired` first.
- `Tier.Rules`, `Kyc_case.Policy` and `Risk.Model` are configuration, not
  stored data; a model change must get a new version string.
