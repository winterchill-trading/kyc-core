# src: kyc-core library

Library `kyc-core` (module `Kyc_core`). Two groups of modules:

- Identifiers and personal data: `Customer_id`, `Event_id`, `Operator_id`, `Screening_id`,
  `Country`, `Address`, `Person`, `Pii`, `Timestamp`, `Actor`.
- Rules and aggregates: `Verification`, `Tier`, `Status`, `Pep`, `Risk`, `Cdd_level`,
  `Name_match`, `Screening`, `Monitoring`, `Investigation`, `Retention`, `Visibility`,
  `Travel_rule`, `Event`, `Audit`, and the aggregate `Kyc_case`.

Rules:

- `Kyc_case.apply` is idempotent by event id: a repeat with the same content is a no-op, and the
  same id with different content is an error. Illegal events return `Error`; nothing raises.
- A confirmed sanctions hit blocks the customer in the same event. Investigation and alert data
  stays visible to compliance only (tipping-off rules in `Visibility`).
- `Event.Stable.V1`, `Audit.Entry.Stable.V1`, `Risk.Assessment.Stable.V1` and
  `Travel_rule.Stable.V1` are persisted. Replay never rescores a stored assessment.
- Name matching folds Latin scripts only; say so in any new matching code.

The full rule list is in `../README.md` ("Rules"). Keep it and this file in step.
