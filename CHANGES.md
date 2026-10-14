# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

### Added

- Business verification (KYB). `Kyb_case` is an event-sourced case for a legal entity,
  rebuilt from `Kyb_event.t` values (`Kyb_event.Stable.V1`, digest pinned) and
  idempotent by provider event id, with an audit trail (`Kyb_audit`) that hides risk,
  linked people's case states and block reasons from the applicant.
- `Legal_entity` and `Legal_form`: registration number, jurisdiction, legal form,
  incorporation date, addresses, industry code, bearer shares; trusts as legal
  arrangements.
- `Ownership`: chains through intermediates, effective shares in exact parts per
  10^12, and an opacity scale (`Transparent`, `Layered`, `Complex`, `Opaque`).
- `Kyb_party`: directors, authorised representatives, shareholders, control by other
  means, senior managing officials and trust parties, each linked to the person's own
  `Kyc_case` by `Customer_id`; beneficial owners at or above a threshold (25 % by
  default), by control, by trust role, or senior managing officials as the fallback.
- `Kyb_document` (metadata only) and `Kyb_requirements`: required documents by legal
  form and jurisdiction, jurisdiction coverage and accepted forms, per-jurisdiction
  monthly limits for the caller to enforce.
- `Source_of_funds` declaration.
- `Kyb_status`: `Draft`, `Submitted`, `In_review`, `More_information`, `Approved`,
  `Rejected`, `Blocked`, `Closed`; submission rounds, with decisions about older rounds
  refused; `Kyb_case.decisions` lists every outcome with its round.
- `Kyb_risk`: business risk over `Risk` (industry, jurisdiction, other countries, PEPs
  among the people) plus ownership opacity and cash intensity; opaque ownership is a
  hard floor. EDD needs a senior sign-off; periodic review by band.
- `Kyb_case.applicant_view` and `follow_ups`; `missing_for_submission`,
  `parties_pending`, `can_approve`.
- `Entity_id` and `Document_id`.
- `kyc-case kyb` subcommands: `replay`, `policy`, `assess`, `requirements`; example
  `examples/kyb-approved.sexp`.
