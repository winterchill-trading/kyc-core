# kyc-core

Customer identity and verification state, independent of any verification
provider: PII wrappers that redact themselves, verification steps, tiers, the
case status machine, sanctions/PEP screening records and an audit trail of
every change. Pure OCaml over `core`; no I/O, no Async.

## Why separate

Every provider integration (document checks, liveness, screening) speaks its
own webhooks and states. Keeping the case model here means the product code,
the ledger-facing limits and the compliance tooling all reason about one
state machine, and a provider adapter only has to translate its webhooks into
`Event.t` values. It also keeps PII handling in one audited place.

## Quick start

```sh
opam exec -- dune build
opam exec -- dune test
opam exec -- dune exec kyc-case -- replay -audit examples/approved.sexp
opam exec -- dune exec kyc-case -- rules
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
  match Kyc_case.apply case event with
  | Ok case -> (* persist [event], then use [Kyc_case.tier case] *) Ok case
  | Error _ as e -> (* illegal for this case: log and keep the old case *) e
```

## Modules

| Module | What it is |
| --- | --- |
| `Pii` | `'a Pii.t`: sexp and `to_string` print `<redacted>`; `Pii.reveal` is the only way to the value; bin_io carries it |
| `Customer_id`, `Event_id`, `Operator_id`, `Screening_id` | string ids (non-empty, no edge whitespace) |
| `Country`, `Address`, `Person` | minimal personal data; every `Person` field is `Pii.t`; `Person.Field` names fields without data |
| `Timestamp` | `Time_ns.t` with a UTC sexp, so output is the same on every host |
| `Actor` | `System`, `Provider of name`, `Operator of Operator_id.t` |
| `Verification` | steps (Email, Mobile, Identity_document, Liveness, Address_proof, Source_of_funds) and their states |
| `Tier` | T0..T3, the rule table (`Tier.Rules`) and the abstract `Tier.Limits` hook |
| `Status` | the case status machine and its transitions |
| `Screening` | hits, outcome, reviewer decision |
| `Event` | what happened, with the provider event id |
| `Audit` | who/when/why for each change; holds no PII |
| `Kyc_case` | the aggregate: `apply`, `tier`, `missing_steps`, `required_for`, `can_approve`, `limits` |

## Rules

Verification step states:

```
Pending  -> Sent | Verified | Failed
Sent     -> Sent (resend) | Verified | Failed | Expired
Verified -> Expired | Failed (revoked on review)
Failed   -> Pending | Sent (retry)
Expired  -> Pending | Sent (retry)
```

Case status:

```
Created / Needs_resubmission --Submit--> Verifying
Verifying --Approve--> Approved           (needs the approval tier, a screening run, no open or true hits)
Verifying / Needs_resubmission --Reject--> Rejected (final)
Verifying / Approved --Request_resubmission fields--> Needs_resubmission
any but Blocked --Block (operator only)--> Blocked { previous }
Blocked --Unblock (operator only)--> previous
```

Tiers (default `Tier.Rules`, each tier includes the one below):

| Tier | Requires |
| --- | --- |
| T0 | nothing |
| T1 | Email, Mobile |
| T2 | + Identity_document, Liveness |
| T3 | + Address_proof, Source_of_funds |

`Kyc_case.tier` is the tier the customer may use now: T0 when rejected or
blocked, the verified tier when approved, at most T1 while the case is open.
What a tier allows is the product's decision: implement `Tier.Limits` and call
`Kyc_case.limits`.

Idempotency: an event id that was already applied is a no-op; the same id
with different content is an error. Illegal events return `Error` and leave
the case unchanged; nothing raises. Events must be applied in the order they
happened (a late `Sent` after `Verified` is rejected as illegal), so sort by
`at` or by provider sequence before applying.

## Stored formats

`Event.Stable.V1` (and the `Stable` modules it is built from, plus
`Audit.Entry.Stable.V1`) is the persisted format; the bin_io digests are
pinned in `test/test_event.ml` and the module tests. Store events, rebuild
cases with `Kyc_case.of_events`. PII is in clear inside bin_io, so the event
store must be treated as PII storage (encrypt at rest). A sexp of an event
redacts PII and cannot be read back into the same value; sexps are for logs
and for hand-written fixtures (`t_of_sexp` reads clear values).

## Environment variables

None. The library does no I/O.

## What is verified

Everything is verified by tests in this repo; there is no live API.

- every verification-state and status transition, legal and illegal (expect tables)
- block/unblock only by an operator; unblock restores the previous status (Quickcheck)
- tier rule validation and monotonicity of `tier_of` (Quickcheck)
- case scenarios: approval to T2, approval guards, resubmission, block/unblock, rejection
- applying the same event twice equals applying it once, and redelivering every
  accepted event changes nothing, over random event streams that reach every status (Quickcheck)
- PII redaction in sexps of persons and events; bin_io round trips keep PII
- the `kyc-case` CLI over `examples/approved.sexp` (diffed output)

## Known gaps

- Screening hits name the list entry only; fuzzy-match details (aliases,
  matched fields) are left to the provider adapter.
- Re-verification of a still-verified step (e.g. a new document before
  expiry) must go through `Expired` first.
- `Tier.Rules` and `Kyc_case.Policy` are not versioned: they are configuration,
  not stored data.
- No document storage or images; steps record outcomes only.
