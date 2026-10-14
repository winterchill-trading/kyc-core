# kyc-core: agent guide

## Purpose

Provider-neutral customer due diligence for a bank, as an event-sourced library: identity
verification steps and tiers, risk rating and CDD levels, PEP and sanctions screening with
four-eyes review, ongoing monitoring, investigations, periodic review and record retention. It
does no I/O and uses no Async; a provider adapter turns webhooks into `Event.t` values.

## Layout

| Path | Holds |
| --- | --- |
| `src/` | Library `kyc-core` (module `Kyc_core`): identifiers, `Pii`, `Tier`, `Risk`, `Screening`, `Event`, `Kyc_case` and the rest |
| `src/kyb_*`, `src/legal_*`, `src/ownership.*` | Business verification (KYB): `Legal_entity`, `Ownership`, `Kyb_party`, `Kyb_requirements`, `Kyb_risk`, `Kyb_event`, `Kyb_case` |
| `bin/` | The `kyc-case` CLI: `replay`, `policy`, `assess`, `match` |
| `examples/` | `approved.sexp`, a case used by the CLI cram test |
| `test/` | Expect tests per rule, Base_quickcheck properties, the CLI expected outputs |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @kyc-core/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @kyc-core/runtest

This repository has no sibling dependencies. Standalone, after the organization's opam
repository is added (`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`),
`dune build` and `dune test` in this directory are the same checks.

Several agents building at once need their own build directory: `DUNE_BUILD_DIR=_build-kyc-core`.

## Conventions

- Format this repository only: `dune build --root .. @kyc-core/fmt --auto-promote`. Never
  `dune fmt` at the workspace root.
- Every module has an `.mli`; every public value has an odoc comment.
- Persisted formats are `Stable.V1` modules. Their bin_io digests are pinned in tests. Never change
  a stable type in place; add a `V2` and keep the old reader.
- `Pii.t` prints `<redacted>`. Use `Pii.reveal` only where the value is needed, and never print a
  revealed value in a test or CLI output.
- Logic is pure: a rule change comes with an expect test that names the rule.
- Risk weights and band cut-offs are configuration (model version `kyc-risk-2026-10`), so a change
  to them bumps the model version rather than editing old assessments.
- Comments for humans at non-obvious choices: years are 365 days, tipping-off visibility, and why
  an override cannot lower a floor.

## Dependencies

- External only: `core`, `ppx_jane`; `core_unix` for the CLI. No sibling repositories.
- No dependents in the organization yet.

## CI

`.github/workflows/ci.yml` runs the `infra` ox-setup action with no pins, then `dune build` and
`dune test`. The `obj-magic` job calls the reusable workflow in `winterchill-trading/infra`.
To check a run, use `gh run list --limit 1` and at most one `gh run watch <id> --exit-status`.
Never poll in a loop.

## Pitfalls

- Inline tests need `(flags (:standard -source-tree-root .))` in the test `dune` stanza under the
  ox switch, or every partition fails.
- `Binable.Of_binable_without_uuid` needs `[@alert "-legacy"]`; use it to validate on read, not to
  change the wire shape.
- The inline test runner shares one process across test files. A test that registers global state
  (for example currencies or list versions) can change another file's output.
- `include Comparable.Make` shadows `<` and `=` for the rest of the file; put it at the end of the
  module.
- A `(*` inside a comment opens a nested comment; avoid `count(*)` in comments.

## Where to read more

- `README.md` (rules, stored formats, what is verified, known gaps)
- `../specs/repos/kyc-core.md` (design summary, known gaps)
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide) and `../oxcaml-claude-skills/GOTCHAS.md`
- Skills in `../oxcaml-claude-skills/skills/`: `banking-standards`, `janestreet-ocaml-style`

## Never do

- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`,
  `async` or `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Store or print personal data outside `Pii` in test output or fixtures; examples use invented
  names only.
- Poll CI in a loop.
