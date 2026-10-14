# bin: kyc-case CLI

`kyc_case.ml` is the `kyc-case` executable with four subcommands:

- `replay`: folds a case's events and prints its state.
- `policy`: prints the tier rule table and limits.
- `assess`: computes a risk assessment for a case.
- `match`: runs name matching against a list.

Rules:

- Output must not contain a revealed `Pii` value. Print `<redacted>` or the derived fact.
- The cram test in `test/` pins this output; a change here means updating the expected file after
  reading the diff.
- Use `Command.basic` with `core_unix`; there is no Async here.
