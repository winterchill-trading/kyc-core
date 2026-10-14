# .github: kyc-core CI

`workflows/ci.yml` builds and tests this repository alone, with the `infra` ox-setup action and no
pins. The `obj-magic` job calls the reusable workflow in `winterchill-trading/infra`.

Rules:

- Keep the job free of sibling checkouts; this repository must stay buildable on its own.
- To check a run: `gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.
