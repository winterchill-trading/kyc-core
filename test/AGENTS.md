# test: kyc-core tests

- Expect tests per transition and per rule (`test_kyc_case.ml`, `test_tier.ml`, `test_risk.ml`,
  `test_screening.ml`, and others).
- Base_quickcheck properties: applying an event twice equals applying it once; floors always give
  High; a riskier sector never lowers the score; Jaro-Winkler is symmetric.
- `fixtures.ml` builds shared cases; add a case here rather than copying one into a test.
- `cli.expected` and `cli-customer.expected` are the CLI cram outputs. Promote them with
  `dune promote` only after reading the diff.
- Digest pins for the stable types are in the tests. Changing a digest means a new stable version,
  not a new expected value.
