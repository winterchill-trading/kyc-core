# examples: kyc-core sample data

`approved.sexp` is a complete case that reaches Approved. The CLI cram test in `test/` reads it.

Rules:

- The data is invented. Do not add real names, documents or identifiers, even redacted ones.
- If a rule change alters the outcome of this case, the cram expectation changes too; update it in
  the same commit.
