(lang dune 3.17)

(name kyc-core)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/kyc-core))

(package
 (name kyc-core)
 (synopsis "Provider-neutral customer identity and verification (KYC) state")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  ppx_jane))
