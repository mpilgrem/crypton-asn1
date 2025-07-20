Change log for `crypton-asn1-parse`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.9.6

* Move library modules to directory `src`.
* Drop support for GHC < 8.8.
* Depend on `crypton-asn1-types >= 0.3.6` and drop dependency on
  `crypton-asn1-encoding`.
* Re-exports module `Data.Types.ASN1` from package `crypton-asn1-types`.
* Use `other-extensions` field in Cabal file.
* Cabal file specifies `cabal-version: 1.22` (not `1.18`).

## 0.9.5

* Rename `asn1-parse-0.9.5` package as `crypton-asn1-parse-0.9.5`.
* Depend on `crypton-asn1-types` and `crypton-asn1-encoding`, rather than
  `asn1-types` and `asn1-encoding`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
* Cabal file specifies `cabal-version: 1.18` (not `>= 1.6`).
* Cabal file specifies expressly `default-language: Haskell98`.
