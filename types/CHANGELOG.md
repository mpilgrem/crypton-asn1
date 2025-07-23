Change log for `crypton-asn1-types`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## UNRELASED

* Depend on package `time-hourglass`, rather than `hourglass`. The fields of the
  `ASNTime` data constructor of data type `ASN1` are now types provided by the
  former package.

## 0.3.6

* Expose `Data.ASN1.Stream`, spun out of the `crypton-asn1-encoding-0.9.6`
  package.
* Use the `other-extensions` field in the Cabal file.

## 0.3.5

* Move library modules to directory `src`.
* Change data type `BitArrayOutOfBound` (a single, unary data constructor
  without strictness annotation) to `newtype`.
* Depend on the `base16` package, remove direct dependency on the `memory`
  package and the indirect dependency on the `basement` package.

## 0.3.4

* Rename `asn1-types-0.3.4` package as `crypton-asn1-types-0.3.4`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
* Cabal file specifies `cabal-version: 1.18` (not `>= 1.6`).
* Cabal file specifies expressly `default-language: Haskell98`.
