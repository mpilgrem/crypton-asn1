Change log for `crypton-asn1-encoding`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## UNRELEASED

* Move library modules to directory `src`.
* Drop support for GHC < 8.8.

## 0.9.6

* Rename `asn1-encoding-0.9.6` package as `crypton-asn1-encoding-0.9.6`.
* Depend on `crypton-asn1-types`, rather than `asn1-types`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
* Cabal file specifies `cabal-version: 1.18` (not `>= 1.10`).
