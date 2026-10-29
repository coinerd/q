# plan-snapshot-legacy-v1 fixture

`waves/W0-tracker-reconciliation-wiring.md` is a **byte-exact copy**
(4584 bytes) of the file a real v1 plan-snapshot capture froze for campaign
`79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e`, read
read-only from `.planning/campaigns/<id>/plan-snapshot/waves/`. Nothing in
`.planning` was modified to produce it.

Its recorded manifest digest was, and is:

```
3b7a40404b84efa40343feedc6081692454bcc504eead643124d8907ba0b43d5
```

Why the bytes are pinned here:

- The digest above was produced by the **legacy (scheme 1)** wave-doc
  normalization that shipped up to `d5bf128b`.
- With the **current (scheme 2)** normalization that shipped from `4ac41abc`
  the *same* bytes hash to
  `7dd8b79de08b65c7d382ea12e98e0a88e6c8176b697c7e77852df4b55dc8955f`.
- Because the manifest schema-version did not change with the normalization,
  a reader that only knew scheme 2 called the authentic capture
  "partial/corrupt snapshot entry" and the live campaign could not resume.

`tests/test-gsd-plan-snapshot.rkt` carries an independent, verbatim copy of
the scheme-1 normalizer, and pins both the size and the digest above so that a
drift in either the fixture or the transcription fails the suite. Do not edit
this file: it is the reproducible baseline for the compat regression
(`legacy v1 capture loads and reports no drift (baseline fail->pass)`).
