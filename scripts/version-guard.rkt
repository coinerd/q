#lang racket/base

;; scripts/version-guard.rkt — Shared version reference guard
;;
;; 7-pattern context-aware guard that identifies lines containing historical
;; version references that should NOT be updated to the current version.
;; Used by sync-version.rkt and lint-version.rkt.

(require racket/string)

(provide historical-line?)

(define (historical-line? line)
  ;; 7-pattern context-aware guard: skip lines that contain historical
  ;; version references that should NOT be updated to the current version.
  (define trimmed (string-trim line))
  ;; Pattern 1: README Status bold entries — "**vX.Y.Z** — Description"
  (or (regexp-match? #rx"^\\*\\*v[0-9]" trimmed)
      ;; Pattern 2: "in vX.Y.Z" — e.g. "narrowed in v0.28.22"
      (regexp-match? #rx" in v[0-9]+\\.[0-9]+\\.[0-9]+" line)
      ;; Pattern 3: Wave labels — "(vX.Y.Z W0)"
      (regexp-match? #rx"\\(v[0-9]+\\.[0-9]+\\.[0-9]+ W[0-9]\\)" line)
      ;; Pattern 4: "As of vX.Y.Z"
      (regexp-match? #rx"As of v[0-9]+\\.[0-9]+\\.[0-9]+" line)
      ;; Pattern 5: Parenthetical EOL — "(vX.Y.Z)" at line end
      (regexp-match? #rx"\\(v[0-9]+\\.[0-9]+\\.[0-9]+\\)[^)]*$" line)
      ;; Pattern 6: Temporal references — "introduced/added/since vX.Y.Z" (case-insensitive)
      (regexp-match? #px"(?i:introduced|added|since|deprecated|removed) v[0-9]+\\.[0-9]+\\.[0-9]+"
                     line)
      ;; Pattern 7: Section headers with version — "## Title (vX.Y.Z)"
      (regexp-match? #rx"^#+ .*\\(v[0-9]+\\.[0-9]+\\.[0-9]+\\)" trimmed)))
