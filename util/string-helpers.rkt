#lang racket/base

;; util/string-helpers.rkt — Canonical string utility functions
;; STABILITY: stable
;;
;; Single source of truth for truncate-string and related helpers.
;; Replaces duplicate definitions across cli/render.rkt, tui/state-types.rkt,
;; runtime/context-summary.rkt, tools/builtins/firecrawl.rkt,
;; tools/builtins/session-recall.rkt.

(require racket/contract)

(provide
 (contract-out
  [truncate-string (->* (string? exact-nonnegative-integer?)
                        (#:ellipsis string?)
                        string?)]))

;; truncate-string : string? nat? #:ellipsis string? -> string?
;; Truncate s to at most max-len characters, appending ellipsis if truncated.
;; The ellipsis is counted within the max-len budget.
(define (truncate-string s max-len #:ellipsis [ellipsis "..."])
  (if (<= (string-length s) max-len)
      s
      (let ([trimmed-len (max 0 (- max-len (string-length ellipsis)))])
        (string-append (substring s 0 trimmed-len) ellipsis))))
