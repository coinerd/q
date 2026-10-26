#lang racket/base

;; extensions/gsd-planning/command-normalization.rkt — command parsing, artifact validation
;;
;; Pure functions for GSD command parsing and artifact name validation.
;;
;; W1 (GSD-SIMPLIFICATION-PLAN): the `waves/...` branch delegates to the ONE
;; wave-document path grammar in
;; extensions/gsd/planning-artifact-validation.rkt (wave-artifact-name-refusal),
;; so planning-read/planning-write normalization, preflight and campaign
;; loading cannot disagree about what a wave document path is. The grammar
;; accepts waves/W<idx>-<slug>.md and waves/WAVE-<idx>-<slug>.md with safe
;; dotted slugs (racket-9.3) and keeps refusing `.`/`..` segments, extra
;; separators, absolute paths, NUL bytes and traversal. Guards are not
;; weakened: non-wave names keep their historical acceptance, wave-prefixed
;; names must satisfy the canonical grammar.

(require racket/string
         (only-in "../gsd/planning-artifact-validation.rkt" wave-artifact-name-refusal))

(provide parse-wave-headers
         valid-artifact-name?
         json-artifact?
         artifact-extensions
         planning-dir-name)

(define planning-dir-name ".planning")

(define artifact-extensions
  '(("PLAN" . ".md") ("STATE" . ".md")
                     ("HANDOFF" . ".json")
                     ("VALIDATION" . ".md")
                     ("BUG_REPORT" . ".md")
                     ("BUG_PLAN" . ".md")
                     ("BUG_STATE" . ".md")
                     ("BUG_VALIDATION" . ".md")
                     ("SUMMARY" . ".md")
                     ("REVIEW" . ".md")
                     ("ANALYSIS" . ".md")))

(define (parse-wave-headers plan-text)
  (define matches (regexp-match* #rx"## [Ww]ave +([0-9]+)" plan-text))
  (for/list ([m (in-list matches)])
    (define num-match (regexp-match #rx"([0-9]+)$" m))
    (if num-match
        (string->number (cadr num-match))
        0)))

(define (valid-artifact-name? name)
  (and (string? name)
       (not (string-contains? name ".."))
       (not (string-contains? name "\x00"))
       (cond
         ;; W1: one shared wave-document path grammar (see the module header).
         [(string-prefix? name "waves/") (not (wave-artifact-name-refusal name))]
         [else
          (and (not (string-contains? name "/"))
               (or (assoc name artifact-extensions)
                   (string-suffix? name ".md")
                   (string-suffix? name ".json")))])
       #t))

(define (json-artifact? name)
  (or (string-suffix? name ".json")
      (let ([ext (assoc name artifact-extensions)]) (and ext (string=? (cdr ext) ".json")))))
