#lang racket/base

;; extensions/gsd-planning/command-normalization.rkt — command parsing, artifact validation
;;
;; Pure functions for GSD command parsing and artifact name validation.
;;
;; W1: planning-read/planning-write share the preflight module's historical
;; artifact-name gate. A waves/ name remains one direct .md filename; unsafe
;; traversal, NUL, absolute and nested paths are refused. Canonical
;; W<n>/WAVE-<n> recognition is advisory and does not narrow legacy names.

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
       (not (string-contains? name "\x00"))
       (cond
         ;; Wave paths use exactly the shared gate, including safe dotted slugs.
         [(string-prefix? name "waves/") (not (wave-artifact-name-refusal name))]
         [else
          (and (not (string-contains? name ".."))
               (not (string-contains? name "/"))
               (or (assoc name artifact-extensions)
                   (string-suffix? name ".md")
                   (string-suffix? name ".json")))])
       #t))

(define (json-artifact? name)
  (or (string-suffix? name ".json")
      (let ([ext (assoc name artifact-extensions)]) (and ext (string=? (cdr ext) ".json")))))
