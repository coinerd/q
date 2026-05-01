#lang racket/base

;; extensions/gsd-planning/command-normalization.rkt — command parsing, artifact validation
;;
;; Pure functions for GSD command parsing and artifact name validation.

(require racket/string)

(provide extract-cmd-args
         parse-wave-headers
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

(define (extract-cmd-args input-text)
  (define trimmed (string-trim input-text))
  (define parts
    (and (> (string-length trimmed) 0)
         (char=? (string-ref trimmed 0) #\/)
         (string-split trimmed)))
  (if (and (pair? parts) (> (length parts) 1))
      (string-trim (string-join (cdr parts) " "))
      ""))

(define (parse-wave-headers plan-text)
  (define matches (regexp-match* #rx"## [Ww]ave +([0-9]+)" plan-text))
  (for/list ([m (in-list matches)])
    (define num-match (regexp-match #rx"([0-9]+)$" m))
    (if num-match (string->number (cadr num-match)) 0)))

(define (valid-artifact-name? name)
  (and (string? name)
       (not (string-contains? name ".."))
       (not (string-contains? name "\x00"))
       (cond
         [(string-prefix? name "waves/")
          (define rest (substring name 6))
          (and (not (string=? rest ""))
               (string-suffix? rest ".md")
               (not (string-contains? rest "/")))]
         [else
          (and (not (string-contains? name "/"))
               (or (assoc name artifact-extensions)
                   (string-suffix? name ".md")
                   (string-suffix? name ".json")))])
       #t))

(define (json-artifact? name)
  (or (string-suffix? name ".json")
      (let ([ext (assoc name artifact-extensions)])
        (and ext (string=? (cdr ext) ".json")))))
