#lang racket/base

;; @speed fast
;; @suite testing

;; tests/helpers/wave-finalizer-goldens.rkt — checked-in differential goldens for
;; the wave-finalizer port.
;;
;; Why this file exists: until this wave the three port suites
;; (test-wave-finalizer-model/-controller/-native.rkt) used the Python original
;; `scripts/github/wave-finalizer.py` as a live ORACLE: they imported
;; `test_wave_finalizer` by path, ran its snippets in a child python3 process and
;; compared the port's answer against the observed one.  That made the suite
;; unusable on a host without python3 and kept a deleted implementation alive
;; inside the test harness.
;;
;; The goldens replace the live oracle with its captured evidence.  Every
;; differential case the suites assert was executed once against the Python
;; original at its final revision (see `golden-source-revision`) and its observed
;; outcome was written to `tests/fixtures/wave-finalizer-goldens.json`.  The
;; assertions are unchanged: they still compare the port against an observed
;; outcome; only the provenance of that outcome changed from "computed now by
;; python3" to "captured once from the Python original".
;;
;; Golden layout (one array per case group, position = case position):
;;
;;   {"source": "scripts/github/wave-finalizer.py",
;;    "source_revision": "<40-hex sha>",
;;    "case_encoding": "canonical-json",
;;    "groups": {"<group>": [{"case": "<canonical json of the input>",
;;                            "observed": <outcome>}, ...]}}
;;
;; `golden-outcomes` returns the `observed` values index-aligned with the caller's
;; payload list and fails loudly when the payload list and the golden disagree
;; (a case added, removed, reordered or edited), so a stale golden can never be
;; silently read for a different case.
;;
;; Maintenance: the Python original is gone, so there is no live oracle to
;; recapture from.  The goldens are frozen evidence; a one-off `python3` at
;; revision `source_revision` (from `git log -1 <sha> -- scripts/github/`) is the
;; only way to regenerate them, and any change to a golden value must be
;; explained in the commit that makes it.

(provide golden-source-revision
         golden-source
         golden-groups
         golden-outcomes)

(require json
         racket/file
         racket/list
         racket/promise
         racket/runtime-path
         racket/string
         "../../scripts/github/wave-finalizer.rkt")

(define-runtime-path goldens-path "../fixtures/wave-finalizer-goldens.json")

(define goldens
  (delay
    (call-with-input-file goldens-path read-json)))

(define (golden-source)
  (hash-ref (force goldens) 'source))

(define (golden-source-revision)
  (hash-ref (force goldens) 'source_revision))

(define (golden-groups)
  (hash-keys (hash-ref (force goldens) 'groups)))

(define (golden-group group)
  (define groups (hash-ref (force goldens) 'groups))
  (or (hash-ref groups (string->symbol (format "~a" group)) #f)
      (error 'wave-finalizer-goldens
             "no golden group ~a (have: ~a)"
             group
             (string-join (map (lambda (symbol) (format "~a" symbol))
                               (sort (hash-keys groups) symbol<?))
                          ", "))))

;; The case text is the module's canonical JSON of the input, i.e. exactly the
;; document the Python oracle received on stdin (the snippets load `json.load`).
(define (case-text payload)
  (finalizer-canonical-json payload "golden case" "ValidationError"))

(define (golden-outcomes group payloads)
  (define cases (golden-group group))
  (unless (= (length cases) (length payloads))
    (error 'wave-finalizer-goldens
           "golden group ~a has ~a cases but ~a were supplied"
           group
           (length cases)
           (length payloads)))
  (for/list ([entry (in-list cases)]
             [payload (in-list payloads)]
             [index (in-naturals)])
    (define expected-case (hash-ref entry 'case))
    (define actual-case (case-text payload))
    (unless (equal? expected-case actual-case)
      (error 'wave-finalizer-goldens
             (string-append "golden group ~a case ~a does not match the supplied payload.\n"
                            "  captured: ~a\n  supplied: ~a")
             group
             index
             expected-case
             actual-case))
    (hash-ref entry 'observed)))
