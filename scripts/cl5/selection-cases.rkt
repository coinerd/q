#lang racket/base

;; scripts/cl5/selection-cases.rkt — frozen registry of executable CL0
;; selective-verification cases (CL5 W2 #9982).
;;
;; Authority: docs/reports/CL0-SELECTIVE-VERIFICATION-CORPUS-003e425d.md.
;; Each case here is a DOCUMENTED fault injection (or a documented selection
;; observation) from that corpus, expressed as one mechanically applicable
;; source mutation plus its changed-file set. The corpus stays the authority:
;; tests/test-cl5-selection.rkt asserts every case id and mutation target
;; appears in the corpus document.
;;
;; A case is data: one substitution, the file it lands in, and what the
;; selector is expected to do. No framework, no plugin registry — adding a
;; case means adding a list entry after verifying the injection in a
;; disposable worktree.

(require racket/file
         racket/list
         racket/string)

(provide selection-cases
         selection-case
         case-injection
         apply-case!
         restore-case!
         classify-selection)

;; Each case:
;;   id            — corpus case id (symbol)
;;   category      — the corpus risk category
;;   changed       — repo-relative file(s) the injection touches
;;   file          — the single file the substitution mutates
;;   from / to     — exact text substitution (must occur exactly once)
;;   expect        — 'detected (the selector must run the detecting owners)
;;   expect-tests  — the corpus-documented detecting owner test files that the
;;                   selected set must actually contain (scope assertion)
;;   note          — what the mutation simulates, in corpus terms
(define selection-cases
  (list (hash 'id
              'D5
              'category
              "behavioral interface violation (vocabulary owner)"
              'changed
              (list "extensions/gsd/wave-status.rkt")
              'file
              "extensions/gsd/wave-status.rkt"
              'from
              (string-append
               "  (or (wave-status-canonical s)\n"
               "      (let ([accepted (accepted-wave-status-spellings)])\n"
               "        (raise (exn:fail:wave-status (unknown-wave-status-message s accepted #f)\n"
               "                                     (current-continuation-marks)\n"
               "                                     s\n"
               "                                     accepted\n"
               "                                     #f)))))")
              'to
              (string-append "  ;; F-VS-1: historical silent fallback (corpus D5)\n"
                             "  (or (wave-status-canonical s) 'pending))")
              'expect
              'detected
              'expect-tests
              (list "tests/test-gsd-wave-status.rkt")
              'escalation-expected
              "consumer-contract escalation (vocabulary fan-in)"
              'note
              "unknown wave status silently read as 'pending instead of a typed rejection")
        (hash 'id
              'F-PE-1
              'category
              "retry classification (held request)"
              'changed
              (list "runtime/auto-retry.rkt")
              'file
              "runtime/auto-retry.rkt"
              'from
              "(define (held-request? exn)\n  (and (exn:fail:network:timeout:stream? exn)"
              'to
              (string-append ";; F-PE-1: held request no longer special-cased (corpus F-PE-1)\n"
                             "(define (held-request? exn)\n  #f\n"
                             "  (and (exn:fail:network:timeout:stream? exn)")
              'expect
              'detected
              'expect-tests
              (list "tests/test-stream-liveness-classification.rkt")
              'escalation-expected
              "retry-classification escalation (timeout ownership)"
              'note
              "held-request (200 + SSE headers, zero chunks) treated as retryable")))

(define (selection-case id)
  (or (for/first ([case (in-list selection-cases)]
                  #:when (eq? (hash-ref case 'id) id))
        case)
      (raise-arguments-error 'selection-case
                             "unknown case id"
                             "id"
                             id
                             "known"
                             (map (lambda (case) (hash-ref case 'id)) selection-cases))))

;; The injection as one exact substitution.
(define (case-injection case)
  (list (hash-ref case 'from) (hash-ref case 'to)))

(define (substitute-once text from to)
  (define occurrences (regexp-match* (regexp-quote from) text))
  (unless (equal? (length occurrences) 1)
    (raise-arguments-error 'substitute-once
                           "injection source must occur exactly once"
                           "occurrences"
                           (length occurrences)
                           "from"
                           from))
  (regexp-replace (regexp-quote from) text to))

;; Apply the case's mutation in place; returns the original text so a caller
;; can restore it (the experiment always restores, even on failure).
(define (apply-case! worktree case)
  (define path (build-path worktree (hash-ref case 'file)))
  (define original (file->string path))
  (define-values (from to) (values (first (case-injection case)) (second (case-injection case))))
  (call-with-output-file path
                         (lambda (out) (display (substitute-once original from to) out))
                         #:exists 'truncate/replace)
  original)

(define (restore-case! worktree case original)
  (call-with-output-file (build-path worktree (hash-ref case 'file))
                         (lambda (out) (display original out))
                         #:exists 'truncate/replace))

;; Pure classification of one case run against comprehensive ground truth.
;; selected-exit / comprehensive-exit are process exit codes (0 = pass).
;;   detected        — selected failed while the defect was present
;;   missed          — selected passed while comprehensive failed (false green)
;;   true-negative   — both passed (clean control)
;;   false-positive  — selected failed while comprehensive passed
(define (classify-selection selected-exit comprehensive-exit)
  (cond
    [(and (not (equal? selected-exit 0)) (not (equal? comprehensive-exit 0))) 'detected]
    [(and (equal? selected-exit 0) (not (equal? comprehensive-exit 0))) 'missed]
    [(and (equal? selected-exit 0) (equal? comprehensive-exit 0)) 'true-negative]
    [else 'false-positive]))
