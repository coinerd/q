#lang racket/base

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-gsd-wave-gen-validation.rkt — v0.75.7 W1 tests
;; Wave generation validation: wave-exists?, parse-plan-index, wave cleanup
;; BUG-0095: planning-write roundtrip of the emitted mandatory template.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/port
         racket/string
         "../extensions/gsd/prompts.rkt"
         "../extensions/gsd/wave-docs.rkt"
         (only-in "../extensions/gsd/tool-handlers.rkt" handle-planning-write)
         (only-in "../extensions/tool-api.rkt" tool-result-is-error?))

;; ============================================================
;; BUG-0095 helper: extract the emitted mandatory template verbatim
;; ============================================================

(define (string-index-of s sub [from 0])
  (define len (string-length sub))
  (let loop ([i from])
    (cond
      [(> (+ i len) (string-length s)) #f]
      [(string=? (substring s i (+ i len)) sub) i]
      [else (loop (add1 i))])))

(define (emitted-wave-doc-template)
  (define p (planning-prompt "test"))
  (define m (regexp-match-positions #rx"### Wave Doc Format[^\n]*\n```\n" p))
  (unless m
    (error "wave-doc template fence missing from planning-prompt"))
  (define body-start (cdr (car m)))
  (define close (string-index-of p "\n```" body-start))
  (unless close
    (error "wave-doc template closing fence missing"))
  (substring p body-start close))

(define (count-lines-matching s rx)
  (count (lambda (line) (regexp-match? rx line)) (string-split s "\n")))

(define-test-suite
 test-wave-gen-validation
 (test-case "wave-exists? returns #f for nonexistent wave"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define result (wave-exists? tmp 0 "nonexistent"))
   (check-false result)
   (delete-directory/files tmp))
 (test-case "wave-exists? returns #t after writing wave doc"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (write-wave-doc! tmp 0 "test-slug" "## Action\nDo stuff" "Inbox")
   (check-true (wave-exists? tmp 0 "test-slug"))
   (delete-directory/files tmp))
 (test-case "parse-plan-index extracts wave entries with slugs"
   (define plan-text
     "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n- [Inbox] W1: implement → waves/W1-implement.md\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 2)
   (check-equal? (wave-index-entry-idx (car entries)) 0)
   (check-equal? (wave-index-entry-idx (cadr entries)) 1)
   (check-equal? (wave-index-entry-slug (car entries)) "setup")
   (check-equal? (wave-index-entry-slug (cadr entries)) "implement"))
 (test-case "missing first wave doc detected via wave-exists?"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define plan-text "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n")
   (define entries (parse-plan-index plan-text))
   (define first-entry (car entries))
   (check-false
    (wave-exists? tmp (wave-index-entry-idx first-entry) (wave-index-entry-slug first-entry)))
   (delete-directory/files tmp))
 (test-case "present first wave doc passes wave-exists? check"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define plan-text "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n")
   (define entries (parse-plan-index plan-text))
   (define first-entry (car entries))
   (write-wave-doc! tmp
                    (wave-index-entry-idx first-entry)
                    (wave-index-entry-slug first-entry)
                    "## Action\nDo stuff"
                    "Inbox")
   (check-true
    (wave-exists? tmp (wave-index-entry-idx first-entry) (wave-index-entry-slug first-entry)))
   (delete-directory/files tmp))
 ;; ============================================================
 ;; BUG-0095 (#9854): the exact emitted mandatory template bytes must
 ;; survive a planning-write roundtrip and lint clean afterward — for
 ;; W0 (literal) and a substituted nonzero wave. planning-write stores
 ;; content VERBATIM, so the read-back bytes equal the emitted bytes.
 ;; ============================================================
 (test-case "BUG-0095: emitted template roundtrips through planning-write clean (W0)"
   (define tmp (make-temporary-file "wave-gen-0095-~a" 'directory))
   (define tpl (emitted-wave-doc-template))
   (define result
     (handle-planning-write (hasheq 'artifact "waves/W0-short-title.md" 'content tpl 'base_dir tmp)))
   (check-false (tool-result-is-error? result))
   (define doc-path (build-path tmp ".planning" "waves" "W0-short-title.md"))
   (check-true (file-exists? doc-path))
   ;; Exact emitted bytes are stored and read back verbatim.
   (define raw (call-with-input-file doc-path port->string))
   (check-equal? raw tpl "planning-write stores the emitted bytes verbatim")
   (check-equal? (count-lines-matching raw #rx"^# Wave [0-9]+$") 1 "single H1 on disk")
   (check-equal? (count-lines-matching raw #px"^Status:") 1 "single Status line on disk")
   ;; Read back through the canonical reader and lint clean.
   (define doc (read-wave-doc tmp 0 "short-title"))
   (check-not-false doc)
   (check-true (hash-ref doc 'status-header?))
   (check-equal? (hash-ref doc 'status) "Inbox")
   (check-equal? (map wave-doc-violation-section (lint-wave-doc doc)) '())
   (check-true (string-contains? (hash-ref doc 'content) "## Files"))
   (check-true (string-contains? (hash-ref doc 'content) "## Verify"))
   (check-true (string-contains? (hash-ref doc 'content) "## Done"))
   (delete-directory/files tmp))
 (test-case "BUG-0095: substituted nonzero-wave template roundtrips clean (W2)"
   (define tmp (make-temporary-file "wave-gen-0095-~a" 'directory))
   (define tpl2 (string-replace (emitted-wave-doc-template) "# Wave 0" "# Wave 2"))
   (define result
     (handle-planning-write (hasheq 'artifact "waves/W2-short-title.md" 'content tpl2 'base_dir tmp)))
   (check-false (tool-result-is-error? result))
   (define doc-path (build-path tmp ".planning" "waves" "W2-short-title.md"))
   (check-true (file-exists? doc-path))
   (define raw (call-with-input-file doc-path port->string))
   (check-equal? raw tpl2)
   (check-equal? (count-lines-matching raw #rx"^# Wave [0-9]+$") 1 "single H1 on disk")
   (check-equal? (count-lines-matching raw #px"^Status:") 1 "single Status line on disk")
   (define doc (read-wave-doc tmp 2 "short-title"))
   (check-not-false doc)
   (check-true (hash-ref doc 'status-header?))
   (check-equal? (hash-ref doc 'status) "Inbox")
   (check-equal? (map wave-doc-violation-section (lint-wave-doc doc)) '())
   (check-true (string-contains? (hash-ref doc 'content) "## Files"))
   (check-true (string-contains? (hash-ref doc 'content) "## Verify"))
   (check-true (string-contains? (hash-ref doc 'content) "## Done"))
   (delete-directory/files tmp)))

(run-tests test-wave-gen-validation)
