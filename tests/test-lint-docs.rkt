#lang racket/base

;; @covers scripts/lint-docs.rkt
;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-lint-docs.rkt — the documentation link checker really resolves
;; local file references AND #anchors (file#anchor and pure #anchor), and it
;; reuses the shipped checker rather than a second parser.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/string
         (only-in "../scripts/lint-docs.rkt"
                  check-doc-links
                  doc-link-problem
                  heading->anchor
                  doc-file-anchors))

(define (with-doc-fixture body)
  (define root (make-temporary-file "q-lint-docs-~a" 'directory))
  (dynamic-wind void
                (lambda () (body root))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(define (write-doc! path text)
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'replace))

(define (quiet thunk)
  (with-output-to-string thunk))

(define lint-docs-suite
  (test-suite "lint-docs link checker"

    (test-case "heading slugs match the anchors used by the real docs"
      (check-equal? (heading->anchor "Campaign lifecycle") "campaign-lifecycle")
      ;; Strict punctuation removal leaves the two spaces around the removed
      ;; `##`; link comparison collapses the hyphen run (below).
      (check-equal? (heading->anchor "Verify grammar (`## Verify`)") "verify-grammar--verify")
      (check-equal? (heading->anchor "Simplification campaign (v1.00.31): measured before/after")
                    "simplification-campaign-v10031-measured-beforeafter")
      (check-equal?
       (heading->anchor "Adoption status — operational state after the waves in v1.00.04")
       "adoption-status--operational-state-after-the-waves-in-v10004"))

    (test-case "file anchors ignore fenced examples and disambiguate duplicates"
      (with-doc-fixture (lambda (root)
                          (define doc (build-path root "anchors.md"))
                          (write-doc! doc
                                      (string-join '("# Doc" "## Repeated"
                                                             "```markdown"
                                                             "## Not a heading"
                                                             "```"
                                                             "## Repeated"
                                                             "### Nested `Code`")
                                                   "\n"))
                          (check-equal? (doc-file-anchors doc)
                                        '("doc" "repeated" "repeated-1" "nested-code")))))

    (test-case "file#anchor and pure #anchor resolve real local references"
      (with-doc-fixture
       (lambda (root)
         (define target (build-path root "target.md"))
         (define source (build-path root "source.md"))
         (write-doc! target "# Target\n\n## Valid Heading\n")
         (write-doc! source "# Source\n\n## Valid Heading\n")
         ;; Valid references: same-file anchor, file#anchor and plain file.
         (check-false (doc-link-problem source "#valid-heading"))
         (check-false (doc-link-problem source "target.md#valid-heading"))
         (check-false (doc-link-problem source "target.md"))
         ;; Missing anchors and missing files are distinct failures.
         (check-equal? (car (doc-link-problem source "#missing-heading")) 'missing-anchor)
         (check-equal? (car (doc-link-problem source "target.md#missing-heading")) 'missing-anchor)
         (check-equal? (car (doc-link-problem source "missing.md")) 'missing-file)
         (check-equal? (car (doc-link-problem source "missing.md#anything")) 'missing-file))))

    (test-case "check-doc-links counts valid and broken file/anchor references"
      (with-doc-fixture
       (lambda (root)
         (define target (build-path root "target.md"))
         (define source (build-path root "source.md"))
         (write-doc! target "# Target\n\n## Valid Heading\n")
         (write-doc! source
                     (string-join '("# Source" "## Valid Heading"
                                               "[same](#valid-heading)"
                                               "[file anchor](target.md#valid-heading)"
                                               "[plain file](target.md)"
                                               "[missing anchor](#missing-heading)"
                                               "[missing file](missing.md)"
                                               "[missing file anchor](missing.md#anything)")
                                  "\n"))
         (define broken (box #f))
         (quiet (lambda () (set-box! broken (check-doc-links (list source)))))
         (check-equal? (unbox broken) 3))))))

(module+ test
  (run-tests lint-docs-suite))
