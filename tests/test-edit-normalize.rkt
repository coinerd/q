#lang racket

(require rackunit
         "../tools/builtins/edit-normalize.rkt")

(define (slice s span)
  (and span (substring s (car span) (cdr span))))

(module+ test
  (test-case "normalize-text preserves ordinary text"
    (check-equal? (normalize-text "alpha\nbeta") "alpha\nbeta"))

  (test-case "normalize-text normalizes CRLF and CR newlines"
    (check-equal? (normalize-text "a\r\nb\rc") "a\nb\nc"))

  (test-case "normalize-text strips trailing spaces and tabs per line"
    (check-equal? (normalize-text "a  \n b\t\n") "a\n b"))

  (test-case "normalize-text collapses repeated blank lines"
    (check-equal? (normalize-text "a\n\n\n\nb") "a\n\nb"))

  (test-case "normalize-text expands tabs to spaces"
    (check-equal? (normalize-text "\ta\n\t\tb") "  a\n    b"))

  (test-case "normalize-text trims leading and trailing blank lines"
    (check-equal? (normalize-text "\n\nalpha\n\n") "alpha"))

  (test-case "normalize-text can preserve trailing whitespace when configured"
    (define opts (normalize-options #f #t #t #t 2 #t))
    (check-equal? (normalize-text "a  \n" opts) "a  "))

  (test-case "normalize-text can preserve repeated blank lines when configured"
    (define opts (normalize-options #t #t #f #t 2 #t))
    (check-equal? (normalize-text "a\n\n\nb" opts) "a\n\n\nb"))

  (test-case "normalize-crlf can be disabled independently"
    (define opts (normalize-options #t #f #t #t 2 #t))
    (check-equal? (normalize-text "a\r\nb" opts) "a\r\nb")
    (check-equal? (normalize-text "a  \r\nb" opts) "a\r\nb"))

  (test-case "normalize-text can preserve boundary blank lines when configured"
    (define opts (normalize-options #t #t #t #t 2 #f))
    (check-equal? (normalize-text "\nalpha\n") "alpha")
    (check-equal? (normalize-text "\nalpha\n" opts) "\nalpha\n"))

  (test-case "similarity-score exact and empty cases"
    (check-= (similarity-score "abc" "abc") 1.0 0.0)
    (check-= (similarity-score "" "") 1.0 0.0)
    (check-equal? (similarity-score "abc" "") 0.0)
    (check-equal? (similarity-score "" "abc") 0.0))

  (test-case "similarity-score partial overlap"
    (check > (similarity-score "abcdef" "abcXYZ") 0.4)
    (check < (similarity-score "abcdef" "XYZ") 0.4))

  (test-case "fuzzy-find-match finds exact match coordinates"
    (define s "one\ntwo\nthree")
    (define span (fuzzy-find-match s "two"))
    (check-equal? span (cons 4 7))
    (check-equal? (slice s span) "two"))

  (test-case "fuzzy-find-match tolerates trailing whitespace drift"
    (define s "alpha   \nbeta\ngamma")
    (define span (fuzzy-find-match s "alpha\nbeta"))
    (check-equal? (slice s span) "alpha   \nbeta"))

  (test-case "fuzzy-find-match tolerates old-text trailing whitespace drift"
    (define s "alpha\nbeta\ngamma")
    (define span (fuzzy-find-match s "alpha   \nbeta   "))
    (check-equal? (slice s span) "alpha\nbeta"))

  (test-case "fuzzy-find-match tolerates CRLF drift"
    (define s "alpha\r\nbeta\r\ngamma")
    (define span (fuzzy-find-match s "alpha\nbeta"))
    (check-equal? (slice s span) "alpha\r\nbeta"))

  (test-case "fuzzy-find-match tolerates tab indentation drift"
    (define s "(define (f)\n\t(+ 1 2))")
    (define span (fuzzy-find-match s "(define (f)\n  (+ 1 2))"))
    (check-equal? (slice s span) "(define (f)\n\t(+ 1 2))"))

  (test-case "fuzzy-find-match tolerates extra blank line drift"
    (define s "alpha\n\n\nbeta\ngamma")
    (define span (fuzzy-find-match s "alpha\n\nbeta"))
    (check-equal? (slice s span) "alpha\n\n\nbeta"))

  (test-case "fuzzy-find-match trims boundary blank lines in old-text"
    (define s "alpha\nbeta\ngamma")
    (define span (fuzzy-find-match s "\n\nalpha\nbeta\n"))
    (check-equal? (slice s span) "alpha\nbeta"))

  (test-case "fuzzy-find-match rejects absent text"
    (check-false (fuzzy-find-match "alpha\nbeta" "delta")))

  (test-case "fuzzy-find-match rejects low threshold mismatch"
    (check-false (fuzzy-find-match "alpha beta gamma" "alpha zeta gamma" #:threshold 0.99))
    (check-not-false (fuzzy-find-match "alpha beta gamma" "alpha zeta gamma" #:threshold 0.70)))

  (test-case "fuzzy-find-match accepts threshold-compatible normalized match"
    (check-not-false (fuzzy-find-match "alpha   \nbeta" "alpha\nbeta" #:threshold 0.85)))

  (test-case "fuzzy-find-match returns original coordinates for middle match"
    (define s "prefix\nalpha   \nbeta\nsuffix")
    (define span (fuzzy-find-match s "alpha\nbeta"))
    (check-equal? (slice s span) "alpha   \nbeta"))

  (test-case "fuzzy-find-match handles multiple whitespace normalizations together"
    (define s "before\r\n\talpha   \r\n\r\n\r\n\tbeta\r\nafter")
    (define span (fuzzy-find-match s "  alpha\n\n  beta"))
    (check-equal? (slice s span) "\talpha   \r\n\r\n\r\n\tbeta"))

  (test-case "normalization suite covers many whitespace variants"
    (define variants
      (list "a\nb"
            "a  \nb"
            "a\r\nb"
            "\na\nb\n"
            "a\n\n\nb"
            "a\t\nb"
            "\t(a)\n\t(b)"
            "x\r\ny\r\nz"
            "x   \n\n\ny   "
            "\n\n\tcall\t\n\n"))
    (for ([v (in-list variants)])
      (check-true (string? (normalize-text v)))
      (check-not-false (regexp-match? #rx"^[^\r]*$" (normalize-text v)))))

  (test-case "coordinate mapping never returns out-of-range spans"
    (for ([old (in-list (list "a\nb" "a  \nb" "\na\nb\n" "a\r\nb"))])
      (define s (string-append "prefix\n" old "\nsuffix"))
      (define span (fuzzy-find-match s "a\nb"))
      (check-not-false span)
      (check-true (<= 0 (car span) (cdr span) (string-length s)))))

  (test-case "additional normalization examples"
    (check-equal? (normalize-text "x\r\ny") "x\ny")
    (check-equal? (normalize-text "x\ry") "x\ny")
    (check-equal? (normalize-text "x  ") "x")
    (check-equal? (normalize-text "x\t") "x")
    (check-equal? (normalize-text "\t") "")
    (check-equal? (normalize-text "\tfoo") "  foo")
    (check-equal? (normalize-text "foo\n\n") "foo")
    (check-equal? (normalize-text "\n\nfoo") "foo")
    (check-equal? (normalize-text "foo\n\nbar") "foo\n\nbar")
    (check-equal? (normalize-text "foo\n\n\nbar") "foo\n\nbar")
    (check-not-false (fuzzy-find-match "x\nfoo  \nbar\ny" "foo\nbar"))
    (check-not-false (fuzzy-find-match "x\r\nfoo\r\nbar" "foo\nbar"))
    (check-not-false (fuzzy-find-match "x\n\tfoo" "  foo"))
    (check-false (fuzzy-find-match "foo" "bar")))

  (test-case "crlf fuzzy case 1"
    (check-not-false (fuzzy-find-match "a\r\nb" "a\nb")))
  (test-case "trailing-space fuzzy case 2"
    (check-not-false (fuzzy-find-match "a  \nb" "a\nb")))
  (test-case "old-text trailing-space fuzzy case 3"
    (check-not-false (fuzzy-find-match "a\nb" "a  \nb")))
  (test-case "boundary blank fuzzy case 4"
    (check-not-false (fuzzy-find-match "a\nb" "\na\nb\n")))
  (test-case "tab fuzzy case 5"
    (check-not-false (fuzzy-find-match "\ta" "  a")))
  (test-case "blank collapse normalize case 6"
    (check-equal? (normalize-text "a\n\n\n\nb") "a\n\nb"))
  (test-case "empty normalize case 7"
    (check-equal? (normalize-text "") ""))
  (test-case "spaces-only normalize case 8"
    (check-equal? (normalize-text "   ") ""))
  (test-case "tabs-only normalize case 9"
    (check-equal? (normalize-text "\t\t") ""))
  (test-case "similarity high case 10"
    (check > (similarity-score "abcdef" "abcxef") 0.8))
  (test-case "similarity low case 11"
    (check < (similarity-score "abcdef" "uvwxyz") 0.2))
  (test-case "reject missing fuzzy case 12"
    (check-false (fuzzy-find-match "abcdef" "ghijk")))
  (test-case "empty normalized fuzzy does not crash case 12b"
    (check-false (fuzzy-find-match "   " "\t"))
    (check-false (fuzzy-find-match "abc" ""))
    (check-false (fuzzy-find-match "   " "   ")))
  (test-case "middle span fuzzy case 13"
    (define s "111\nabc  \ndef\n222")
    (check-equal? (slice s (fuzzy-find-match s "abc\ndef")) "abc  \ndef"))

  (test-case "default-normalization-options is a normalize-options value"
    (check-true (normalize-options? default-normalization-options))
    (check-equal? (normalize-options-tab-width default-normalization-options) 2)))
