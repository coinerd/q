#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers extensions/gsd/generated-readme.rkt
(require rackunit racket/list racket/string
         "../extensions/gsd/generated-readme.rkt")
(define inputs
  '(("source.rkt" . "#lang racket/base\n")
    ("tests/probe.rkt" . "#lang racket/base\n(check-true #t) (check-true #t)\n; (check-comment)\n")
    ("nested/tests/second.rkt" . "last line without newline")
    ("x/compiled/ignored.rkt" . "(check-ignored)\n")
    ("x/__pycache__/ignored.rkt" . "(check-ignored)\n")
    ("notes.txt" . "(check-ignored)\n")))
(define before
  (string-append "Unchanged prose: 123 test lines.\n## Test Suite\n| Source modules | 1 |\n| Test files | 2 |\n"
                 "| Source lines | 1 |\n| Test lines | 0 |\n| Test assertions | 0 |\n## Other\n"
                 "Do not sync prose or markers <!-- METRICS: test-lines -->.\n"))
(module+ test
  (test-case "recognized semantics: tracked filters, unterminated lines, assertion-BEARING lines"
    (check-equal? (generated-metrics inputs)
                  '(("Source modules" . 1) ("Test files" . 2) ("Source lines" . 1)
                    ("Test lines" . 4) ("Test assertions" . 2)))
    (define after (generated-readme-correction before inputs))
    (check-equal? after (string-replace (string-replace before "| Test lines | 0 |" "| Test lines | 4 |")
                                      "| Test assertions | 0 |" "| Test assertions | 2 |"))
    (check-equal? (generated-readme-correction after inputs) after)
    (check-equal? (generated-readme-correction before (reverse inputs)) after))
  (for ([bad (in-list (list (string-append before "| Test lines | 3 |\n")
                            (string-replace before "## Other" "|Test lines|99|\n## Other")
                            (string-append before "## Test Suite extra\n|Test assertions|9|\n")
                            (string-replace before "| Test lines | 0 |" "| Test lines | unknown |")
                            (string-replace before "## Test Suite" "## Not the actual generated section")
                            (string-replace before "| Test files | 2 |" "| Test files | 9 |")
                            (string-replace before "| Source modules | 1 |" "| Source modules | 2 |")
                            (string-replace before "| Source lines | 1 |" "| Source lines | 2 |")))])
    (test-case "ambiguous/missing rows and non-correction metrics fail closed"
      (check-exn exn:fail? (lambda () (generated-readme-correction bad inputs))))))
