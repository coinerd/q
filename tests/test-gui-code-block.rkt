#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-code-block.rkt — Tests for gui/views/code-block.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/code-block.rkt")

(define-test-suite test-gui-code-block
                   (test-case "render-code-block produces view descriptor"
                     (define result (render-code-block (default-theme) "(+ 1 2)"))
                     (check-equal? (hash-ref result 'view) 'code-block)
                     (check-equal? (hash-ref result 'code) "(+ 1 2)"))
                   (test-case "render-code-block splits lines"
                     (define result (render-code-block (default-theme) "line1\nline2\nline3"))
                     (check-equal? (hash-ref result 'line-count) 3)
                     (check-equal? (hash-ref result 'lines) '("line1" "line2" "line3")))
                   (test-case "render-code-block with language"
                     (define result (render-code-block (default-theme) "(+ 1 2)" #:language "racket"))
                     (check-equal? (hash-ref result 'language) "racket"))
                   (test-case "detect-language extracts from fence"
                     (check-equal? (detect-language "```racket\n(+ 1 2)\n```") "racket")
                     (check-equal? (detect-language "```python\nprint()\n```") "python"))
                   (test-case "detect-language returns #f without fence"
                     (check-false (detect-language "just plain text")))
                   (test-case "render-code-block with line numbers off"
                     (define result (render-code-block (default-theme) "x" #:line-numbers #f))
                     (check-false (hash-ref result 'line-numbers))))

(run-tests test-gui-code-block)
