#lang racket/base

(require rackunit
         rackunit/text-ui
         "../util/tool/tool-display.rkt"
         json)

(define-test-suite test-tool-display
  (test-case "extract-arg-summary returns #f for #f"
    (check-false (extract-arg-summary #f)))

  (test-case "extract-arg-summary extracts command from hash"
    (check-equal? (extract-arg-summary (hasheq 'command "ls -la")) "ls -la"))

  (test-case "extract-arg-summary extracts path from hash"
    (check-equal? (extract-arg-summary (hasheq 'path "/tmp/test.rkt")) "/tmp/test.rkt"))

  (test-case "extract-arg-summary extracts pattern from hash"
    (check-equal? (extract-arg-summary (hasheq 'pattern "TODO")) "TODO"))

  (test-case "extract-arg-summary truncates long values"
    (define long-cmd (make-string 200 #\x))
    (check-true (<= (string-length (extract-arg-summary (hasheq 'command long-cmd))) 103)))

  (test-case "extract-arg-summary returns #f for hash without known keys"
    (check-false (extract-arg-summary (hasheq 'foo "bar"))))

  (test-case "extract-arg-summary parses JSON string"
    (check-equal? (extract-arg-summary (jsexpr->string (hasheq 'command "grep pattern")))
                  "grep pattern"))

  (test-case "format-tool-call-display with detail"
    (check-equal? (format-tool-call-display "bash" (hasheq 'command "ls"))
                  "[tool: bash: ls]"))

  (test-case "format-tool-call-display without detail"
    (check-equal? (format-tool-call-display "bash" #f)
                  "[tool: bash]"))
  )

(run-tests test-tool-display)
