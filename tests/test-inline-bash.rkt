#lang racket

;; q/tests/test-inline-bash.rkt — Tests for G3.3 inline bash expansion
;;
;; Tests the expand-inline-bash function from q/tui/input.rkt.
;; `!!` at start of line expands to the last prompt text.

(require rackunit
         rackunit/text-ui
         "../tui/input.rkt")

(define inline-bash-suite
  (test-suite "Inline bash expansion tests"
    (test-case "!! expands to last prompt text"
      (check-equal? (expand-inline-bash "!!" "list files") "list files"))
    (test-case "!! with extra text expands and appends"
      (check-equal? (expand-inline-bash "!! some extra" "list files") "list files some extra"))
    (test-case "!! with no last prompt stays as !!"
      (check-equal? (expand-inline-bash "!!" #f) "!!"))
    (test-case "!! with extra text and no last prompt stays as-is"
      (check-equal? (expand-inline-bash "!! and more" #f) "!! and more"))
    (test-case "regular text passes through unchanged"
      (check-equal? (expand-inline-bash "hello world" "previous") "hello world"))
    (test-case "!ls (single !) passes through unchanged"
      (check-equal? (expand-inline-bash "!ls -la" "previous") "!ls -la"))
    (test-case "! at end of text passes through"
      (check-equal? (expand-inline-bash "hello!" "previous") "hello!"))
    (test-case "!! in middle of text is not expanded"
      (check-equal? (expand-inline-bash "say !! now" "previous") "say !! now"))
    (test-case "empty string passes through"
      (check-equal? (expand-inline-bash "" "previous") ""))
    (test-case "!! with empty last prompt expands to empty + rest"
      (check-equal? (expand-inline-bash "!! extra" "") " extra"))))

(run-tests inline-bash-suite)
