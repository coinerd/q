#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-edit-mismatch-diagnostics.rkt — W2 readable edit mismatch.
;;
;; The direct edit tool and the sandboxed worker share ONE bounded diagnostic:
;; line/column plus the first-difference offset, a readable diff of the
;; nearest region, and visible escaping for whitespace/control characters.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool.rkt" tool-result-is-error? tool-result-content)
         (only-in "../tools/builtins/edit.rkt" tool-edit)
         (only-in "../tools/builtins/edit-mismatch.rkt"
                  format-not-found-diagnostic
                  visible-text
                  char-description)
         (only-in "../sandbox/ipc-protocol.rkt" ipc-response-error-message)
         (only-in "../sandbox/worker-tools.rkt" current-allowed-roots execute-edit))

(define (result-text result)
  (define c (tool-result-content result))
  (cond
    [(and (pair? c) (hash? (car c))) (hash-ref (car c) 'text "")]
    [(string? c) c]
    [else (format "~a" c)]))

(define mismatch-suite
  (test-suite "edit mismatch diagnostics"

    (test-case "visible escaping never emits raw control characters"
      (check-equal? (visible-text "a\tb\n") "a\\tb\\n")
      (check-equal? (visible-text (string #\u0001)) "\\u{1}")
      (check-true (string-contains? (char-description #\space) "U+20"))
      (check-true (string-contains? (char-description #\tab) "\\t"))
      (check-true (string-contains? (char-description #\u2014) "U+2014")))

    (test-case "diagnostic gives line/column, readable diff, and visible tabs"
      (define content "def foo():\n\tpass\n")
      (define old-text "def foo():\n\tfail\n")
      (define text (format-not-found-diagnostic "/tmp/probe.py" old-text content))
      (check-true (string-contains? text "Nearest match at line"))
      (check-true (string-contains? text "First differing offset:"))
      (check-true (string-contains? text "line 2, column 2"))
      (check-true (string-contains? text "  file:     \\tpass")
                  "file line is readable with an escaped tab")
      (check-true (string-contains? text "  old-text: \\tfail")
                  "old-text line is readable with an escaped tab")
      (check-true (string-contains? text "file line has 0 leading spaces (\\t)")
                  "indentation is visible, not only counted")
      (check-false (regexp-match? #rx"[\t]" text) "no raw tab reaches the model/terminal"))

    (test-case "diagnostic output is bounded for a huge line"
      (define long-line (string-append (make-string 4000 #\a) "X" (make-string 4000 #\b)))
      (define old-line (string-append (make-string 4000 #\a) "Y" (make-string 4000 #\b)))
      (define text (format-not-found-diagnostic "/tmp/big.txt" old-line long-line))
      (check-true (string-contains? text "First differing offset: 4000"))
      (check-true (string-contains? text "…") "clipped context is marked")
      (check-true (< (string-length text) 2500)
                  (format "diagnostic stays bounded, got ~a chars" (string-length text))))

    (test-case "worker and direct edit emit the same readable mismatch contract"
      (define dir (make-temporary-file "q-edit-mismatch-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define path (build-path dir "probe.rkt"))
         (display-to-file "#lang racket/base\n\n(define (foo x) x)\n" path #:exists 'replace)
         (define direct
           (tool-edit
            (hasheq 'path (path->string path) 'old-text "(define (foo y) x)" 'new-text "replacement")
            #f))
         (check-true (tool-result-is-error? direct))
         (define direct-text (result-text direct))
         (define worker
           (parameterize ([current-allowed-roots (list dir)])
             (execute-edit (hasheq 'path
                                   (path->string path)
                                   'old-text
                                   "(define (foo y) x)"
                                   'new-text
                                   "replacement"))))
         (define worker-text (ipc-response-error-message worker))
         (for ([text (in-list (list direct-text worker-text))])
           (check-true (string-contains? text "old-text not found in"))
           (check-true (string-contains? text "First differing offset:"))
           (check-true (string-contains? text "line 3, column 14"))
           (check-true (string-contains? text "  old-text: (define (foo y) x)"))))
       (lambda () (delete-directory/files dir))))))

(module+ test
  (run-tests mismatch-suite))

(module+ main
  (run-tests mismatch-suite))
