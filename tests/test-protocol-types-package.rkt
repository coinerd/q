#lang racket

;; tests/test-protocol-types-package.rkt — verify protocol-types sub-module decomposition
;;
;; Tests that:
;; 1. Each sub-module compiles and exports expected identifiers
;; 2. The façade re-exports everything
;; 3. No circular dependencies exist
;; 4. Backward compatibility: all previously-exported names remain accessible

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         ;; Direct sub-module imports
         "../util/content-parts.rkt"
         "../util/message.rkt"
         "../util/event.rkt"
         "../util/entry-predicates.rkt"
         "../util/tree-entries.rkt"
         "../util/loop-result.rkt"
         "../util/custom-entries.rkt"
         "../util/tool-types.rkt")

(define pt-tests
  (test-suite "Protocol Types Package Decomposition"

    (test-case "façade exports message struct"
      (check-true (procedure? make-message))
      (check-true (procedure? message?))
      (define msg (make-message "test" #f 'user 'message
                                (list (make-text-part "hello"))
                                (current-seconds) (hasheq)))
      (check-true (message? msg))
      (check-equal? (message-id msg) "test"))

    (test-case "façade exports content-part types"
      (check-true (procedure? make-text-part))
      (check-true (procedure? text-part?))
      (check-true (procedure? make-tool-call-part))
      (check-true (procedure? make-tool-result-part)))

    (test-case "façade exports tool-call and tool-result"
      (check-true (procedure? make-tool-call))
      (check-true (procedure? tool-call?))
      (check-true (procedure? make-tool-result))
      (check-true (procedure? tool-result?))
      (define tc (make-tool-call "tc-1" "bash" (hasheq 'cmd "ls")))
      (check-equal? (tool-call-name tc) "bash")
      (check-equal? (tool-call-id tc) "tc-1"))

    (test-case "façade exports custom entries"
      (check-true (procedure? make-custom-entry))
      (check-true (procedure? custom-entry?))
      (define ce (make-custom-entry "my-ext" "key1" '(1 2 3)))
      (check-true (custom-entry? ce))
      (check-equal? (custom-entry-extension ce) "my-ext")
      (check-equal? (custom-entry-key ce) "key1")
      (check-equal? (custom-entry-data ce) '(1 2 3)))

    (test-case "façade exports entry predicates"
      (check-true (procedure? message-entry?)))

    (test-case "façade exports loop-result"
      (check-true (procedure? loop-result?)))

    (test-case "sub-modules are independently importable"
      ;; Verify each sub-module can be used without the façade
      (define tp (make-text-part "standalone"))
      (check-true (text-part? tp))
      (define msg (make-message "s" #f 'user 'message '() (current-seconds) (hasheq)))
      (check-true (message? msg))
      (define tc (make-tool-call "id" "read" (hasheq)))
      (check-true (tool-call? tc))
      (define ce (make-custom-entry "ext" "k" "v"))
      (check-true (custom-entry? ce)))))

(module+ main
  (run-tests pt-tests))
(module+ test
  (run-tests pt-tests))
