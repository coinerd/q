#lang racket/base

;; tests/test-typed-tool-outcome.rkt
;; W3A (#8940): Typed outcome contracts at the tool boundary.
;;
;; Tests that a (tool-call, tool-result) pair can be classified into a
;; safe typed-tool-outcome with no raw content, no secrets, and correct
;; provenance (tool-call-id correlation).

(require rackunit
         rackunit/text-ui
         racket/string
         "../util/tool/tool-types.rkt"
         "../util/outcome/outcome-types.rkt")

(define-test-suite
 typed-tool-outcome-suite
 ;; ── Struct construction and accessors ──
 (test-case "typed-tool-outcome struct has all required fields"
   (define o
     (make-typed-tool-outcome #:kind 'artifact
                              #:tool-call-id "call-1"
                              #:tool-name "edit"
                              #:status 'success
                              #:payload (hash 'path "src/foo.rkt" 'generation 1)
                              #:timestamp 1000))
   (check-equal? (typed-tool-outcome-kind o) 'artifact)
   (check-equal? (typed-tool-outcome-tool-call-id o) "call-1")
   (check-equal? (typed-tool-outcome-tool-name o) "edit")
   (check-equal? (typed-tool-outcome-status o) 'success)
   (check-equal? (typed-tool-outcome-timestamp o) 1000)
   (check-equal? (hash-ref (typed-tool-outcome-payload o) 'path) "src/foo.rkt"))
 (test-case "typed-tool-outcome is transparent"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c2"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (check-not-false (typed-tool-outcome? o)))
 (test-case "make-typed-tool-outcome rejects invalid kind"
   (check-exn exn:fail?
              (lambda ()
                (make-typed-tool-outcome #:kind 'bogus
                                         #:tool-call-id "c"
                                         #:tool-name "x"
                                         #:status 'success
                                         #:payload (hash)
                                         #:timestamp #f))))
 (test-case "make-typed-tool-outcome rejects invalid status"
   (check-exn exn:fail?
              (lambda ()
                (make-typed-tool-outcome #:kind 'test
                                         #:tool-call-id "c"
                                         #:tool-name "x"
                                         #:status 'bogus
                                         #:payload (hash)
                                         #:timestamp #f))))
 ;; ── Outcome kind enum ──
 (test-case "all documented outcome kinds are valid"
   (for ([k '(prompt artifact
                     test
                     git-status
                     commit
                     push
                     conclusion
                     task-transition
                     checkpoint
                     archive)])
     (check-true (valid-outcome-kind? k) (format "kind ~a should be valid" k))))
 (test-case "unknown kind is invalid"
   (check-false (valid-outcome-kind? 'unknown-kind)))
 ;; ── Status enum ──
 (test-case "all documented statuses are valid"
   (for ([s '(success error cancelled timeout partial)])
     (check-true (valid-outcome-status? s) (format "status ~a should be valid" s))))
 ;; ── Classifier: edit tool → artifact outcome ──
 (test-case "classify edit success → artifact outcome with mutation gen"
   (define tc (make-tool-call "call-1" "edit" (hash 'path "src/foo.rkt")))
   (define tr (make-tool-result "Edited src/foo.rkt" (hash 'occurrences 1) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'artifact)
   (check-equal? (typed-tool-outcome-tool-call-id o) "call-1")
   (check-equal? (typed-tool-outcome-tool-name o) "edit")
   (check-equal? (typed-tool-outcome-status o) 'success)
   (check-equal? (hash-ref (typed-tool-outcome-payload o) 'path) "src/foo.rkt"))
 (test-case "classify write success → artifact outcome"
   (define tc (make-tool-call "call-2" "write" (hash 'path "new.rkt")))
   (define tr (make-tool-result "Wrote new.rkt" #f #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'artifact))
 ;; ── Classifier: bash tool → test outcome if test command ──
 (test-case "classify bash with raco test → test outcome"
   (define tc (make-tool-call "call-3" "bash" (hash 'command "raco test tests/foo.rkt")))
   (define tr (make-tool-result "14 tests passed" (hash 'exit-code 0) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'test)
   (check-equal? (typed-tool-outcome-status o) 'success)
   (check-equal? (hash-ref (typed-tool-outcome-payload o) 'command-class) 'raco-test))
 (test-case "classify bash with failing tests → test outcome with error status"
   (define tc (make-tool-call "call-4" "bash" (hash 'command "raco test tests/foo.rkt")))
   (define tr (make-tool-result "3 failures" (hash 'exit-code 3) #t))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'test)
   (check-equal? (typed-tool-outcome-status o) 'error))
 (test-case "classify bash with git status → git-status outcome"
   (define tc (make-tool-call "call-5" "bash" (hash 'command "git status --porcelain")))
   (define tr (make-tool-result "M src/a.rkt" (hash 'exit-code 0) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'git-status))
 (test-case "classify bash with git commit → commit outcome"
   (define tc (make-tool-call "call-6" "bash" (hash 'command "git commit -m feat")))
   (define tr (make-tool-result "[main abc1234] feat" (hash 'exit-code 0) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'commit))
 (test-case "classify bash with git push → push outcome"
   (define tc (make-tool-call "call-7" "bash" (hash 'command "git push origin main")))
   (define tr (make-tool-result "Pushed" (hash 'exit-code 0) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'push))
 ;; ── Classifier: record-conclusion → conclusion outcome ──
 (test-case "classify record-conclusion → conclusion outcome"
   (define tc (make-tool-call "call-8" "record_conclusion" (hash 'text "Done")))
   (define tr (make-tool-result "Recorded" #f #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-kind o) 'conclusion))
 ;; ── Classifier: error results ──
 (test-case "classify error result → outcome with error status"
   (define tc (make-tool-call "call-9" "edit" (hash 'path "x.rkt")))
   (define tr (make-tool-result "File not found" #f #t))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   (check-equal? (typed-tool-outcome-status o) 'error))
 ;; ── Safety: no raw content in payload ──
 (test-case "payload never contains raw tool-result content string"
   (define tc (make-tool-call "call-10" "bash" (hash 'command "raco test tests/foo.rkt")))
   (define tr (make-tool-result "SECRET_API_KEY=abc123 in output" (hash 'exit-code 0) #f))
   (define o (classify-tool-outcome tc tr))
   (check-not-false o)
   ;; The raw content string must NOT appear in the payload
   (define pl (typed-tool-outcome-payload o))
   (check-false (hash-has-key? pl 'content))
   (for ([(k v) (in-hash pl)])
     (when (string? v)
       (check-false (string-contains? v "SECRET_API_KEY")
                    (format "payload key ~a leaked raw content" k)))))
 ;; ── Classifier: unknown/unclassifiable tool → #f ──
 (test-case "classify unknown tool with no arguments → #f"
   (define tc (make-tool-call #f "some-unknown-tool" #f))
   (define tr (make-tool-result "ok" #f #f))
   (define o (classify-tool-outcome tc tr))
   (check-false o))
 ;; ── Safe hash extraction ──
 (test-case "extract-safe-path from arguments hash"
   (define args (hash 'path "src/foo.rkt" 'other "ignored"))
   (check-equal? (extract-safe-path args) "src/foo.rkt"))
 (test-case "extract-safe-path returns #f when missing"
   (check-false (extract-safe-path (hash 'x 1))))
 (test-case "classify-command-class detects raco test"
   (check-equal? (classify-command-class "raco test tests/foo.rkt") 'raco-test)
   (check-equal? (classify-command-class "raco test") 'raco-test))
 (test-case "classify-command-class detects git commands"
   (check-equal? (classify-command-class "git status") 'git-status)
   (check-equal? (classify-command-class "git commit -m foo") 'git-commit)
   (check-equal? (classify-command-class "git push origin main") 'git-push))
 (test-case "classify-command-class returns 'other for unknown"
   (check-equal? (classify-command-class "ls -la") 'other)))

(run-tests typed-tool-outcome-suite)
