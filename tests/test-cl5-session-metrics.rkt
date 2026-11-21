#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-session-metrics.rkt — CL5 W1 (#9982): metric derivation from
;; synthetic pi session JSONL. Covers the happy path (tokens/reads/calls
;; before the first in-scope edit, owner-location time, subsystems), the
;; negative shapes (no in-scope edit, unrelated writes, absolute paths outside
;; the repo root), timestamp parsing, and the I/O shell's skipped-line count.

(require json
         rackunit
         racket/file
         racket/string
         "../scripts/cl5/session-metrics.rkt")

;; ---- synthetic event builders ------------------------------------------------

(define (ts seconds ms)
  (format "2026-11-20T10:00:~a.~aZ" (~a seconds #:width 2 #:align 'right #:pad-string "0") ms))

(define (session-event t)
  (hash 'type "session" 'timestamp t))

(define (model-change t provider model)
  (hash 'type "model_change" 'timestamp t 'provider provider 'modelId model))

(define (thinking-change t level)
  (hash 'type "thinking_level_change" 'timestamp t 'thinkingLevel level))

(define (tool-call name arguments)
  (hash 'type "toolCall" 'id "call-1" 'name name 'arguments arguments))

(define (assistant-event t calls in-tokens out-tokens)
  (hash 'type
        "message"
        'timestamp
        t
        'message
        (hash 'role
              "assistant"
              'content
              calls
              'usage
              (hash 'input
                    in-tokens
                    'output
                    out-tokens
                    'cacheRead
                    0
                    'cacheWrite
                    0
                    'totalTokens
                    (+ in-tokens out-tokens)))))

(define ownership '("tui/char-width.rkt" "tests/tui/test-char-width.rkt"))
(define root (build-path (find-system-path 'temp-dir) "q-trial"))
(define root-string (path->string root))
(define char-width-abs (path->string (build-path root "tui" "char-width.rkt")))

;; Happy path: 2 reads + 1 bash before the edit, edit message inclusive for
;; tokens, a trailing assistant message that must NOT be counted.
(define happy-events
  (list (session-event (ts 0 "000"))
        (model-change (ts 0 "100") "acme" "model-7")
        (thinking-change (ts 0 "200") "high")
        (assistant-event (ts 5 "000")
                         (list (tool-call "read" (hash 'path "docs/reports/cl0.md"))
                               (tool-call "bash" (hash 'command "ls tui/char-width.rkt")))
                         1000
                         100)
        (assistant-event (ts 10 "000")
                         (list (tool-call "read" (hash 'path "docs/reports/cl0.md")) ; duplicate read
                               (tool-call "read" (hash 'path "tui/char-width.rkt")))
                         2000
                         200)
        (assistant-event
         (ts 20 "000")
         (list (tool-call "write" (hash 'path char-width-abs 'content "...")))
         3000
         300)
        (assistant-event (ts 40 "000") '() 9000 900)))

(module+ test
  (test-case "iso8601->ms parses Zulu timestamps with fractions"
    (check-equal? (iso8601->ms "1970-01-01T00:00:00.000Z") 0)
    (check-equal? (iso8601->ms "1970-01-01T00:00:01.500Z") 1500)
    (check-false (iso8601->ms "2026-11-20 10:00:00"))
    (check-false (iso8601->ms "not a timestamp"))
    (check-false (iso8601->ms 42)))

  (test-case "happy path: tokens, reads, calls, times, model, thinking"
    (define metrics (session-metrics happy-events ownership #:repo-root root))
    ;; tokens: 1100 + 2200 + 3300 (edit message inclusive); trailing 9900 excluded
    (check-equal? (hash-ref metrics 'tokens-before-first-in-scope-edit) 6600)
    ;; distinct reads strictly before the edit: cl0.md + char-width.rkt
    (check-equal? (hash-ref metrics 'files-read-before-first-edit) 2)
    ;; investigation calls strictly before the edit: read + bash + read + read
    (check-equal? (hash-ref metrics 'investigation-calls-before-first-edit) 4)
    ;; owner path first named by the bash call at t=5s
    (check-equal? (hash-ref metrics 'owner-location-ms) 5000)
    (check-equal? (hash-ref metrics 'first-edit-ms) 20000)
    (check-equal? (hash-ref metrics 'elapsed-ms) 40000)
    (check-equal? (hash-ref metrics 'written-subsystems) '("tui"))
    (check-equal? (hash-ref metrics 'unrelated-writes) '())
    (check-equal? (hash-ref metrics 'tool-call-count) 5)
    (check-equal? (hash-ref metrics 'assistant-turn-count) 4)
    (check-equal? (hash-ref metrics 'model) "acme/model-7")
    (check-equal? (hash-ref metrics 'thinking-level) "high"))

  (test-case "no in-scope edit: edit-relative fields are #f, writes recorded as unrelated"
    (define events
      (list
       (session-event (ts 0 "000"))
       (assistant-event (ts 1 "000") (list (tool-call "read" (hash 'path "tui/char-width.rkt"))) 10 1)
       (assistant-event (ts 2 "000")
                        (list (tool-call "write" (hash 'path "notes.md" 'content "x")))
                        10
                        1)))
    (define metrics (session-metrics events ownership #:repo-root root))
    (check-false (hash-ref metrics 'tokens-before-first-in-scope-edit))
    (check-false (hash-ref metrics 'files-read-before-first-edit))
    (check-false (hash-ref metrics 'investigation-calls-before-first-edit))
    (check-false (hash-ref metrics 'first-edit-ms))
    (check-equal? (hash-ref metrics 'unrelated-writes) '("notes.md"))
    (check-equal? (hash-ref metrics 'written-subsystems) '())
    ;; owner-location still works without any edit
    (check-equal? (hash-ref metrics 'owner-location-ms) 1000))

  (test-case "absolute path outside the repo root is an unrelated write"
    (define events
      (list (session-event (ts 0 "000"))
            (assistant-event (ts 1 "000")
                             (list (tool-call "write" (hash 'path "/etc/scratch.rkt" 'content "x")))
                             1
                             1)))
    (define metrics (session-metrics events ownership #:repo-root root))
    (check-false (hash-ref metrics 'first-edit-ms))
    (check-equal? (hash-ref metrics 'unrelated-writes) '("/etc/scratch.rkt")))

  (test-case "absolute paths are relativized only with an explicit repo root"
    (define events
      (list (session-event (ts 0 "000"))
            (assistant-event
             (ts 1 "000")
             (list (tool-call "write" (hash 'path char-width-abs 'content "x")))
             1
             1)))
    ;; Without #:repo-root the same absolute write is unrelated.
    (define without-root (session-metrics events ownership))
    (check-false (hash-ref without-root 'first-edit-ms))
    (check-equal? (hash-ref without-root 'unrelated-writes) (list char-width-abs))
    (define with-root (session-metrics events ownership #:repo-root root))
    (check-equal? (hash-ref with-root 'first-edit-ms) 1000))

  (test-case "leading ./ is normalized before ownership comparison"
    (check-equal? (normalize-session-path "./tui/char-width.rkt" #f) "tui/char-width.rkt")
    (define events
      (list (session-event (ts 0 "000"))
            (assistant-event (ts 3 "000")
                             (list (tool-call "edit" (hash 'path "./tui/char-width.rkt" 'edits '())))
                             5
                             5)))
    (define metrics (session-metrics events ownership))
    (check-equal? (hash-ref metrics 'first-edit-ms) 3000))

  (test-case "top-level in-scope write maps to the root subsystem"
    (define events
      (list (session-event (ts 0 "000"))
            (assistant-event (ts 1 "000")
                             (list (tool-call "write" (hash 'path "info.rkt" 'content "x")))
                             1
                             1)))
    (define metrics (session-metrics events '("info.rkt")))
    (check-equal? (hash-ref metrics 'written-subsystems) '("root")))

  (test-case "model_change / thinking_level_change take the LAST occurrence"
    (define events
      (list (session-event (ts 0 "000"))
            (model-change (ts 1 "000") "a" "m1")
            (model-change (ts 2 "000") "b" "m2")
            (thinking-change (ts 1 "000") "low")
            (thinking-change (ts 2 "000") "medium")))
    (define metrics (session-metrics events '()))
    (check-equal? (hash-ref metrics 'model) "b/m2")
    (check-equal? (hash-ref metrics 'thinking-level) "medium"))

  (test-case "no timestamps: elapsed and times are #f, counting still works"
    (define events
      (list (hash 'type
                  "message"
                  'message
                  (hash 'role
                        "assistant"
                        'content
                        (list (tool-call "read" (hash 'path "x.rkt")))
                        'usage
                        (hash 'input 1 'output 1)))))
    (define metrics (session-metrics events '()))
    (check-false (hash-ref metrics 'elapsed-ms))
    (check-false (hash-ref metrics 'owner-location-ms))
    (check-equal? (hash-ref metrics 'tool-call-count) 1))

  (test-case "read-session-events skips unparsable lines and counts them"
    (define tmp (make-temporary-file "cl5-session-~a.jsonl"))
    (dynamic-wind
     (lambda () (void))
     (lambda ()
       (call-with-output-file tmp
                              (lambda (out)
                                (displayln (jsexpr->string (session-event (ts 0 "000"))) out)
                                (displayln "this is not json" out)
                                (displayln "" out)
                                (displayln (jsexpr->string (model-change (ts 1 "000") "a" "b")) out))
                              #:exists 'truncate/replace)
       (define-values (events skipped) (read-session-events tmp))
       (check-equal? (length events) 2)
       (check-equal? skipped 2)
       (define metrics (session-metrics-file tmp '()))
       (check-equal? (hash-ref metrics 'skipped-lines) 2)
       (check-equal? (hash-ref metrics 'model) "a/b")
       (check-equal? (hash-ref metrics 'elapsed-ms) 1000))
     (lambda () (delete-file tmp)))))
