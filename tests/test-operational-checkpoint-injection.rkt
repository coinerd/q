#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; Issue #9064: provider-visible operational checkpoint wiring.

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         (only-in "../runtime/context-assembly/operational-checkpoint.rkt"
                  make-empty-checkpoint
                  checkpoint-set-repo-root
                  inject-checkpoint-message)
         (only-in "../runtime/context-assembly/turn-context.rkt" assemble-context/pure)
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../util/message/message.rkt"
                  make-message
                  message?
                  message-id
                  message-role
                  message-content)
         (only-in "../util/content/content-parts.rkt"
                  make-text-part
                  make-tool-call-part
                  make-tool-result-part
                  text-part?
                  text-part-text))

(define (msg id role kind parts [parent-id #f])
  (make-message id parent-id role kind parts 0 (hasheq)))

(define base-history
  (list (msg "u1" 'user 'message (list (make-text-part "hello")))
        (msg "a1" 'assistant 'message (list (make-text-part "world")))))

(define (config repo planning)
  (hash->session-config (hash 'repo-root
                              repo
                              'planning-root
                              planning
                              'project-dir
                              "/workspace"
                              'tier-b-count
                              20
                              'tier-c-count
                              4
                              'max-tokens
                              4096)))

(define (checkpoint-messages messages)
  (filter (lambda (m) (equal? (message-id m) "op-checkpoint")) messages))

(define (checkpoint-text messages)
  (define checkpoints (checkpoint-messages messages))
  (check-equal? (length checkpoints) 1)
  (define parts (message-content (car checkpoints)))
  (check-equal? (length parts) 1)
  (check-pred text-part? (car parts))
  (text-part-text (car parts)))

(define (assemble history cfg)
  (define-values (messages _hook _tiered) (assemble-context/pure history cfg))
  messages)

(define (read-history path #:error? [error? #f])
  (list (msg "read-call"
             'assistant
             'message
             (list (make-tool-call-part "read-1" "read" (hasheq 'path path))))
        (msg "read-result"
             'tool
             'tool-result
             (list (make-tool-result-part "read-1"
                                          (if error? "File not found" "authoritative contents")
                                          error?))
             "read-call")))

(define injection-tests
  (test-suite "Operational Checkpoint Injection (#9064)"

    (test-case "inject-checkpoint-message creates an internal message with text content"
      (define cp (checkpoint-set-repo-root (make-empty-checkpoint) "/test-area/q"))
      (define injected (inject-checkpoint-message cp base-history))
      (check-equal? (length injected) 3)
      (check-pred message? (car injected))
      (check-eq? (message-role (car injected)) 'system)
      (check-pred text-part? (car (message-content (car injected))))
      (check-true (string-contains? (text-part-text (car (message-content (car injected))))
                                    "/test-area/q")))

    (test-case "assemble-context/pure exposes one checkpoint as the first provider message"
      (define assembled (assemble base-history (config "/test-area/q" "/test-area/.planning")))
      (check-equal? (message-id (car assembled)) "op-checkpoint")
      (check-equal? (length (checkpoint-messages assembled)) 1)
      (define text (checkpoint-text assembled))
      (check-true (string-contains? text "/test-area/q"))
      (check-true (string-contains? text "/test-area/.planning")))

    (test-case "repeated pure assembly replaces rather than accumulates checkpoints"
      (define cfg (config "/repo" "/planning"))
      (define once (assemble base-history cfg))
      (define twice (assemble once cfg))
      (check-equal? (message-id (car twice)) "op-checkpoint")
      (check-equal? (length (checkpoint-messages twice)) 1))

    (test-case "checkpoint assembly is isolated between session configs"
      (define a (assemble base-history (config "/repo/a" "/plans/a")))
      (define b (assemble base-history (config "/repo/b" "/plans/b")))
      (check-true (string-contains? (checkpoint-text a) "/repo/a"))
      (check-false (string-contains? (checkpoint-text a) "/repo/b"))
      (check-true (string-contains? (checkpoint-text b) "/repo/b"))
      (check-false (string-contains? (checkpoint-text b) "/repo/a")))

    (test-case "only a successful correlated read activates named planning authority"
      (define named "/plans/VALIDATION-v0.99.73-W13.md")
      (define cfg (config "/repo" "/plans"))
      (define success (assemble (append base-history (read-history named)) cfg))
      (define failure (assemble (append base-history (read-history named #:error? #t)) cfg))
      (check-true (string-contains? (checkpoint-text success) named))
      (check-false (string-contains? (checkpoint-text failure) named)))

    (test-case "successful discovery replaces outer fallback coordinates"
      (define canonical-plan "/canonical/.planning/VALIDATION-v0.99.73-W13.md")
      (define discovery
        (list (msg "git-call"
                   'assistant
                   'message
                   (list (make-tool-call-part
                          "git-1"
                          "bash"
                          (hasheq 'command "git -C '/canonical/repo' rev-parse --show-toplevel"))))
              (msg "git-result"
                   'tool
                   'tool-result
                   (list (make-tool-result-part "git-1"
                                                (list (hasheq 'type "text" 'text "/canonical/repo\n"))
                                                #f))
                   "git-call")))
      (define assembled
        (assemble (append base-history discovery (read-history canonical-plan))
                  (config "/outer" "/outer/.planning")))
      (define text (checkpoint-text assembled))
      (check-true (string-contains? text "repo-root:       /canonical/repo"))
      (check-true (string-contains? text "planning-root:   /canonical/.planning"))
      (check-false (string-contains? text "repo-root:       /outer")))

    (test-case "planning-read base_dir establishes canonical planning root"
      (define named "VALIDATION-v0.99.73-W13.md")
      (define history
        (list (msg "planning-call"
                   'assistant
                   'message
                   (list (make-tool-call-part "planning-1"
                                              "planning-read"
                                              (hasheq 'artifact named 'base_dir "/canonical/repo"))))
              (msg "planning-result"
                   'tool
                   'tool-result
                   (list (make-tool-result-part "planning-1"
                                                (list (hasheq 'type "text" 'text "accepted"))
                                                #f))
                   "planning-call")))
      (define assembled (assemble (append base-history history) (config "/outer" "/outer/.planning")))
      (define text (checkpoint-text assembled))
      (check-true (string-contains? text "planning-root:   /canonical/repo/.planning"))
      (check-true (string-contains? text "/canonical/repo/.planning/VALIDATION-v0.99.73-W13.md")))))

(module+ test
  (run-tests injection-tests))

(module+ main
  (run-tests injection-tests))
