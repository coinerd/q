#lang racket/base

;; q/tests/test-gsd-retry-prompt-hardening.rkt — W3 (#9514, #9515) of the
;; executor-hardening release.
;;
;; Retry-prompt hardening for the /go campaign coordinator:
;;   1. `executor-reanchor-prompt` (prompts.rkt) — pure constructor; the
;;      message sent into the SAME executor session when a turn ended with
;;      reasoning but no output ('approved-empty / empty-stream). Must
;;      restate the executor role verbatim, name wave-id + campaign-id,
;;      carry the one-line task and the last tool result excerpt, and ORDER
;;      continuation of implementation (never "what would you like?").
;;   2. `wave-failure-context-block` (prompts.rkt) — pure constructor for the
;;      no-change retry suffix: verbatim verifier message + target file
;;      list + imperative "apply the first edit now".
;;   3. Orchestrator bounded no-change retry (#9515): on the delivery
;;      verifier's "no wave target files changed: ..." verdict the wave is
;;      retried ONCE with that failure-context block; a second consecutive
;;      no-change rejection fails permanently; "verifier rejected" (empty
;;      message) does NOT consume the retry.

(require racket/format
         racket/match
         racket/string
         rackunit
         "../extensions/gsd/prompts.rkt")

;; ============================================================
;; 1. executor-reanchor-prompt (#9514)

;; count-substring: defined BEFORE first use (module-level order matters).
(define (count-substring needle haystack)
  (define n (string-length needle))
  (let loop ([i 0]
             [count 0])
    (cond
      [(> (+ i n) (string-length haystack)) count]
      [(string=? (substring haystack i (+ i n)) needle) (loop (add1 i) (add1 count))]
      [else (loop (add1 i) count)])))
;; ============================================================

(define reanchor
  (executor-reanchor-prompt "W3"
                            "camp-77"
                            "Add re-anchor prompt constructor"
                            "(edit applied: prompts.rkt:40)"))

(test-case "re-anchor restates the executor role verbatim"
  (check-equal? (count-substring executor-reanchor-role-line reanchor) 1))

(test-case "re-anchor names wave-id, campaign-id, task, tool excerpt"
  (check-pred (lambda (s) (string-contains? s "W3")) reanchor)
  (check-pred (lambda (s) (string-contains? s "camp-77")) reanchor)
  (check-pred (lambda (s) (string-contains? s "Add re-anchor prompt constructor")) reanchor)
  (check-pred (lambda (s) (string-contains? s "(edit applied: prompts.rkt:40)")) reanchor))

(test-case "re-anchor orders continuation, never asks the user"
  (check-pred (lambda (s) (string-contains? (string-downcase s) "continue")) reanchor)
  (check-false (string-contains? (string-downcase reanchor) "what would you like")
               "must not read as an interactive assistant turn")
  (check-false (string-contains? (string-downcase reanchor) "how can i help")
               "must not read as an interactive assistant turn"))

(test-case "re-anchor is pure — repeated calls identical"
  (check-equal? reanchor
                (executor-reanchor-prompt "W3"
                                          "camp-77"
                                          "Add re-anchor prompt constructor"
                                          "(edit applied: prompts.rkt:40)")))

;; ============================================================
;; 2. wave-failure-context-block (#9515)
;; ============================================================

(define ctx-block
  (wave-failure-context-block
   "no wave target files changed: q/extensions/gsd/prompts.rkt, q/extensions/gsd/go-orchestrator.rkt"
   '("q/extensions/gsd/prompts.rkt" "q/extensions/gsd/go-orchestrator.rkt")))

(test-case "failure-context block carries the verbatim verifier message"
  (check-pred
   (lambda (s)
     (string-contains?
      s
      "no wave target files changed: q/extensions/gsd/prompts.rkt, q/extensions/gsd/go-orchestrator.rkt"))
   ctx-block))

(test-case "failure-context block lists every target file"
  (for ([f '("q/extensions/gsd/prompts.rkt" "q/extensions/gsd/go-orchestrator.rkt")])
    (check-pred (lambda (s) (string-contains? s f)) ctx-block)))

(test-case "failure-context block orders the first edit now"
  (check-pred (lambda (s) (string-contains? (string-downcase s) "apply the first edit now"))
              ctx-block)
  (check-pred (lambda (s) (string-contains? (string-downcase s) "zero")) ctx-block))

(test-case "failure-context block is pure"
  (check-equal?
   ctx-block
   (wave-failure-context-block
    "no wave target files changed: q/extensions/gsd/prompts.rkt, q/extensions/gsd/go-orchestrator.rkt"
    '("q/extensions/gsd/prompts.rkt" "q/extensions/gsd/go-orchestrator.rkt"))))

;; ============================================================
;; 3. Orchestrator no-change retry (#9515) — via policy knobs
;; ============================================================

(require (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-wave-no-change-retries
                  current-gsd-wave-failure-context))

(test-case "no-change retry budget defaults to exactly one"
  (check-equal? (current-gsd-wave-no-change-retries) 1))

(test-case "failure-context parameter defaults to #f (outside a retry)"
  (check-false (current-gsd-wave-failure-context)))

;; The end-to-end orchestrator behavior (one no-change retry, then
;; permanent failure; "verifier rejected" never retries) is exercised
;; against the real run-go-campaign path in
;; test-gsd-executor-retry-characterization.rkt, which owns a live
;; fake-verifier harness. This file pins the prompt constructors and
;; the policy defaults they coordinate through.

(test-case "reanchor + failure-context compose without losing either role statement"
  (define combined (string-append reanchor "\n\n" ctx-block))
  (check-equal? (count-substring executor-reanchor-role-line combined) 1)
  (check-pred (lambda (s) (string-contains? (string-downcase s) "apply the first edit now"))
              combined))
