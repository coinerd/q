#lang racket/base

;; scripts/benchmark/task-state-aware.rkt — Prompt injection effectiveness benchmark
;; v0.76.1 W1: Verify state-aware instructions are present and effective.

(require racket/format
         racket/string
         "../../runtime/context-assembly/state-aware-builder.rkt"
         "../../runtime/context-assembly/task-conclusion.rkt"
         "../../runtime/context-assembly/context-floor.rkt"
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/message/message.rkt" message-content message-role make-message)
         (only-in "../../util/content/content-parts.rkt" text-part-text))

;; ── Helpers ──

(define (make-test-msg text)
  (make-message "id" #f 'user 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-conclusion text)
  (task-conclusion (format "c~a" (current-inexact-milliseconds))
                   text
                   'fact
                   'exploration
                   '()
                   (current-seconds)
                   '()
                   '()))

(define (preamble-text task-state conclusions)
  (define p (build-state-awareness-preamble task-state conclusions))
  (if p
      (text-part-text (car (message-content p)))
      ""))

(define (instruction-present? text)
  (string-contains? text "record_conclusion"))

(define (token-estimate text)
  ;; Rough token estimate: ~4 chars per token
  (ceiling (/ (string-length text) 4.0)))

;; ── Benchmark ──

(define states '(exploration planning implementation verification debugging))

(printf "═══════════════════════════════════════════════════════════\n")
(printf "   Prompt Injection Effectiveness Benchmark v0.76.1\n")
(printf "═══════════════════════════════════════════════════════════\n\n")

;; 1. Instruction presence per state
(printf "1. Instruction presence per state (no conclusions):\n")
(for ([s (in-list states)])
  (define text (preamble-text s '()))
  (define present? (instruction-present? text))
  (define tokens (token-estimate text))
  (printf "   ~a: ~a (~a tokens)\n"
          (~a s #:min-width 14)
          (if present? "✓ present" "✗ MISSING")
          (~r tokens #:precision 0)))

;; 2. Conclusion count accuracy
(printf "\n2. Conclusion count accuracy:\n")
(for ([n (in-list '(0 1 3 5 10))])
  (define conclusions
    (for/list ([i (in-range n)])
      (make-conclusion (format "C~a" i))))
  (define text (preamble-text 'implementation conclusions))
  (define accurate?
    (if (zero? n)
        (string-contains? text "No conclusions in memory yet")
        (string-contains? text (format "~a in memory" n))))
  (printf "   ~a conclusions: ~a\n" n (if accurate? "✓ accurate" "✗ inaccurate")))

;; 3. Token overhead measurement
(printf "\n3. Preamble token overhead per state:\n")
(for ([s (in-list states)])
  (define text-no-conclusions (preamble-text s '()))
  (define text-with-conclusions (preamble-text s (list (make-conclusion "Sample finding"))))
  (printf "   ~a: ~a tokens (no conclusions), ~a tokens (1 conclusion)\n"
          (~a s #:min-width 14)
          (~r (token-estimate text-no-conclusions) #:precision 0)
          (~r (token-estimate text-with-conclusions) #:precision 0)))

;; 4. Full assembly integration check
(printf "\n4. Full assembly integration (tier-a preamble presence):\n")
(for ([s (in-list states)])
  (define msgs (list (make-test-msg "hello")))
  (define tc (build-tiered-context/state-aware msgs #:task-state s))
  (define tier-a (tiered-context-tier-a tc))
  (define has-preamble?
    (and (> (length tier-a) 0) (eq? 'system-instruction (message-role (car tier-a)))))
  (printf "   ~a: ~a (~a messages in tier-a)\n"
          (~a s #:min-width 14)
          (if has-preamble? "✓ preamble" "✗ no preamble")
          (length tier-a)))

;; 5. Pass/Fail summary
(printf "\n═══════════════════════════════════════════════════════════\n")
(printf "   SUMMARY\n")
(printf "═══════════════════════════════════════════════════════════\n")
(define all-pass?
  (for/and ([s (in-list states)])
    (instruction-present? (preamble-text s '()))))
(printf "All states include record_conclusion instruction: ~a\n" (if all-pass? "PASS ✓" "FAIL ✗"))
(printf "Benchmark complete.\n")
