#lang racket/base

;; runtime/context-assembly/blackboard-context.rkt — Blackboard context snippet
;; STABILITY: evolving
;;
;; W5 (v0.99.7): Build a compact system-prompt snippet from blackboard state.
;; Injected into the state-awareness preamble when enabled.
;;
;; Design:
;;   - build-blackboard-context-snippet: returns #f if empty, else formatted text
;;   - Individual formatters for each blackboard section
;;   - Compact output suitable for LLM system prompt
;;
;; Feature-gated via current-blackboard-injection-enabled (default #f).
;;
;; Part of MAS Schritt 4: Blackboard & Event Log (milestone #793).

(require racket/string
         racket/list
         (only-in racket/function identity)
         (only-in "../../agent/blackboard.rkt"
                  blackboard-state
                  blackboard-state?
                  blackboard-state-active-plan
                  blackboard-state-open-hypotheses
                  blackboard-state-test-results
                  blackboard-state-artifact-refs
                  blackboard-state-wave-status
                  blackboard-state-verifier-decisions
                  blackboard-state-agent-activities
                  blackboard-state-last-updated
                  empty-blackboard
                  read-blackboard
                  current-blackboard))

;; ============================================================
;; Individual Section Formatters
;; ============================================================

;; Format active plan summary.
(define (format-active-plan state)
  (define plan (blackboard-state-active-plan state))
  (and (hash? plan)
       (let ([wc (hash-ref plan 'wave-count #f)]
             [summary (hash-ref plan 'summary #f)])
         (cond
           [(and wc summary) (format "Plan: ~a waves — ~a" wc summary)]
           [wc (format "Plan: ~a waves" wc)]
           [summary (format "Plan: ~a" summary)]
           [else (format "Plan: ~a" plan)]))))

;; Format wave status as wave=status pairs.
(define (format-wave-status state)
  (define ws (blackboard-state-wave-status state))
  (and (hash? ws)
       (> (hash-count ws) 0)
       (format "Waves: ~a"
               (string-join (for/list ([(wave status) (in-hash ws)])
                              (format "~a=~a" wave status))
                            ", "))))

;; Format test results (max 5 shown).
(define (format-test-results state)
  (define results (blackboard-state-test-results state))
  (and (pair? results)
       (let* ([shown (take results (min 5 (length results)))]
              [parts (for/list ([r (in-list shown)])
                       (format "~a=~a" (hash-ref r 'file "unknown") (hash-ref r 'result 'unknown)))])
         (format "Tests (~a): ~a" (length results) (string-join parts ", ")))))

;; Format artifact references (max 5 shown).
(define (format-artifacts state)
  (define artifacts (blackboard-state-artifact-refs state))
  (and (pair? artifacts)
       (let* ([shown (take artifacts (min 5 (length artifacts)))]
              [parts (for/list ([a (in-list shown)])
                       (format "~a (~a)"
                               (hash-ref a 'name "unknown")
                               (hash-ref a 'artifact-type 'unknown)))])
         (format "Artifacts (~a): ~a" (length artifacts) (string-join parts ", ")))))

;; Format open hypotheses (max 3 shown).
(define (format-hypotheses state)
  (define hyps (blackboard-state-open-hypotheses state))
  (and (pair? hyps)
       (let* ([shown (take hyps (min 3 (length hyps)))]
              [parts (for/list ([h (in-list shown)])
                       (format "~a: \"~a\"" (hash-ref h 'id "?") (hash-ref h 'question "unknown")))])
         (format "Open hypotheses (~a): ~a" (length hyps) (string-join parts "; ")))))

;; Format verifier decisions (max 3 shown).
(define (format-verifier-decisions state)
  (define decisions (blackboard-state-verifier-decisions state))
  (and (pair? decisions)
       (let* ([shown (take decisions (min 3 (length decisions)))]
              [parts (for/list ([d (in-list shown)])
                       (format "~a (~a)"
                               (hash-ref d 'verdict 'unknown)
                               (hash-ref d 'risk-level 'unknown)))])
         (format "Verifier: ~a" (string-join parts ", ")))))

;; ============================================================
;; Token Budget Guard (v0.99.14 W3)
;; ============================================================

;; Maximum length of the blackboard context snippet in characters.
;; Prevents context-injection bloat in the system prompt.
;; If the snippet exceeds this limit, it is truncated and suffixed with "...".
(define MAX-BLACKBOARD-SNIPPET-LEN 500)

;; Truncate a snippet to MAX-BLACKBOARD-SNIPPET-LEN chars.
;; If already within budget, returns as-is.
;; If over budget, returns first (MAX-BLACKBOARD-SNIPPET-LEN - 3) chars + "...".
(define (truncate-snippet text)
  (if (and (string? text) (> (string-length text) MAX-BLACKBOARD-SNIPPET-LEN))
      (string-append (substring text 0 (- MAX-BLACKBOARD-SNIPPET-LEN 3)) "...")
      text))

;; ============================================================
;; Combined Snippet Builder
;; ============================================================

;; Build a compact blackboard context snippet for system prompt injection.
;; Returns #f if the blackboard is empty or no sections have content.
;; The result is capped at MAX-BLACKBOARD-SNIPPET-LEN chars (500) to prevent
;; context-injection bloat (v0.99.14 W3 token budget guard).
;; Optional state argument; defaults to current-blackboard.
(define (build-blackboard-context-snippet [state #f])
  (define bb-state
    (cond
      [(blackboard-state? state) state]
      [(and (not state) (current-blackboard)) (or (read-blackboard) empty-blackboard)]
      [else empty-blackboard]))
  (define sections
    (filter identity
            (list (format-active-plan bb-state)
                  (format-wave-status bb-state)
                  (format-test-results bb-state)
                  (format-artifacts bb-state)
                  (format-hypotheses bb-state)
                  (format-verifier-decisions bb-state))))
  (if (null? sections)
      #f
      (truncate-snippet (string-append "[Blackboard]\n" (string-join sections "\n")))))

;; ============================================================
;; Provides
;; ============================================================

(provide build-blackboard-context-snippet
         MAX-BLACKBOARD-SNIPPET-LEN
         truncate-snippet
         format-active-plan
         format-wave-status
         format-test-results
         format-artifacts
         format-hypotheses
         format-verifier-decisions)
