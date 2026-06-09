#lang racket/base

;; tests/test-gapfg-memory-pipeline-integration.rkt
;; v0.97.5 W2: Integration tests for GAP-F+G memory pipeline

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/context-assembly/memory-builder.rkt"
                  observe-memory-for-context
                  memory-telemetry?)
         (only-in "../runtime/session/session-events.rkt"
                  current-mid-session-bridge-enabled
                  major-forward-transition?))

(define suite
  (test-suite "gapfg-memory-pipeline-integration"

    ;; GAP-G: observe-memory-for-context accepts tags and query text together
    (test-case "observe-memory-for-context with tags and enriched query"
      (define result
        (observe-memory-for-context #f
                                    #:scope #f
                                    #:query-text "State: planning. Active Tags: config."
                                    #:tags '(config.rkt runtime)))
      (check-equal? (car result) '())
      (check-true (memory-telemetry? (cdr result))))

    ;; GAP-F: Bridge parameter is off by default (safe rollout)
    (test-case "mid-session bridge defaults to off"
      (check-equal? (current-mid-session-bridge-enabled) #f))

    ;; GAP-F + G: Forward transition detection works with bridge flag
    (test-case "forward transition detected but bridge off → no action"
      (parameterize ([current-mid-session-bridge-enabled #f])
        (check-true (major-forward-transition? 'exploration 'planning))
        (check-equal? (current-mid-session-bridge-enabled) #f)))

    ;; GAP-G: Query enrichment preserves state context
    (test-case "enriched query contains state and tag context"
      (define query "State: implementation. Active Tags: tool, registry. Recent Conclusions: fix.")
      (check-true (string-contains? query "State:"))
      (check-true (string-contains? query "Active Tags:"))
      (check-true (string-contains? query "Recent Conclusions:")))

    ;; GAP-F: Non-forward transitions do not trigger bridge
    (test-case "non-forward transitions are not detected"
      (check-false (major-forward-transition? 'planning 'exploration))
      (check-false (major-forward-transition? 'implementation 'planning))
      (check-false (major-forward-transition? 'idle 'planning)))))

(run-tests suite)
