#lang racket/base

;; tests/test-q-sync-extension.rkt — Tests for q-sync extension

(require rackunit
         racket/file
         racket/string
         "../extensions/q-sync.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Handler tests
;; ============================================================

(test-case "handle-q-sync status returns success"
  (define result (handle-q-sync (hasheq 'direction "status")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result)))

(test-case "handle-q-sync status includes sync status text"
  (define result (handle-q-sync (hasheq 'direction "status")))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "Sync Status")))

(test-case "handle-q-sync unknown direction returns error"
  (define result
    (handle-q-sync (hasheq 'direction "sideways")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-q-sync push returns tool result"
  ;; Push will fail without remote, but should return a result
  (define result
    (handle-q-sync (hasheq 'direction "push"
                           'domain "planning"
                           'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync pull returns tool result"
  (define result
    (handle-q-sync (hasheq 'direction "pull"
                           'domain "planning"
                           'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync handoff returns tool result"
  (define result
    (handle-q-sync (hasheq 'direction "handoff"
                           'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync domain planning only"
  (define result
    (handle-q-sync (hasheq 'direction "push"
                           'domain "planning"
                           'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result)
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "planning")))

(test-case "handle-q-sync domain git only"
  (define result
    (handle-q-sync (hasheq 'direction "push"
                           'domain "git")))
  (check-pred tool-result? result)
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "git")))
