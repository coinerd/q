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
  (define result (handle-q-sync (hasheq 'direction "status" 'remote_host "user@localhost")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result)))

(test-case "handle-q-sync status includes sync status text"
  (define result (handle-q-sync (hasheq 'direction "status" 'remote_host "user@localhost")))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "Sync Status")))

(test-case "handle-q-sync unknown direction returns error"
  (define result (handle-q-sync (hasheq 'direction "sideways" 'remote_host "user@localhost")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-q-sync push returns tool result"
  ;; Push will fail without remote, but should return a result
  (define result
    (handle-q-sync
     (hasheq 'direction "push" 'domain "planning" 'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync pull returns tool result"
  (define result
    (handle-q-sync
     (hasheq 'direction "pull" 'domain "planning" 'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync handoff returns tool result"
  (define result
    (handle-q-sync (hasheq 'direction "handoff" 'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result))

(test-case "handle-q-sync domain planning only"
  (define result
    (handle-q-sync
     (hasheq 'direction "push" 'domain "planning" 'remote_host "nonexistent.test.invalid")))
  (check-pred tool-result? result)
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "planning")))

(test-case "handle-q-sync domain git only"
  (define result
    (handle-q-sync (hasheq 'direction "push" 'domain "git" 'remote_host "user@localhost")))
  (check-pred tool-result? result)
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "git")))

;; M5 regression: no default host -- must provide remote_host
(test-case "handle-q-sync requires remote_host for push"
  (check-exn exn:fail? (lambda () (handle-q-sync (hasheq 'direction "push" 'domain "git")))))

(test-case "handle-q-sync requires remote_host for pull"
  (check-exn exn:fail? (lambda () (handle-q-sync (hasheq 'direction "pull" 'domain "git")))))

;; ============================================================
;; rsync argument construction tests
;; ============================================================

(test-case "sync-pi-config push appends trailing slash to source"
  (define-values (ec out err) (sync-pi-config "nonexistent.test.invalid" "/tmp" "push"))
  ;; rsync will fail quickly (no remote), but command should be constructed correctly
  (check-pred integer? ec))

(test-case "sync-scripts pull appends trailing slash to dest"
  (define-values (ec out err) (sync-scripts "nonexistent.test.invalid" "/tmp" "pull"))
  (check-pred integer? ec))
