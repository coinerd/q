#lang racket

;; tests/test-extension-lifecycle.rkt — tests for extension lifecycle features
;; #1146: Hot reload
;; #1147: Custom entry types
;; #1148: Resource discovery hook

(require rackunit
         "../extensions/loader.rkt"
         "../extensions/api.rkt"
         "../runtime/session-store.rkt"
         "../extensions/resource-discovery.rkt"
         "../util/hook-types.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; #1146: Hot reload
;; ============================================================

(test-case "reload-extensions! returns empty for empty paths"
  (define reg (make-extension-registry))
  (define loaded (reload-extensions! reg '()))
  (check-equal? loaded '()))

(test-case "discover-extension-files returns empty for empty list"
  (check-equal? (discover-extension-files '()) '()))

(test-case "discover-extension-files returns empty for non-existent directory"
  (check-equal? (discover-extension-files '("/tmp/no-such-dir-q-test-1146")) '()))

(test-case "reload-extensions! with non-existent dir returns empty"
  (define reg (make-extension-registry))
  (define loaded (reload-extensions! reg '("/tmp/no-such-dir-q-test-1146")))
  (check-equal? loaded '()))

(test-case "reload-extensions! unregisters removed extensions"
  (define reg (make-extension-registry))
  ;; Manually register an extension
  (define ext (extension "ghost-ext" "1.0" "1" (hasheq)))
  (register-extension! reg ext)
  (check-equal? (length (list-extensions reg)) 1)
  ;; Reload with empty paths — should unregister the ghost
  (reload-extensions! reg '())
  (check-equal? (length (list-extensions reg)) 0))

;; ============================================================
;; #1147: Custom entry types
;; ============================================================

(test-case "make-custom-entry creates message with correct fields"
  (define e (make-custom-entry "my-ext" "state-key" '(a b c)))
  (check-true (message? e))
  (check-equal? (message-kind e) 'custom-message)
  (check-equal? (hash-ref (message-meta e) 'extension) "my-ext")
  (check-equal? (hash-ref (message-meta e) 'key) "state-key")
  (check-equal? (hash-ref (message-meta e) 'data) '(a b c)))

(test-case "custom-entry? predicate works"
  (define e (make-custom-entry "ext" "k" "data"))
  (check-true (custom-entry? e))
  ;; A regular message should not be a custom entry
  (define m (make-message "id1" #f 'user 'message '() (current-seconds) (hasheq)))
  (check-false (custom-entry? m)))

(test-case "custom entry accessors"
  (define e (make-custom-entry "my-ext" "config" (hasheq 'theme "dark")))
  (check-equal? (custom-entry-extension e) "my-ext")
  (check-equal? (custom-entry-key e) "config")
  (check-equal? (hash-ref (custom-entry-data e) 'theme) "dark"))

(test-case "custom entry round-trip via in-memory session store"
  (define mgr (make-in-memory-session-manager))
  (define sid "test-custom-entries")
  ;; Append entries for two extensions
  (append-custom-entry! mgr sid "ext-a" "config" (hasheq 'theme "dark"))
  (append-custom-entry! mgr sid "ext-b" "state" (hasheq 'pos 42))
  (append-custom-entry! mgr sid "ext-a" "config2" (hasheq 'lang "en"))
  ;; Load for ext-a
  (define entries (load-custom-entries mgr sid "ext-a"))
  (check-equal? (length entries) 2)
  ;; Load for ext-a with key filter
  (define filtered (load-custom-entries mgr sid "ext-a" "config"))
  (check-equal? (length filtered) 1)
  (check-equal? (hash-ref (custom-entry-data (car filtered)) 'theme) "dark")
  ;; Load for ext-b
  (define ext-b-entries (load-custom-entries mgr sid "ext-b"))
  (check-equal? (length ext-b-entries) 1))

(test-case "load-custom-entries returns empty for unknown extension"
  (define mgr (make-in-memory-session-manager))
  (define sid "test-empty")
  (append-custom-entry! mgr sid "ext-a" "k" "v")
  (define entries (load-custom-entries mgr sid "ext-b"))
  (check-equal? entries '()))

;; ============================================================
;; #1148: Resource discovery hook
;; ============================================================

(test-case "resources-discover hook point exists"
  (check-not-false (member 'resources-discover (hook-point-names))
                   "'resources-discover should be in hook-point-names"))

(test-case "discover-extension-resources returns empty list without extensions"
  (define reg (make-extension-registry))
  (define paths (discover-extension-resources reg))
  (check-true (list? paths))
  (check-equal? paths '()))

(test-case "discover-extension-resources filters non-existent paths"
  (define reg (make-extension-registry))
  ;; Register an extension that returns a non-existent path
  (define ext
    (extension "path-ext"
               "1.0"
               "1"
               (hasheq 'resources-discover
                       (lambda (payload) (hook-amend (list "/tmp/no-such-resource-path-1148"))))))
  (register-extension! reg ext)
  (define paths (discover-extension-resources reg))
  ;; Should be filtered out since path doesn't exist
  (check-equal? paths '()))

(test-case "discover-extension-resources returns valid directory paths"
  (define reg (make-extension-registry))
  (define tmp-dir (make-temporary-file "q-resource-test-~a" 'directory))
  ;; Register an extension that returns a real path
  (define ext
    (extension "real-path-ext"
               "1.0"
               "1"
               (hasheq 'resources-discover (lambda (payload) (hook-amend (list tmp-dir))))))
  (register-extension! reg ext)
  (define paths (discover-extension-resources reg))
  (check-equal? paths (list tmp-dir))
  ;; Cleanup
  (delete-directory tmp-dir))
