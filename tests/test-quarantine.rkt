#lang racket

;; tests/test-quarantine.rkt — tests for extension quarantine/disable workflow
;;
;; Covers Issue #56:
;;   - extension-state for unknown extension → 'unknown
;;   - disable-extension! records state, extension-state returns 'disabled
;;   - quarantine-extension! moves dir, state becomes 'quarantined
;;   - restore-extension! moves dir back, state becomes 'active
;;   - list-quarantined returns quarantined entries
;;   - format-extension-status produces readable output
;;   - State file persistence: write then read back
;;   - Multiple extensions: independent states

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../extensions/quarantine.rkt")

(define (make-temp-base)
  (make-temporary-file "q-quarantine-test-~a" 'directory))

(define (with-temp-quarantine-dir thunk)
  (define base (make-temp-base))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-quarantine-dir (build-path base "quarantine")])
        (thunk)))
    (lambda ()
      (when (directory-exists? base)
        (delete-directory/files base)))))

(define (make-ext-dir base name)
  (define dir (build-path base name))
  (make-directory* dir)
  (call-with-output-file (build-path dir "main.rkt")
    (lambda (p)
      (displayln "#lang racket/base" p)))
  dir)

;; ============================================================
;; Test suite
;; ============================================================

(define quarantine-tests
  (test-suite
   "extension quarantine tests"

   ;; Test 1: extension-state for unknown extension
   (test-case "unknown extension returns 'unknown"
     (with-temp-quarantine-dir
      (lambda ()
        (check-equal? (extension-state "nonexistent-ext") 'unknown))))

   ;; Test 2: disable-extension! records state
   (test-case "disable extension records disabled state"
     (with-temp-quarantine-dir
      (lambda ()
        (disable-extension! "my-ext")
        (check-equal? (extension-state "my-ext") 'disabled))))

   ;; Test 3: quarantine-extension! moves dir
   (test-case "quarantine extension moves directory and records state"
     (with-temp-quarantine-dir
      (lambda ()
        (define src-parent (build-path (current-quarantine-dir) ".."))
        (define src-dir (build-path src-parent "test-ext-src"))
        (make-directory* src-dir)
        (call-with-output-file (build-path src-dir "main.rkt")
          (lambda (p) (displayln "# test" p)))
        (quarantine-extension! "test-ext" src-dir)
        (check-equal? (extension-state "test-ext") 'quarantined)
        ;; Original dir should be gone
        (check-false (directory-exists? src-dir))
        ;; Should exist inside quarantine
        (define q-path (build-path (current-quarantine-dir) "test-ext"))
        (check-true (directory-exists? q-path)))))

   ;; Test 4: restore-extension! moves dir back
   (test-case "restore extension moves back and clears quarantined state"
     (with-temp-quarantine-dir
      (lambda ()
        ;; Setup: create source, quarantine it
        (define src-parent (build-path (current-quarantine-dir) ".."))
        (define src-dir (build-path src-parent "restore-ext-src"))
        (make-directory* src-dir)
        (call-with-output-file (build-path src-dir "main.rkt")
          (lambda (p) (displayln "# test" p)))
        (quarantine-extension! "restore-ext" src-dir)
        (check-equal? (extension-state "restore-ext") 'quarantined)
        ;; Restore
        (define restore-dir (build-path src-parent "restore-ext-restored"))
        (restore-extension! "restore-ext" restore-dir)
        (check-equal? (extension-state "restore-ext") 'active)
        ;; Should exist at restore location
        (check-true (directory-exists? restore-dir))
        ;; Should NOT exist in quarantine anymore
        (check-false (directory-exists? (build-path (current-quarantine-dir) "restore-ext"))))))

   ;; Test 5: list-quarantined returns quarantined entries
   (test-case "list-quarantined returns quarantined extensions"
     (with-temp-quarantine-dir
      (lambda ()
        (define src-parent (build-path (current-quarantine-dir) ".."))
        (define src-dir (build-path src-parent "list-ext-src"))
        (make-directory* src-dir)
        (call-with-output-file (build-path src-dir "main.rkt")
          (lambda (p) (displayln "# test" p)))
        (quarantine-extension! "list-ext" src-dir)
        (define listed (list-quarantined))
        (check-equal? (length listed) 1)
        (define entry (car listed))
        (check-equal? (hash-ref entry 'name) "list-ext")
        (check-equal? (hash-ref entry 'state) "quarantined")
        (check-true (hash-has-key? entry 'original-path))
        (check-true (hash-has-key? entry 'quarantined-at)))))

   ;; Test 6: format-extension-status produces readable output
   (test-case "format-extension-status returns readable string"
     (with-temp-quarantine-dir
      (lambda ()
        ;; Unknown extension
        (check-true (string? (format-extension-status "no-ext")))
        (check-true (string-contains? (format-extension-status "no-ext") "unknown"))
        ;; Disabled
        (disable-extension! "fmt-ext")
        (check-true (string-contains? (format-extension-status "fmt-ext") "disabled")))))

   ;; Test 7: State file persistence
   (test-case "state persists across reads"
     (with-temp-quarantine-dir
      (lambda ()
        (disable-extension! "persist-a")
        (disable-extension! "persist-b")
        ;; Re-reading should get same states
        (check-equal? (extension-state "persist-a") 'disabled)
        (check-equal? (extension-state "persist-b") 'disabled)
        ;; State file should exist
        (check-true (file-exists? (quarantine-state-file))))))

   ;; Test 8: Multiple extensions with independent states
   (test-case "multiple extensions have independent states"
     (with-temp-quarantine-dir
      (lambda ()
        (define src-parent (build-path (current-quarantine-dir) ".."))
        ;; Disable ext-a
        (disable-extension! "multi-a")
        ;; Quarantine ext-b
        (define src-b (build-path src-parent "multi-b-src"))
        (make-directory* src-b)
        (call-with-output-file (build-path src-b "main.rkt")
          (lambda (p) (displayln "# test" p)))
        (quarantine-extension! "multi-b" src-b)
        ;; ext-c stays unknown
        (check-equal? (extension-state "multi-a") 'disabled)
        (check-equal? (extension-state "multi-b") 'quarantined)
        (check-equal? (extension-state "multi-c") 'unknown)
        ;; list-quarantined only has multi-b
        (check-equal? (length (list-quarantined)) 1))))

   ;; Test 9: restoring a disabled extension clears disabled state
   (test-case "restore disabled extension clears disabled state"
     (with-temp-quarantine-dir
      (lambda ()
        (disable-extension! "reenable-ext")
        (check-equal? (extension-state "reenable-ext") 'disabled)
        ;; Restoring a disabled extension (no files to move) should mark active
        (restore-extension! "reenable-ext" "/dev/null")
        (check-equal? (extension-state "reenable-ext") 'active))))

   ))

(run-tests quarantine-tests)
