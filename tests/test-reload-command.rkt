#lang racket

;; tests/test-reload-command.rkt — Tests for /reload hot-reload command (G6.2)
;;
;; Covers:
;;   - reload-extensions! unloads and reloads all extensions
;;   - reload-extensions! continues on individual extension failure
;;   - /reload command shows success message in transcript
;;   - /reload with no registry shows appropriate message

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/runtime-path
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         "../tui/state.rkt"
         "../tui/commands.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Resolve api.rkt relative to this test file's directory
(define-runtime-path test-dir ".")
(define api-abs-path (simplify-path (build-path test-dir ".." "extensions" "api.rkt")))

;; Write an extension .rkt file into dir that provides `the-extension`
(define (write-ext-file! dir name)
  (make-directory* dir)
  (define path (build-path dir (format "~a.rkt" name)))
  (call-with-output-file
   path
   (lambda (out)
     (displayln "#lang racket/base" out)
     ;; Use (file ...) form for absolute paths in require
     (fprintf out "(require (file \"~a\"))\n" (path->string api-abs-path))
     (fprintf out "(define the-extension (extension \"~a\" \"1.0\" \"1\" (hash)))\n" name)
     (displayln "(provide the-extension)" out))
   #:exists 'replace)
  path)

;; Write a broken extension file (syntax error)
(define (write-broken-ext-file! dir name)
  (make-directory* dir)
  (define path (build-path dir (format "~a.rkt" name)))
  (call-with-output-file path
                         (lambda (out)
                           (displayln "#lang racket/base" out)
                           (displayln "(this is broken syntax (" out))
                         #:exists 'replace)
  path)

;; Create a minimal cmd-ctx for testing
(define (make-test-cctx #:ext-reg [ext-reg #f])
  (cmd-ctx (box (initial-ui-state))
           (box #t)
           #f ; event-bus
           #f ; session-dir
           (box #f) ; needs-redraw
           (box #f) ; model-registry-box
           (box #f) ; last-prompt-box
           #f ; session-runner
           (box #f) ; input-text-box
           (box ext-reg))) ; extension-registry-box

;; ============================================================
;; Test suite
;; ============================================================

(define reload-tests
  (test-suite "G6.2: /reload hot-reload command"

    ;; ----------------------------------------------------------
    ;; reload-extensions! unloads and reloads all extensions
    ;; ----------------------------------------------------------
    (test-case "reload-extensions! unloads and reloads all extensions"
      (define tmp-dir (make-temporary-file "q-reload-test-~a" 'directory))
      (define ext-dir tmp-dir) ;; discover-extension-files scans this dir
      (cleanup! tmp-dir)

      ;; Write extension file
      (write-ext-file! ext-dir "test-ext-a")

      ;; Pre-populate registry with a stale extension (simulating loaded state)
      (define reg (make-extension-registry))
      (register-extension! reg (extension "test-ext-a" "0.1" "1" (hash)))
      (check-equal? (length (list-extensions reg)) 1 "pre-populated")

      ;; Reload — should unload old version and load fresh from files
      (define loaded (reload-extensions! reg (list ext-dir)))
      (check-equal? loaded '("test-ext-a") "reload returns loaded names")
      (check-equal? (length (list-extensions reg)) 1 "still one extension after reload")
      ;; Version should now be "1.0" from the file, not "0.1"
      (check-equal? (extension-version (car (list-extensions reg)))
                    "1.0"
                    "version updated after reload")

      (cleanup! tmp-dir))

    ;; ----------------------------------------------------------
    ;; reload-extensions! continues on individual extension failure
    ;; ----------------------------------------------------------
    (test-case "reload-extensions! continues on individual extension failure"
      (define tmp-dir (make-temporary-file "q-reload-fail-~a" 'directory))
      (define ext-dir tmp-dir)
      (cleanup! tmp-dir)

      ;; Write one good and one broken extension
      (write-ext-file! ext-dir "good-ext")
      (write-broken-ext-file! ext-dir "bad-ext")

      (define reg (make-extension-registry))
      ;; Pre-load good one with stale version
      (register-extension! reg (extension "good-ext" "0.1" "1" (hash)))
      (check-equal? (length (list-extensions reg)) 1 "pre-load one ext")

      ;; Reload — should unload all, then load good-ext, skip bad-ext
      (define loaded (reload-extensions! reg (list ext-dir)))
      (check-true (and (member "good-ext" loaded) #t) "good-ext was loaded")
      (check-false (member "bad-ext" loaded) "bad-ext was not loaded")

      ;; Registry should have exactly the good one
      (define exts (list-extensions reg))
      (check-equal? (length exts) 1 "only good extension in registry")
      (check-equal? (extension-name (car exts)) "good-ext")

      (cleanup! tmp-dir))

    ;; ----------------------------------------------------------
    ;; /reload command shows success message in transcript
    ;; ----------------------------------------------------------
    (test-case "/reload command shows success message in transcript"
      (define tmp-dir (make-temporary-file "q-reload-cmd-~a" 'directory))
      (cleanup! tmp-dir)

      ;; Write extension file
      (write-ext-file! tmp-dir "demo-ext")

      (define reg (make-extension-registry))
      ;; Pre-register so we have something to reload
      (register-extension! reg (extension "demo-ext" "0.1" "1" (hash)))

      (define cctx (make-test-cctx #:ext-reg reg))

      ;; Parameterize current-directory so reload finds extensions in tmp-dir
      ;; We place extensions directly in a .q/extensions/ subdir
      (define local-ext-dir (build-path tmp-dir ".q" "extensions"))
      (write-ext-file! local-ext-dir "demo-ext")

      (parameterize ([current-directory tmp-dir])
        ;; Clear the registry and register directly so reload has something to unload
        (unregister-extension! reg "demo-ext")
        (register-extension! reg (extension "demo-ext" "0.1" "1" (hash)))

        (define result (process-slash-command cctx 'reload))
        (check-equal? result 'continue)

        ;; Check transcript for success message
        (define state (unbox (cmd-ctx-state-box cctx)))
        (define transcript (ui-state-transcript state))
        (check-true (not (null? transcript)) "transcript has entries")
        (define last-entry (car (reverse transcript)))
        (check-true (string-contains? (transcript-entry-text last-entry) "reload complete")
                    (format "transcript contains 'reload complete': ~a"
                            (transcript-entry-text last-entry))))

      (cleanup! tmp-dir))

    ;; ----------------------------------------------------------
    ;; /reload with no registry shows appropriate message
    ;; ----------------------------------------------------------
    (test-case "/reload with no registry shows appropriate message"
      (define cctx (make-test-cctx #:ext-reg #f))
      (define result (process-slash-command cctx 'reload))
      (check-equal? result 'continue)

      (define state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript state))
      (check-true (not (null? transcript)) "transcript has entries")
      (define last-entry (car (reverse transcript)))
      (check-true (string-contains? (transcript-entry-text last-entry) "no extension registry")
                  (format "transcript contains error message: ~a"
                          (transcript-entry-text last-entry))))))

;; ============================================================
;; Helpers — cleanup
;; ============================================================

(define (cleanup! dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; ============================================================
;; Run
;; ============================================================

(run-tests reload-tests)
