#lang racket

(require rackunit
         racket/file
         racket/runtime-path
         "../extensions/loader.rkt"
         "../extensions/api.rkt"
         "../extensions/quarantine.rkt")

;; ============================================================
;; Helper: create a temp extension module that provides the-extension
;; ============================================================

(define-runtime-path test-dir ".")

(define api-abs-path
  (path->string (path->complete-path (build-path test-dir ".." "extensions" "api.rkt"))))

(define (make-temp-ext-module! dir name)
  (define mod-file (build-path dir (format "~a.rkt" name)))
  (call-with-output-file
   mod-file
   (λ (out)
     (displayln "#lang racket/base" out)
     (displayln "(provide the-extension)" out)
     (displayln (format "(require (file \"~a\"))" api-abs-path) out)
     (displayln (format "(define the-extension (extension \"~a\" \"1.0\" \"1\" (hasheq)))" name) out))
   #:exists 'replace)
  mod-file)

(define (with-temp-env thunk)
  (define tmpdir (make-temporary-file "q-test-ext-~a" 'directory))
  (define qdir (build-path tmpdir "quarantine"))
  (dynamic-wind void
                (λ ()
                  (parameterize ([current-quarantine-dir qdir])
                    (thunk tmpdir)))
                (λ ()
                  (when (directory-exists? tmpdir)
                    (delete-directory/files tmpdir)))))

;; ============================================================
;; discover-extensions — no extensions dir
;; ============================================================

(test-case "discover-extensions returns empty for missing dir"
  (define tmpdir (make-temporary-file "q-test-ext-~a" 'directory))
  (define result (discover-extensions tmpdir))
  (check-equal? result '())
  (delete-directory tmpdir))

(test-case "discover-extensions returns empty for empty extensions dir"
  (define tmpdir (make-temporary-file "q-test-ext-~a" 'directory))
  (define ext-dir (build-path tmpdir "extensions"))
  (make-directory ext-dir)
  (define result (discover-extensions tmpdir))
  (check-equal? result '())
  (delete-directory ext-dir)
  (delete-directory tmpdir))

;; ============================================================
;; load-extension! — invalid module
;; ============================================================

(test-case "load-extension! skips modules without the-extension"
  (with-temp-env
   (λ (tmpdir)
     (define ext-dir (build-path tmpdir "extensions"))
     (make-directory ext-dir)
     ;; Write a module that does NOT provide the-extension
     (define mod-file (build-path ext-dir "bad.rkt"))
     (call-with-output-file mod-file (λ (out) (display "#lang racket/base\n" out)) #:exists 'replace)
     (define reg (make-extension-registry))
     (load-extension! reg mod-file)
     (check-equal? (list-extensions reg) '()))))

(test-case "load-extension! loads valid extension module when state is unknown"
  (with-temp-env (λ (tmpdir)
                   (define reg (make-extension-registry))
                   (define mod-file (make-temp-ext-module! tmpdir "test-good"))
                   ;; State is 'unknown by default — should load
                   (load-extension! reg mod-file)
                   (define exts (list-extensions reg))
                   (check-equal? (length exts) 1)
                   (check-equal? (extension-name (car exts)) "test-good"))))

(test-case "load-extension! loads valid extension module when state is active"
  (with-temp-env (λ (tmpdir)
                   (define reg (make-extension-registry))
                   (define mod-file (make-temp-ext-module! tmpdir "test-active"))
                   ;; Mark as active explicitly — should load
                   (restore-extension! "test-active" (build-path tmpdir "dummy"))
                   (check-equal? (extension-state "test-active") 'active)
                   (load-extension! reg mod-file)
                   (define exts (list-extensions reg))
                   (check-equal? (length exts) 1)
                   (check-equal? (extension-name (car exts)) "test-active"))))

(test-case "load-extension! skips loading when extension-state is disabled"
  (with-temp-env (λ (tmpdir)
                   (define reg (make-extension-registry))
                   (define mod-file (make-temp-ext-module! tmpdir "test-disabled"))
                   ;; Disable it
                   (disable-extension! "test-disabled")
                   (check-equal? (extension-state "test-disabled") 'disabled)
                   (load-extension! reg mod-file)
                   (check-equal? (list-extensions reg) '()))))

(test-case "load-extension! skips loading when extension-state is quarantined"
  (with-temp-env (λ (tmpdir)
                   (define reg (make-extension-registry))
                   (define mod-file (make-temp-ext-module! tmpdir "test-quarantined"))
                   ;; Create a source dir to quarantine
                   (define src-dir (build-path tmpdir "src-quarantined"))
                   (make-directory* src-dir)
                   (call-with-output-file (build-path src-dir "main.rkt")
                                          (λ (p) (displayln "# placeholder" p)))
                   (quarantine-extension! "test-quarantined" src-dir)
                   (check-equal? (extension-state "test-quarantined") 'quarantined)
                   (load-extension! reg mod-file)
                   (check-equal? (list-extensions reg) '()))))

;; ============================================================
;; Subdirectory extension discovery tests (Wave B3)
;; ============================================================

;; Helper: create a subdirectory extension
(define (make-subdir-ext-module! base-dir ext-name)
  (define ext-dir (build-path base-dir "extensions" ext-name))
  (make-directory* ext-dir)
  (define mod-file (build-path ext-dir (format "~a.rkt" ext-name)))
  (call-with-output-file
   mod-file
   (λ (out)
     (displayln "#lang racket/base" out)
     (displayln "(provide the-extension)" out)
     (displayln (format "(require (file \"~a\"))" api-abs-path) out)
     (displayln (format "(define the-extension (extension \"~a\" \"1.0\" \"1\" (hasheq)))" ext-name)
                out))
   #:exists 'replace)
  mod-file)

(test-case "discover-extensions finds subdirectory extension by name.rkt"
  (define tmpdir (make-temporary-file "q-test-subdir-~a" 'directory))
  (make-directory* (build-path tmpdir "extensions"))
  (make-subdir-ext-module! tmpdir "subdir-ext-a")
  (define exts (discover-extensions tmpdir))
  (check-equal? (length exts) 1)
  (check-equal? (extension-name (car exts)) "subdir-ext-a")
  (delete-directory/files tmpdir))

(test-case "discover-extensions finds both flat and subdirectory extensions"
  (define tmpdir (make-temporary-file "q-test-subdir-~a" 'directory))
  (make-directory* (build-path tmpdir "extensions"))
  ;; flat extension
  (make-temp-ext-module! (build-path tmpdir "extensions") "flat-ext")
  ;; subdir extension
  (make-subdir-ext-module! tmpdir "subdir-ext-c")
  (define exts (discover-extensions tmpdir))
  (define names (sort (map extension-name exts) string<?))
  (check-equal? names '("flat-ext" "subdir-ext-c"))
  (delete-directory/files tmpdir))

(test-case "discover-extensions skips subdir without entry point"
  (define tmpdir (make-temporary-file "q-test-subdir-~a" 'directory))
  (define ext-dir (build-path tmpdir "extensions" "no-entry"))
  (make-directory* ext-dir)
  ;; Just a helper file, no <name>.rkt or main.rkt
  (call-with-output-file (build-path ext-dir "helper.rkt")
                         (λ (out) (displayln "#lang racket/base" out)))
  (define exts (discover-extensions tmpdir))
  (check-equal? exts '())
  (delete-directory/files tmpdir))

;; ============================================================
;; Hook dispatch regression tests (Wave 1 — C1 fix)
;; Verifies ext-register-tool! arity is correct (5 args, not 2).
;; If extensions pass (make-tool ...) instead of (name desc schema handler),
;; this test catches the arity mismatch.
;; ============================================================

(require "../extensions/dynamic-tools.rkt"
         "../extensions/context.rkt"
         "../extensions/api.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt")

(define (make-test-ctx)
  (define reg (make-tool-registry))
  (define ext-reg (make-extension-registry))
  (define bus (make-event-bus))
  (make-extension-ctx #:session-id "test-session"
                      #:session-dir (find-system-path 'temp-dir)
                      #:event-bus bus
                      #:extension-registry ext-reg
                      #:tool-registry reg))

(test-case "ext-register-tool! accepts 5 positional args (ctx name desc schema handler)"
  (define ctx (make-test-ctx))
  (define reg (ctx-tool-registry ctx))
  (ext-register-tool! ctx
                      "test-tool"
                      "A test tool for arity regression"
                      (hasheq 'type "object" 'required '() 'properties (hasheq))
                      (λ (args) (make-tool-result '() (hasheq) #f)))
  (check-not-exn (λ () (lookup-tool reg "test-tool"))))

(test-case "ext-register-tool! rejects make-tool wrapper (2-arg form)"
  ;; This documents the bug pattern: passing (make-tool ...) as 2nd arg
  ;; should fail because ext-register-tool! expects a string as 2nd arg.
  (define ctx (make-test-ctx))
  (check-exn exn:fail?
             (λ ()
               (ext-register-tool! ctx
                                   (make-tool "bad"
                                              "desc"
                                              (hasheq 'type "object")
                                              (λ (args) (make-tool-result '() (hasheq) #f)))))))
