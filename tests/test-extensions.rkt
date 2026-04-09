#lang racket

;; tests/test-extensions.rkt — tests for the extension system
;;
;; Covers:
;;   1. hooks.rkt    — hook dispatch with pass/amend/block semantics
;;   2. api.rkt      — extension registry (register, unregister, lookup, handlers-for)
;;   3. define-extension.rkt — define-q-extension macro
;;   4. loader.rkt   — discover and load extension modules

(require rackunit
         racket/match
         racket/port
         racket/file
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         "../extensions/define-extension.rkt")

;; ============================================================
;; 1. hooks.rkt — hook-result struct and constructors
;; ============================================================

(test-case "hook-pass creates a pass result"
  (define r (hook-pass))
  (check-equal? (hook-result-action r) 'pass)
  (check-false (hook-result-payload r)))

(test-case "hook-pass with payload"
  (define r (hook-pass 'some-value))
  (check-equal? (hook-result-action r) 'pass)
  (check-equal? (hook-result-payload r) 'some-value))

(test-case "hook-amend creates an amend result"
  (define r (hook-amend (hasheq 'text "modified")))
  (check-equal? (hook-result-action r) 'amend)
  (check-equal? (hook-result-payload r) (hasheq 'text "modified")))

(test-case "hook-block creates a block result"
  (define r (hook-block "not allowed"))
  (check-equal? (hook-result-action r) 'block)
  (check-equal? (hook-result-payload r) "not allowed"))

(test-case "hook-block without reason"
  (define r (hook-block))
  (check-equal? (hook-result-action r) 'block)
  (check-false (hook-result-payload r)))

(test-case "hook-result is transparent"
  (define r (hook-amend 42))
  (check-equal? r (hook-result 'amend 42)))

;; ============================================================
;; 2. hooks.rkt — dispatch-hooks
;; ============================================================

(test-case "dispatch-hooks with no handlers returns pass with original payload"
  (define reg (make-extension-registry))
  (define result (dispatch-hooks 'tool-call "original-payload" reg))
  (check-equal? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "original-payload"))

(test-case "dispatch-hooks passes through when handler returns pass"
  (define reg (make-extension-registry))
  (define ext (extension "test-ext" "1.0" "1"
                          (hasheq 'tool-call (λ (payload) (hook-pass payload)))))
  (register-extension! reg ext)
  (define result (dispatch-hooks 'tool-call "original" reg))
  (check-equal? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "original"))

(test-case "dispatch-hooks amends payload for next handler"
  (define reg (make-extension-registry))
  ;; First handler amends
  (define ext1 (extension "amender" "1.0" "1"
                           (hasheq 'context (λ (payload) (hook-amend (string-append payload " +amended"))))))
  ;; Second handler passes through
  (define ext2 (extension "passer" "1.0" "1"
                           (hasheq 'context (λ (payload) (hook-pass payload)))))
  (register-extension! reg ext1)
  (register-extension! reg ext2)
  (define result (dispatch-hooks 'context "start" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "start +amended"))

(test-case "dispatch-hooks chains amendments across multiple handlers"
  (define reg (make-extension-registry))
  (define ext1 (extension "add-a" "1.0" "1"
                           (hasheq 'context (λ (p) (hook-amend (string-append p " A"))))))
  (define ext2 (extension "add-b" "1.0" "1"
                           (hasheq 'context (λ (p) (hook-amend (string-append p " B"))))))
  (register-extension! reg ext1)
  (register-extension! reg ext2)
  (define result (dispatch-hooks 'context "start" reg))
  (check-equal? (hook-result-payload result) "start A B"))

(test-case "dispatch-hooks stops on block"
  (define reg (make-extension-registry))
  (define blocker-called (box #f))
  (define after-blocker-called (box #f))
  (define ext1 (extension "blocker" "1.0" "1"
                           (hasheq 'tool-call
                                   (λ (p)
                                     (set-box! blocker-called #t)
                                     (hook-block "denied")))))
  (define ext2 (extension "after-blocker" "1.0" "1"
                           (hasheq 'tool-call
                                   (λ (p)
                                     (set-box! after-blocker-called #t)
                                     (hook-pass p)))))
  (register-extension! reg ext1)
  (register-extension! reg ext2)
  (define result (dispatch-hooks 'tool-call "payload" reg))
  (check-equal? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "denied")
  (check-true (unbox blocker-called))
  (check-false (unbox after-blocker-called) "handler after block should not be called"))

(test-case "dispatch-hooks only calls handlers registered for that point"
  (define reg (make-extension-registry))
  (define context-called (box #f))
  (define tool-called (box #f))
  (define ext (extension "mixed" "1.0" "1"
                          (hasheq 'context (λ (p) (set-box! context-called #t) (hook-pass p))
                                  'tool-call (λ (p) (set-box! tool-called #t) (hook-pass p)))))
  (register-extension! reg ext)
  (dispatch-hooks 'context "payload" reg)
  (check-true (unbox context-called))
  (check-false (unbox tool-called)))

(test-case "dispatch-hooks with multiple extensions on same point"
  (define reg (make-extension-registry))
  (define ext1 (extension "e1" "1.0" "1"
                           (hasheq 'turn-start (λ (p) (hook-amend (cons 'e1 p))))))
  (define ext2 (extension "e2" "1.0" "1"
                           (hasheq 'turn-start (λ (p) (hook-amend (cons 'e2 p))))))
  (register-extension! reg ext1)
  (register-extension! reg ext2)
  (define result (dispatch-hooks 'turn-start '() reg))
  (check-equal? (hook-result-payload result) '(e2 e1)))

;; ============================================================
;; 3. api.rkt — extension registry
;; ============================================================

(test-case "make-extension-registry creates a registry"
  (define reg (make-extension-registry))
  (check-pred extension-registry? reg))

(test-case "register-extension! and lookup-extension"
  (define reg (make-extension-registry))
  (define ext (extension "my-ext" "1.0" "1" (hasheq)))
  (register-extension! reg ext)
  (define found (lookup-extension reg "my-ext"))
  (check-not-false found)
  (check-equal? (extension-name found) "my-ext")
  (check-equal? (extension-version found) "1.0"))

(test-case "lookup-extension returns #f for unknown extension"
  (define reg (make-extension-registry))
  (check-false (lookup-extension reg "nonexistent")))

(test-case "list-extensions returns all registered"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "a" "1.0" "1" (hasheq)))
  (register-extension! reg (extension "b" "2.0" "1" (hasheq)))
  (define names (sort (map extension-name (list-extensions reg)) string<?))
  (check-equal? names '("a" "b")))

(test-case "unregister-extension! removes extension"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "removeme" "1.0" "1" (hasheq)))
  (check-not-false (lookup-extension reg "removeme"))
  (unregister-extension! reg "removeme")
  (check-false (lookup-extension reg "removeme")))

(test-case "unregister-extension! on unknown name is a no-op"
  (define reg (make-extension-registry))
  ;; Should not raise
  (unregister-extension! reg "ghost"))

(test-case "handlers-for-point returns correct handlers"
  (define reg (make-extension-registry))
  (define h1 (λ (p) (hook-pass p)))
  (define h2 (λ (p) (hook-amend p)))
  (register-extension! reg (extension "ext-a" "1.0" "1"
                                        (hasheq 'tool-call h1 'context h2)))
  (register-extension! reg (extension "ext-b" "1.0" "1"
                                        (hasheq 'tool-call h2)))
  (define handlers (handlers-for-point reg 'tool-call))
  (check-equal? (length handlers) 2)
  (define names (map car handlers))
  (check-not-false (member "ext-a" names))
  (check-not-false (member "ext-b" names))
  (define ext-a-handler (cdr (assoc "ext-a" handlers)))
  (define ext-b-handler (cdr (assoc "ext-b" handlers)))
  (check-eq? ext-a-handler h1)
  (check-eq? ext-b-handler h2))

(test-case "handlers-for-point returns empty for unregistered point"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "ext" "1.0" "1" (hasheq 'context (λ (p) (hook-pass p)))))
  (define handlers (handlers-for-point reg 'tool-call))
  (check-equal? handlers '()))

(test-case "extension struct is transparent"
  (define ext (extension "test" "1.0" "1" (hasheq 'x (λ (p) p))))
  (check-equal? (extension-name ext) "test")
  (check-equal? (extension-version ext) "1.0")
  (check-equal? (extension-api-version ext) "1"))

;; ============================================================
;; 4. define-extension.rkt — define-q-extension macro
;; ============================================================

(test-case "define-q-extension creates an extension with defaults"
  (define-q-extension simple-ext)
  (check-equal? (extension-name simple-ext) "simple-ext")
  (check-equal? (extension-version simple-ext) "0.1.0")
  (check-equal? (extension-api-version simple-ext) "1")
  (check-equal? (extension-hooks simple-ext) (hasheq)))

(test-case "define-q-extension with version and api-version"
  (define-q-extension versioned-ext
    #:version "2.3.1"
    #:api-version "2")
  (check-equal? (extension-name versioned-ext) "versioned-ext")
  (check-equal? (extension-version versioned-ext) "2.3.1")
  (check-equal? (extension-api-version versioned-ext) "2"))

(test-case "define-q-extension with hook handlers"
  (define-q-extension hooking-ext
    #:version "1.0"
    #:on tool-call (λ (p) (hook-block "blocked"))
    #:on context (λ (p) (hook-amend (string-append p " modified"))))
  (check-equal? (extension-name hooking-ext) "hooking-ext")
  (define hooks (extension-hooks hooking-ext))
  (check-equal? (hash-count hooks) 2)
  ;; Test the tool-call handler
  (define tc-result ((hash-ref hooks 'tool-call) "payload"))
  (check-equal? (hook-result-action tc-result) 'block)
  ;; Test the context handler
  (define ctx-result ((hash-ref hooks 'context) "start"))
  (check-equal? (hook-result-action ctx-result) 'amend)
  (check-equal? (hook-result-payload ctx-result) "start modified"))

(test-case "define-q-extension can be registered and dispatched"
  (define-q-extension dispatchable-ext
    #:on turn-start (λ (p) (hook-amend (cons 'dispatchable p))))
  (define reg (make-extension-registry))
  (register-extension! reg dispatchable-ext)
  (define result (dispatch-hooks 'turn-start '() reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) '(dispatchable)))

;; ============================================================
;; 5. loader.rkt — discover and load extensions
;; ============================================================

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? void])
    (delete-directory/files dir #:must-exist? #f)))

(test-case "discover-extensions returns empty for nonexistent directory"
  (check-equal? (discover-extensions "/nonexistent/path/extensions") '()))

(test-case "discover-extensions finds extension modules in directory"
  ;; Create a temp directory with extension files
  (define tmp-dir (make-temporary-file "q-ext-test-~a" 'directory))
  (define ext-dir (build-path tmp-dir "extensions"))
  (make-directory ext-dir)
  ;; Write a simple extension module using absolute paths
  (with-output-to-file (build-path ext-dir "my-ext.rkt")
    (λ ()
      (displayln "#lang racket")
      (displayln "(require (file \"/home/user/src/q-agent/q/extensions/define-extension.rkt\"))")
      (displayln "(require (file \"/home/user/src/q-agent/q/extensions/hooks.rkt\"))")
      (displayln "(provide the-extension)")
      (displayln "(define-q-extension my-ext")
      (displayln "  #:version \"1.0.0\"")
      (displayln "  #:on context (λ (p) (hook-amend (string-append p \" +my-ext\"))))")
      (displayln "(define the-extension my-ext)")))
  (define exts (discover-extensions tmp-dir))
  (check-true (>= (length exts) 1) "should find at least one extension")
  ;; Cleanup
  (cleanup-dir tmp-dir))

(test-case "load-extension! loads and registers an extension from a path"
  ;; Create a temp extension module
  (define tmp-dir (make-temporary-file "q-ext-load-~a" 'directory))
  (define ext-file (build-path tmp-dir "loadable-ext.rkt"))
  (with-output-to-file ext-file
    (λ ()
      (displayln "#lang racket")
      (displayln "(require (file \"/home/user/src/q-agent/q/extensions/define-extension.rkt\"))")
      (displayln "(require (file \"/home/user/src/q-agent/q/extensions/hooks.rkt\"))")
      (displayln "(provide the-extension)")
      (displayln "(define-q-extension loadable-ext")
      (displayln "  #:version \"0.5.0\"")
      (displayln "  #:on session-start (λ (p) (hook-amend (cons 'loaded p))))")
      (displayln "(define the-extension loadable-ext)")))
  (define reg (make-extension-registry))
  (load-extension! reg ext-file)
  (define found (lookup-extension reg "loadable-ext"))
  (check-not-false found)
  (check-equal? (extension-name found) "loadable-ext")
  (check-equal? (extension-version found) "0.5.0")
  ;; Cleanup
  (cleanup-dir tmp-dir))

(test-case "load-extension! with invalid module does not crash registry"
  (define tmp-dir (make-temporary-file "q-ext-bad-~a" 'directory))
  (define ext-file (build-path tmp-dir "bad-ext.rkt"))
  (with-output-to-file ext-file
    (λ ()
      (displayln "#lang racket")
      (displayln "(provide the-extension)")
      (displayln "(define the-extension 42)")))  ;; Not an extension struct
  (define reg (make-extension-registry))
  (register-extension! reg (extension "safe" "1.0" "1" (hasheq)))
  ;; Should not raise, but the bad extension should not be registered
  (with-check-info (['msg "loading invalid extension should not crash"])
    (with-handlers ([exn:fail? (λ (e) (void))])
      (load-extension! reg ext-file)))
  ;; Safe extension should still be there
  (check-not-false (lookup-extension reg "safe"))
  (cleanup-dir tmp-dir))

;; ============================================================
;; 6. Integration: full lifecycle with registry + dispatch
;; ============================================================

(test-case "full lifecycle: register, dispatch, unregister"
  (define reg (make-extension-registry))
  (define-q-extension lifecycle-ext
    #:version "1.0"
    #:on tool-call (λ (p) (hook-amend (string-append p " [logged]"))))

  (register-extension! reg lifecycle-ext)
  (define r1 (dispatch-hooks 'tool-call "read file.txt" reg))
  (check-equal? (hook-result-payload r1) "read file.txt [logged]")

  (unregister-extension! reg "lifecycle-ext")
  (define r2 (dispatch-hooks 'tool-call "read file.txt" reg))
  (check-equal? (hook-result-action r2) 'pass)
  (check-equal? (hook-result-payload r2) "read file.txt"))

(test-case "multiple extensions: one amends, one blocks"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "logger" "1.0" "1"
                                           (hasheq 'tool-call (λ (p) (hook-amend (string-append p " [logged]"))))))
  (register-extension! reg (extension "blocker" "1.0" "1"
                                           (hasheq 'tool-call (λ (p) (hook-block "denied")))))
  ;; Logger is first, so it amends; then blocker blocks
  (define result (dispatch-hooks 'tool-call "read file.txt" reg))
  (check-equal? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "denied"))

(test-case "all core hook points are valid symbols"
  (for ([point '(resources-discover session-start session-before-switch
                   session-before-compact before-agent-start context
                   before-provider-request tool-call tool-result
                   message-start message-update message-end
                   turn-start turn-end model-select session-shutdown)])
    (check-true (symbol? point))))