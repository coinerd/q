#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-package-host-service-isolation.rkt — v0.99.88 W3 dual-run
;; @boundary integration
;; characterization for the neutral package-host-service (MA-04).
;;
;; Proves behavioral equivalence between the OLD direct path
;; (extensions/ext-package-manager.rkt → runtime/package.rkt) and the NEW
;; adapter path (extensions → neutral package-host-service ←
;; runtime/extension-host-adapter.rkt). E1–E7, mirroring the W2 D1–D7 pattern.

(require rackunit
         racket/file
         racket/string
         json
         "../extensions/context.rkt"
         "../extensions/ext-package-manager.rkt"
         "../extensions/manifest.rkt"
         "../runtime/package.rkt"
         "../runtime/extension-host-adapter.rkt"
         "../util/extension/host-services.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Fixtures (mirror tests/test-package.rkt)
;; ============================================================

(define (with-test-packages-dir thunk)
  (define tmpdir (make-temporary-file "q-pkg-test-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-packages-dir tmpdir])
                    (thunk)))
                (lambda ()
                  (when (directory-exists? tmpdir)
                    (delete-directory/files tmpdir)))))

(define (make-test-source-package #:name [name "q-test-pkg"])
  (define srcdir (make-temporary-file "q-pkg-src-~a" 'directory))
  (call-with-output-file (build-path srcdir "main.rkt")
                         (lambda (out) (display "#lang racket/base\n(define x 1)\n" out))
                         #:exists 'replace)
  (define manifest
    (make-qpm-manifest #:name name
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Test package"
                       #:author "tester"
                       #:files '("main.rkt")
                       #:entry "main.rkt"))
  (write-qpm-manifest manifest (build-path srcdir "qpm.json"))
  srcdir)

;; The direct-path summary conversion (what the adapter must replicate).
(define (direct-summary pkg)
  (define m (qpm-package-manifest pkg))
  (cons (qpm-manifest-name m) (qpm-manifest-version m)))

;; ============================================================
;; E1 — neutral service shape
;; ============================================================

(define e1-suite
  (test-suite "v0.99.88-w3-e1-neutral-shape"
    (test-case "E1: make-package-host-service yields a neutral service with procedure ops"
      (define svc (make-package-host-service))
      (check-true (package-host-service? svc))
      (check-true (procedure? (package-host-service-package-list svc)))
      (check-true (procedure? (package-host-service-package-installed? svc)))
      (check-true (procedure? (package-host-service-package-install svc)))
      (check-true (procedure? (package-host-service-package-remove svc))))
    (test-case "E1b: package-summary is pure data with name/version"
      (define s (package-summary "pkg" "1.2.3"))
      (check-true (package-summary? s))
      (check-equal? (package-summary-name s) "pkg")
      (check-equal? (package-summary-version s) "1.2.3"))))

;; ============================================================
;; E2–E5 — dual-run parity: direct runtime path vs adapter service
;; ============================================================

(define e2-suite
  (test-suite "v0.99.88-w3-e2-parity"
    (test-case "E2: list parity — direct list-packages vs service list"
      (define srcdir (make-test-source-package))
      (dynamic-wind
       void
       (lambda ()
         (with-test-packages-dir
          (lambda ()
            (define svc (make-package-host-service))
            ;; fresh dir: both empty
            (check-equal? ((package-host-service-package-list svc)) '())
            (check-equal? (list-packages) '())
            ;; install directly, then compare both views
            (install-package-from-dir srcdir)
            (define direct (map direct-summary (list-packages)))
            (define svc-view
              (map (lambda (s) (cons (package-summary-name s) (package-summary-version s)))
                   ((package-host-service-package-list svc))))
            (check-equal? svc-view
                          direct
                          "adapter list must match direct list-packages (sorted, summarized)"))))
       (lambda ()
         (when (directory-exists? srcdir)
           (delete-directory/files srcdir)))))

    (test-case "E3: installed? parity"
      (define srcdir (make-test-source-package))
      (dynamic-wind void
                    (lambda ()
                      (with-test-packages-dir
                       (lambda ()
                         (define svc (make-package-host-service))
                         (check-false (package-installed? "q-test-pkg"))
                         (check-false ((package-host-service-package-installed? svc) "q-test-pkg"))
                         (install-package-from-dir srcdir)
                         (check-true (package-installed? "q-test-pkg"))
                         (check-true ((package-host-service-package-installed? svc) "q-test-pkg")))))
                    (lambda ()
                      (when (directory-exists? srcdir)
                        (delete-directory/files srcdir)))))

    (test-case "E4: install parity — success summary and error string identical"
      (define srcdir (make-test-source-package))
      (define bad-srcdir (make-temporary-file "q-pkg-bad-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (with-test-packages-dir
          (lambda ()
            (define svc (make-package-host-service))
            ;; success: direct returns qpm-package, service returns summary
            (define direct (install-package-from-dir srcdir))
            (check-true (qpm-package? direct))
            (define svc-result ((package-host-service-package-install svc) srcdir))
            (check-true (package-summary? svc-result))
            (check-equal? (package-summary-name svc-result)
                          (qpm-manifest-name (qpm-package-manifest direct)))
            (check-equal? (package-summary-version svc-result)
                          (qpm-manifest-version (qpm-package-manifest direct)))
            ;; error: missing qpm.json → identical error string
            (define direct-err (install-package-from-dir bad-srcdir))
            (define svc-err ((package-host-service-package-install svc) bad-srcdir))
            (check-pred string? direct-err)
            (check-equal? svc-err
                          direct-err
                          "adapter must pass the runtime error string through verbatim"))))
       (lambda ()
         (when (directory-exists? srcdir)
           (delete-directory/files srcdir))
         (when (directory-exists? bad-srcdir)
           (delete-directory/files bad-srcdir)))))

    (test-case "E5: remove parity"
      (define srcdir (make-test-source-package))
      (dynamic-wind void
                    (lambda ()
                      (with-test-packages-dir
                       (lambda ()
                         (define svc (make-package-host-service))
                         ;; both fail on unknown package
                         (check-false (remove-package "ghost"))
                         (check-false ((package-host-service-package-remove svc) "ghost"))
                         ;; both succeed after install
                         (install-package-from-dir srcdir)
                         (check-true (remove-package "q-test-pkg"))
                         (install-package-from-dir srcdir)
                         (check-true ((package-host-service-package-remove svc) "q-test-pkg"))
                         (check-false (package-installed? "q-test-pkg")))))
                    (lambda ()
                      (when (directory-exists? srcdir)
                        (delete-directory/files srcdir)))))))

;; ============================================================
;; E6 — null-service degradation (direct handle-ext-pkg, no service)
;; ============================================================

(define e6-suite
  (test-suite "v0.99.88-w3-e6-null-service"
    (test-case "E6: handle-ext-pkg without a service degrades to safe defaults"
      (define list-result (handle-ext-pkg (hasheq 'action "list")))
      (check-pred tool-result? list-result)
      (check-false (tool-result-is-error? list-result))
      (check-equal? (hash-ref (car (tool-result-content list-result)) 'text "")
                    "No packages installed.")
      (check-true (tool-result-is-error? (handle-ext-pkg (hasheq 'action "info" 'name "nope"))))
      (check-true (tool-result-is-error?
                   (handle-ext-pkg (hasheq 'action "install" 'path "/tmp/nope"))))
      (check-true (tool-result-is-error? (handle-ext-pkg (hasheq 'action "remove" 'name "nope"))))
      (check-true (tool-result-is-error? (handle-ext-pkg (hasheq 'action "bogus")))))
    (test-case "E6b: explicit #f exec-ctx behaves like the null service"
      (check-pred tool-result? (handle-ext-pkg (hasheq 'action "list") #f))
      (check-false (tool-result-is-error? (handle-ext-pkg (hasheq 'action "list") #f))))))

;; ============================================================
;; E7 — ctx-injected service and full tool path
;; ============================================================

(define e7-suite
  (test-suite "v0.99.88-w3-e7-ctx-tool-path"
    (test-case "E7: ctx carries the injected service; ext-package tool uses it"
      (define srcdir (make-test-source-package))
      (dynamic-wind
       void
       (lambda ()
         (with-test-packages-dir
          (lambda ()
            (define reg (make-tool-registry))
            (define ctx
              (make-extension-ctx #:session-id "w3-e7"
                                  #:session-dir #f
                                  #:event-bus #f
                                  #:extension-registry #f
                                  #:tool-registry reg
                                  #:package-service (make-package-host-service)))
            (check-true (package-host-service? (ctx-package-service ctx)))
            (register-ext-pkg-tools ctx (hasheq))
            (define t (lookup-tool reg "ext-package"))
            (check-not-false t "ext-package tool must be registered")
            ;; empty dir → success with no packages
            (define res-empty ((tool-execute t) (hasheq 'action "list") #f))
            (check-pred tool-result? res-empty)
            (check-false (tool-result-is-error? res-empty))
            ;; install a fixture through the tool, then list shows it
            (define res-install
              ((tool-execute t) (hasheq 'action "install" 'path (path->string srcdir)) #f))
            (check-pred tool-result? res-install)
            (check-false (tool-result-is-error? res-install))
            (define res-list ((tool-execute t) (hasheq 'action "list") #f))
            (check-false (tool-result-is-error? res-list))
            (check-true (string-contains? (hash-ref (car (tool-result-content res-list)) 'text "")
                                          "q-test-pkg (1.0.0)")))))
       (lambda ()
         (when (directory-exists? srcdir)
           (delete-directory/files srcdir)))))))

(module+ test
  (require rackunit/text-ui)
  (exit (run-tests (test-suite "v0.99.88-w3-package-host-service-isolation"
                     e1-suite
                     e2-suite
                     e6-suite
                     e7-suite))))
