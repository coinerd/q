#lang racket

;;; test-reload-bytecode-recovery.rkt — W3 delivery test for BUG-0047.
;;;
;;; FLIPPED from the W0 characterization pin: /reload now has a
;;; bytecode-cache purge step and reloads extensions through a FRESH
;;; namespace (recompile from current source) with HONEST failure
;;; reporting via reload-extensions!/report. This test proves:
;;;
;;;   (a) extensions/loader.rkt CONTAINS the purge seam
;;;       (purge-compiled-dirs! wired into the reload path).
;;;   (b) RECOVERY: after the stale-linklet condition (source rewritten
;;;       underneath a loaded extension), /reload picks up the NEW
;;;       module instance — verified by dispatching the registered
;;;       startup hook and observing the NEW marker — not by trusting
;;;       the reported names.
;;;   (c) PURGE+RETRY: with a corrupt/stale .zo bytecode file planted
;;;       for the extension, reload still ends in a working registry
;;;       and the planted compiled/ cache was purged.
;;;   (d) NO FALSE SUCCESS: a syntactically broken extension produces a
;;;       NAMED failure in the report, an empty 'loaded list, and the
;;;       broken extension is NOT registered — never "N extensions
;;;       reloaded" while broken.
;;;
;;; Pure in-process test: no live TUI/worker processes are spawned.

(require rackunit
         racket/file
         racket/path
         (file "../extensions/loader.rkt")
         (file "../extensions/api.rkt")
         (file "../util/hook-types.rkt"))

;; path->complete-path: `find-system-path 'run-file` echoes the invocation
;; spelling — `racket tests/x.rkt` from the git root yields a RELATIVE
;; run-file, which would collapse repo-root to "." and break the temp
;; extension's absolute api.rkt require below.
(define repo-root
  (simplify-path (build-path (path->complete-path (find-system-path 'run-file)) 'up 'up)))

(define loader-source
  (file->string (build-path repo-root "extensions" "loader.rkt")))

;; --- Pin (a) FLIPPED: the bytecode purge seam now EXISTS. The W0 pin
;; asserted its absence; W3 added purge-compiled-dirs! and wired it into
;; reload-extensions!/report.
(check-true
 (ormap (lambda (line)
          (regexp-match? #px"(?i:(delete|remove|purge).{0,80}compiled|compiled.{0,80}(delete|remove|purge))"
                         line))
        (string-split loader-source "\n"))
 "extensions/loader.rkt contains the bytecode/compiled purge step (absent seam in W0, present since W3)")

(check-not-false (regexp-match? #rx"purge-compiled-dirs!" loader-source)
                 "purge-compiled-dirs! is defined")

(check-not-false (regexp-match? #rx"reload-extensions!/report" loader-source)
                 "reload-extensions!/report exists (honest reporting seam)")

(check-not-false (regexp-match? #rx"load-extension-fresh" loader-source)
                 "load-extension-fresh exists (fresh-namespace recompile seam)")

;; --- Shared fixture: a temp-dir extension whose startup hook returns
;; the module-level MARKER in its result payload. Marker visibility
;; through real hook dispatch is how the test distinguishes the OLD
;; cached module instance from the freshly recompiled one.
(define tmp-dir (make-temporary-file "bug0047-tmp~a" 'directory))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-dir)))

;; Dispatch the registered 'startup hook for `name` and return the
;; marker carried in the result payload — proves the registry holds a
;; WORKING extension compiled from the expected source, not a stale one.
(define (startup-marker registry name)
  (define pairs (handlers-for-point registry 'startup))
  (define hit (assoc name pairs))
  (and hit
       (let* ([handlers (cdr hit)]
              [result ((car handlers) #f (hasheq 'probe #t))])
         (hash-ref (hook-result-payload result) 'marker #f))))

(with-handlers ([exn:fail? (lambda (e) (cleanup!) (raise e))])

  (define ext-path (build-path tmp-dir "reload-stale.rkt"))

  ;; The extension module uses the REPO api.rkt so its `the-extension`
  ;; value satisfies the `extension?` check in the running namespace
  ;; (load-extension-fresh attaches the shared modules — identity holds).
  (define api-path (path->string (build-path repo-root "extensions" "api.rkt")))

  (define (write-ext! marker #:broken? [broken? #f])
    (call-with-output-file #:exists 'truncate
      ext-path
      (lambda (out)
        (fprintf out "#lang racket\n")
        (unless broken? (fprintf out "(require (file ~s))\n" api-path))
        (unless broken? (fprintf out "(provide the-extension)\n"))
        (fprintf out "(define marker ~s)\n" marker)
        (if broken?
            (fprintf out "(define the-extension (extension oops unclosed\n")
            (begin
              (fprintf out "(module+ proxy (provide marker))\n")
              (fprintf out
                       "(require (file ~s))\n" (path->string (build-path repo-root "util" "hook-types.rkt")))
              (fprintf out
                       "(define the-extension (extension \"reload-stale\" \"1.0.0\" \"1.0.0\" (hasheq 'startup (list (lambda (ctx payload) (hook-result 'continue (hash-set payload 'marker marker)))))))\n"))))))

  (define registry (make-extension-registry))

  ;; --- (b) RECOVERY: initial load of v1, source rewritten underneath
  ;; (stale-linklet condition), reload picks up v2 through real hook
  ;; dispatch — no TUI restart, no manual purge.
  (write-ext! "v1")
  (define report-1 (reload-extensions!/report registry (list (path->string tmp-dir))))
  (check-equal? (hash-ref report-1 'loaded) '("reload-stale")
                "initial reload reports the extension loaded")
  (check-equal? (hash-ref report-1 'failed) '()
                "initial reload has no failures")
  (check-equal? (startup-marker registry "reload-stale") "v1"
                "initial registry dispatch observes marker v1")

  ;; Source rewritten underneath the loaded module — the stale-linklet
  ;; condition.
  (write-ext! "v2")
  (define report-2 (reload-extensions!/report registry (list (path->string tmp-dir))))
  (check-equal? (hash-ref report-2 'loaded) '("reload-stale")
                "changed-source reload reports the extension loaded")
  (check-equal? (hash-ref report-2 'failed) '()
                "changed-source reload reports no failures")
  (check-equal? (startup-marker registry "reload-stale") "v2"
                "RECOVERED: registry dispatch observes marker v2 — the new instance, not the cached linklet (BUG-0047 fixed)")

  ;; --- (c) PURGE+RETRY: a corrupt/stale .zo planted for the extension
  ;; must not leave the registry broken. The reload path purges
  ;; compiled/ caches under the extension roots (step 2 of
  ;; reload-extensions!/report) and recompiles from source.
  (define compiled-dir (build-path tmp-dir "compiled"))
  (make-directory* compiled-dir)
  (with-output-to-file (build-path compiled-dir "reload-stale_rkt.zo")
    (lambda () (display "not-a-valid-bytecode-file")))
  (write-ext! "v3")
  (define report-3 (reload-extensions!/report registry (list (path->string tmp-dir))))
  (check-equal? (hash-ref report-3 'loaded) '("reload-stale")
                "corrupt-zo reload still ends with the extension loaded")
  (check-equal? (hash-ref report-3 'failed) '()
                "corrupt-zo reload reports no failures")
  (check-equal? (startup-marker registry "reload-stale") "v3"
                "corrupt-zo reload recovered from CURRENT source (marker v3)")
  (check-not-false (member compiled-dir (hash-ref report-3 'purged))
                   "planted compiled/ cache was purged during reload")

  ;; --- (d) NO FALSE SUCCESS: a syntactically broken extension yields a
  ;; NAMED failure and is NOT registered — /reload never claims success
  ;; for an extension it could not load.
  (write-ext! "v4" #:broken? #t)
  (define report-4 (reload-extensions!/report registry (list (path->string tmp-dir))))
  (check-equal? (hash-ref report-4 'loaded) '()
                "broken extension is NOT reported as loaded (no false success)")
  (define failures-4 (hash-ref report-4 'failed))
  (check-equal? (length failures-4) 1
                "exactly one named failure is reported")
  (define failure-4 (car failures-4))
  (check-equal? (car failure-4) "reload-stale" "failure names the extension")
  (check-true (non-empty-string? (cdr failure-4))
              "failure carries a non-empty message")
  (check-equal? (handlers-for-point registry 'startup) '()
                "broken extension is not registered (registry stays clean)")

  ;; Backward-compatible wrapper still returns just the loaded names.
  (write-ext! "v5")
  (check-equal? (reload-extensions! registry (list (path->string tmp-dir)))
                '("reload-stale")
                "reload-extensions! wrapper returns loaded names")
  (check-equal? (startup-marker registry "reload-stale") "v5"
                "final registry dispatch observes marker v5")

  (cleanup!))

(displayln "PASS test-reload-bytecode-recovery (BUG-0047 fixed: purge seam present; stale-linklet recovered in-process; corrupt-zo purged; broken extension fails honestly)")
