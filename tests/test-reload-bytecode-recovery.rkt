#lang racket

;;; test-reload-bytecode-recovery.rkt — W0 characterization pin for BUG-0047.
;;;
;;; TODAY's behavior (to be flipped by W3 when /reload gains a
;;; bytecode-purge/recompile step):
;;;   (a) extensions/loader.rkt contains NO bytecode ("compiled" cache)
;;;       purge/remove/recompile step anywhere in the reload path.
;;;   (b) After loading an extension, rewriting its source underneath the
;;;       loaded module (the stale-linklet condition), and reloading,
;;;       reload-extensions! REPORTS the extension as successfully
;;;       reloaded — the result carries no staleness information at all
;;;       and nothing verifies the linklet actually matches the new
;;;       source.
;;;
;;; Pure characterization: no live TUI/worker processes are spawned.

(require rackunit
         racket/file
         racket/path
         (file "../extensions/loader.rkt")
         (file "../extensions/api.rkt"))

(define repo-root
  (simplify-path (build-path (find-system-path 'run-file) 'up 'up)))

(define loader-source
  (file->string (build-path repo-root "extensions" "loader.rkt")))

;; --- Pin (a): no bytecode purge/recompile step anywhere in the
;; /reload implementation. Any line that both mentions the compiled
;; cache and a delete/purge/remove action would be the seam — none
;; exists today. (Absent-seam marker, v1.00.19 freshness-pin precedent.)
(check-false
 (ormap (lambda (line)
          (regexp-match? #px"(?i:(delete|remove|purge).{0,80}compiled|compiled.{0,80}(delete|remove|purge))"
                         line))
        (string-split loader-source "\n"))
 "extensions/loader.rkt contains no bytecode/compiled purge step (absent seam)")

(check-not-false
 (regexp-match? #rx"reload-extensions!" loader-source)
 "reload-extensions! exists (the seam that lacks the purge step)")

;; --- Pin (b): stale-linklet state is REPORTED as reloaded. We build a
;; temp-dir extension, load it, rewrite its source, then reload: the
;; reload path reports success for the changed extension without any
;; recompile/staleness verification.
(define tmp-dir (make-temporary-file "bug0047-tmp~a" 'directory))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-dir)))

(with-handlers ([exn:fail? (lambda (e) (cleanup!) (raise e))])

  (define ext-path (build-path tmp-dir "reload-stale.rkt"))

  ;; The extension module uses the REPO api.rkt so its `the-extension`
  ;; value satisfies the `extension?` check inside reload-extensions!.
  (define api-path (path->string (build-path repo-root "extensions" "api.rkt")))

  (define (write-ext! marker)
    (call-with-output-file #:exists 'truncate
      ext-path
      (lambda (out)
        (fprintf out "#lang racket\n")
        (fprintf out "(require (file ~s))\n" api-path)
        (fprintf out "(provide the-extension)\n")
        (fprintf out "(define marker ~s)\n" marker)
        (fprintf out
                 "(define the-extension (extension \"reload-stale\" \"1.0.0\" \"1.0.0\" (hasheq 'startup (list))))\n")
        (fprintf out "(module+ proxy (provide marker))\n"))))

  (define registry (make-extension-registry))

  ;; Initial load of v1.
  (write-ext! "v1")
  (define reloaded-1 (reload-extensions! registry (list ext-path)))
  (check-pred (lambda (names) (member "reload-stale" names)) reloaded-1
              "initial reload reports the extension loaded")

  ;; Source rewritten underneath the loaded module — the stale-linklet
  ;; condition.
  (write-ext! "v2")

  ;; /reload reports the extension as successfully reloaded despite the
  ;; changed source and despite having no purge/recompile step: the
  ;; reload result carries no staleness information at all.
  (define reloaded-2 (reload-extensions! registry (list ext-path)))
  (check-pred (lambda (names) (member "reload-stale" names)) reloaded-2
              "changed-source extension is REPORTED as reloaded (BUG-0047: no recompile verification)")

  (cleanup!))

(displayln "PASS test-reload-bytecode-recovery (BUG-0047 pin: no purge step; stale state reported as reloaded)")
