#lang racket/base
;; @covers extensions/gsd/projection-effects.rkt
;; @speed fast
;; @suite extensions
;; @boundary integration

;; tests/test-gsd-projection-temp-lifecycle.rkt — W4 (GSD-SIMPLIFICATION-PLAN):
;; projection effect-shell temporary files must be UNIQUE per write and
;; SELF-IDENTIFYING, so concurrent writers cannot collide and an interrupted
;; apply can clean up exactly its own work. Cleanup never guesses away another
;; operation's files: the shell removes only the temp paths it created, inside
;; the destination directory, and leaves unknown `.bak`/backup artifacts alone.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../extensions/gsd/projection-effects.rkt")

(define (make-dir)
  (make-temporary-file "projection-temp-~a" 'directory))

(define (dir-files dir)
  (sort (map path->string (directory-list dir #:build? #t)) string<?))

(define (owned-temp-files dir)
  (filter (lambda (p)
            (define name (path->string (file-name-from-path p)))
            (and (string? name) (string-prefix? name projection-temp-prefix)))
          (dir-files dir)))

;; Content whose display (the projection write phase) fails. This exercises the
;; window between temp allocation and temp commit, which no filesystem fixture
;; can reproduce reliably.
(struct exploding-content ()
  #:property prop:custom-write
  (lambda (_value _out _mode) (error 'projection-write-phase "disk said no")))

(define projection-temp-suite
  (test-suite
   "gsd-projection-temp-lifecycle"

   (test-case "projection temp names are unique and owned"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define target (build-path dir "PLAN.md"))
        (define a (write-temp-file! target "one\n"))
        (define b (write-temp-file! target "two\n"))
        (check-not-equal? a b "each write allocates its own temp path")
        (check-equal? (path-only a) (path-only target) "temp is a same-directory sibling")
        (define name (path->string (file-name-from-path a)))
        (check-true (string-prefix? name projection-temp-prefix)
                    "the temp name identifies the owning writer")
        (delete-file a)
        (delete-file b))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "successful apply leaves no temp residue and no unknown-file damage"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define plan (build-path dir "PLAN.md"))
        (define state (build-path dir "STATE.md"))
        (display-to-file "old plan\n" plan #:exists 'replace)
        ;; An operator artifact the shell must never touch.
        (display-to-file "precious\n" (build-path dir "PLAN.md.bak") #:exists 'replace)
        (apply-atomic-files! (list (cons plan "new plan\n") (cons state "state\n")))
        (check-equal? (file->string plan) "new plan\n")
        (check-equal? (file->string state) "state\n")
        (check-equal? (file->string (build-path dir "PLAN.md.bak")) "precious\n")
        (check-equal? (owned-temp-files dir) '()
                      "no owned temp survives a successful apply"))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "an interrupted apply cleans up only its own temps"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define first (build-path dir "PLAN.md"))
        (define impossible (build-path dir "not-a-file"))
        (make-directory impossible)
        ;; A foreign in-progress-looking file with a different owner suffix
        ;; must survive: the shell cleans only temps it allocated.
        (display-to-file "foreign\n" (build-path dir "projection.tmp-foreign") #:exists 'replace)
        (check-exn exn:fail?
                   (lambda ()
                     (apply-atomic-files! (list (cons first "one\n")
                                                (cons impossible "two\n")))))
        (check-equal? (remove* (list (build-path dir "projection.tmp-foreign"))
                               (map string->path (owned-temp-files dir)))
                      '()
                      "the shell removes exactly the temps it created")
        (check-equal? (file->string (build-path dir "projection.tmp-foreign")) "foreign\n"))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "atomic-write-file! publishes whole content or nothing"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define target (build-path dir "waves" "W0-x.md"))
        (atomic-write-file! target "complete\n")
        (check-equal? (file->string target) "complete\n")
        (atomic-write-file! target "replaced\n")
        (check-equal? (file->string target) "replaced\n")
        (check-equal? (owned-temp-files (build-path dir "waves")) '()))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   ;; W4 write-phase residue (RED): the shell allocated its temp and then wrote
   ;; the content; a failure DURING that write (ENOSPC, permission change,
   ;; signal) used to leave the allocated temp behind, because only the caller's
   ;; list of committed temps was cleaned. The allocator now removes exactly the
   ;; temp it just created before re-raising.
   (test-case "a write-phase failure leaves no owned temp behind"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define before (dir-files dir))
        (check-exn exn:fail?
                   (lambda () (write-temp-file! (build-path dir "PLAN.md")
                                                (exploding-content))))
        (check-equal? (dir-files dir) before
                      "the failed write phase removes the temp it allocated")
        (check-equal? (owned-temp-files dir) '()))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "an interrupted apply clears its own temps when a later write fails"
     (define dir (make-dir))
     (dynamic-wind
      void
      (lambda ()
        (define first (build-path dir "PLAN.md"))
        (display-to-file "old plan\n" first #:exists 'replace)
        (check-exn exn:fail?
                   (lambda ()
                     (apply-atomic-files! (list (cons first "one\n")
                                                (cons (build-path dir "STATE.md")
                                                      (exploding-content))))))
        (check-equal? (file->string first) "old plan\n"
                      "the earlier temp is never published")
        (check-equal? (owned-temp-files dir) '()
                      "no owned temp survives a write-phase failure"))
      (lambda () (delete-directory/files dir #:must-exist? #f))))))

(void (run-tests projection-temp-suite))
