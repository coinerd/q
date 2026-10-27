#lang racket/base
;; @covers extensions/gsd/command-handlers.rkt
;; @speed fast
;; @suite extensions
;; @boundary integration

;; tests/test-gsd-plan-supersede-backup.rkt — W4 (GSD-SIMPLIFICATION-PLAN):
;; `/plan <text>` supersedes the previous waves directory and keeps ONE owned
;; backup (BUG-0032, bounded rotation per R-007). Rotation is only allowed for a
;; backup this operation can PROVE it created. An unknown
;; `waves-pre-plan-backup` (operator copy, `.bak` archive, another tool) is
;; preserved verbatim and supersession happens under a fresh sibling name.
;; No unknown file is ever deleted or moved.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         (only-in "../extensions/gsd/command-handlers.rkt"
                  supersede-waves-dir!
                  waves-backup-marker-filename))

(define (make-project)
  (define dir (make-temporary-file "gsd-supersede-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (display-to-file "wave body\n"
                   (build-path dir ".planning" "waves" "W0-old.md")
                   #:exists 'replace)
  dir)

(define (write-unknown-backup! dir)
  (define backup (build-path dir ".planning" "waves-pre-plan-backup"))
  (make-directory* backup)
  (display-to-file "operator copy\n" (build-path backup "W0-operator.md.bak") #:exists 'replace)
  backup)

(define supersede-suite
  (test-suite
   "gsd-plan-supersede-backup"

   (test-case "first supersession owns the canonical backup and rotates later"
     (define dir (make-project))
     (dynamic-wind
      void
      (lambda ()
        (define canonical (build-path dir ".planning" "waves-pre-plan-backup"))
        (check-equal? (supersede-waves-dir! dir) canonical)
        (check-true (file-exists? (build-path canonical "W0-old.md")))
        (check-true (file-exists? (build-path canonical waves-backup-marker-filename))
                    "the backup proves its owner")
        ;; A later supersession of the SAME owned backup rotates it (bounded
        ;; churn: one backup, not an ever-growing set).
        (make-directory* (build-path dir ".planning" "waves"))
        (display-to-file "newer wave\n"
                         (build-path dir ".planning" "waves" "W1-new.md")
                         #:exists 'replace)
        (check-equal? (supersede-waves-dir! dir) canonical)
        (check-false (file-exists? (build-path canonical "W0-old.md")))
        (check-true (file-exists? (build-path canonical "W1-new.md"))))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "an unknown backup is preserved and never deleted"
     (define dir (make-project))
     (dynamic-wind
      void
      (lambda ()
        (define unknown (write-unknown-backup! dir))
        (define target (supersede-waves-dir! dir))
        (check-true (file-exists? (build-path unknown "W0-operator.md.bak"))
                    "unknown backup content survives verbatim")
        (check-false (file-exists? (build-path unknown waves-backup-marker-filename))
                     "the unknown backup is not relabelled as ours")
        (check-not-equal? target unknown "supersession uses a fresh sibling")
        (check-true (file-exists? (build-path target "W0-old.md"))
                    "the current waves directory is still preserved")
        (check-true (file-exists? (build-path target waves-backup-marker-filename))))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   (test-case "no waves directory is a no-op"
     (define dir (make-project))
     (dynamic-wind
      void
      (lambda ()
        (delete-directory/files (build-path dir ".planning" "waves"))
        (check-false (supersede-waves-dir! dir))
        (check-false (directory-exists? (build-path dir ".planning" "waves-pre-plan-backup"))))
      (lambda () (delete-directory/files dir #:must-exist? #f))))

   ;; W4 BLOCKER (RED): rotation only checked that the marker FILE existed, so
   ;; an impostor marker (wrong kind / wrong schema / empty bytes) let the
   ;; operation delete an operator directory. Only the authentic record proves
   ;; ownership; anything else is preserved and supersession uses a sibling.
   (test-case "an impostor marker never authorizes rotating or deleting a backup"
     (define dir (make-project))
     (dynamic-wind
      void
      (lambda ()
        (define canonical (build-path dir ".planning" "waves-pre-plan-backup"))
        (make-directory* canonical)
        (call-with-output-file (build-path canonical waves-backup-marker-filename)
          (lambda (out) (write (hasheq 'schema-version 2
                                       'kind 'waves-pre-plan-backup) out))
          #:exists 'truncate)
        (display-to-file "operator copy\n"
                         (build-path canonical "W0-operator.md")
                         #:exists 'replace)
        (define target (supersede-waves-dir! dir))
        (check-not-equal? target canonical "supersession uses a fresh sibling")
        (check-true (file-exists? (build-path canonical "W0-operator.md"))
                    "the impostor-marked backup is preserved verbatim")
        (check-true (directory-exists? canonical))
        (check-true (file-exists? (build-path target "W0-old.md")))
        (check-true (file-exists? (build-path target waves-backup-marker-filename))))
      (lambda () (delete-directory/files dir #:must-exist? #f))))))

(void (run-tests supersede-suite))
