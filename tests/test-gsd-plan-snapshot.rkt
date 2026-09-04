#lang racket/base
;; @covers extensions/gsd/plan-snapshot.rkt
;; @speed fast  ;; @suite extensions

(require racket/path
         racket/string
         rackunit
         rackunit/text-ui
         racket/file
         "../extensions/gsd/plan-snapshot.rkt"
         (only-in "../util/json/checksum.rkt" sha256-string))

(define plan-text
  (string-append "# Plan\n\n"
                 "- [Inbox] W0: Alpha -> waves/W0-alpha.md\n"
                 "- [Inbox] W1: Beta -> waves/W1-beta.md\n"))

(define wave-0 "# Wave 0\n\nalpha body\n")
(define wave-1 "# Wave 1\n\nbeta body\n")
(define campaign-id (make-string 64 #\a))

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

(define (make-plan-tree)
  (define dir (make-temporary-file "plan-snapshot-~a" 'directory))
  (write-text! (build-path dir ".planning" "PLAN.md") plan-text)
  (write-text! (build-path dir ".planning" "waves" "W0-alpha.md") wave-0)
  (write-text! (build-path dir ".planning" "waves" "W1-beta.md") wave-1)
  dir)

(define plan-snapshot-suite
  (test-suite "gsd-plan-snapshot"

    (test-case "extracts and deduplicates real PLAN.md wave references"
      (check-equal? (plan-references->wave-doc-paths (string-append plan-text
                                                                    "duplicate waves/W0-alpha.md\n"))
                    '("waves/W0-alpha.md" "waves/W1-beta.md")))

    (test-case "seed binds and verifies PLAN plus every referenced wave"
      (define dir (make-plan-tree))
      (define-values (bound-path digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? bound-path (path->string (snapshot-dir dir campaign-id)))
      (check-regexp-match #px"^[0-9a-f]+$" digest)
      (check-equal? (string-length digest) 64)
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-equal? (map snapshot-file-path (plan-snapshot-manifest-files manifest))
                    '("PLAN.md" "waves/W0-alpha.md" "waves/W1-beta.md"))
      (write-text! (build-path (snapshot-dir dir campaign-id) "waves" "W0-alpha.md")
                   "corrupt snapshot bytes")
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "snapshot capture is create-once across resume migration"
      (define dir (make-plan-tree))
      (define-values (_path first-digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (define captured-plan (file->string (build-path (snapshot-dir dir campaign-id) "PLAN.md")))
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (string-append plan-text "\nauthored later\n"))
      (define-values (_same-path second-digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? second-digest first-digest)
      (check-equal? (file->string (build-path (snapshot-dir dir campaign-id) "PLAN.md"))
                    captured-plan)
      (delete-directory/files dir))

    (test-case "manifest traversal entry is rejected before external access"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define outside (build-path dir "outside.txt"))
      (write-text! outside "owned")
      (define manifest-path (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd"))
      (define datum (call-with-input-file manifest-path read))
      (define evil-file
        (hasheq 'path "../../../../outside.txt" 'size 5 'sha256 (sha256-string "owned")))
      (call-with-output-file manifest-path
                             (lambda (out) (write (hash-set datum 'files (list evil-file)) out))
                             #:exists 'truncate)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "symlinked snapshot entry is rejected even when bytes match"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define outside (build-path dir "outside-wave.md"))
      (write-text! outside wave-0)
      (define captured (build-path (snapshot-dir dir campaign-id) "waves" "W0-alpha.md"))
      (delete-file captured)
      (make-file-or-directory-link outside captured)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "restore rejects dangling symlink destinations without following them"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-plan (build-path dir ".planning" "PLAN.md"))
      (define outside (build-path dir "outside-plan.md"))
      (delete-file live-plan)
      (make-file-or-directory-link outside live-plan)
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-false (file-exists? outside))
      (delete-directory/files dir))

    (test-case "restore rejects a symlinked live parent directory"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-waves (build-path dir ".planning" "waves"))
      (define outside (build-path dir "outside-waves"))
      (delete-directory/files live-waves)
      (make-directory outside)
      (make-file-or-directory-link outside live-waves)
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-false (file-exists? (build-path outside "W0-alpha.md")))
      (delete-directory/files dir))

    (test-case "missing referenced wave fails before writing a snapshot"
      (define dir (make-plan-tree))
      (delete-file (build-path dir ".planning" "waves" "W1-beta.md"))
      (check-exn exn:fail:gsd-missing-wave-doc?
                 (lambda () (seed-and-bind-plan-snapshot! dir campaign-id)))
      (check-false (directory-exists? (snapshot-dir dir campaign-id)))
      (delete-directory/files dir))

    (test-case "status projection changes do not count as content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (regexp-replace* (regexp (regexp-quote "[Inbox]")) plan-text "[DONE]"))
      (write-text! (build-path dir ".planning" "waves" "W0-alpha.md")
                   (string-append "# Wave 0\nStatus: FAILED\n\n"
                                  wave-0
                                  "\n## Last Failure\n\nverifier rejected\n"))
      (check-equal? (snapshot-drift? dir campaign-id) '())
      (write-text! (build-path dir ".planning" "waves" "W0-alpha.md")
                   "# Wave 0\nStatus: DONE\n\nchanged body\n")
      (check-equal? (snapshot-drift? dir campaign-id) '("waves/W0-alpha.md"))
      (delete-directory/files dir))

    (test-case "restore replaces missing files but rejects existing content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define plan-path (build-path dir ".planning" "PLAN.md"))
      (define wave-path (build-path dir ".planning" "waves" "W0-alpha.md"))
      (write-text! plan-path (regexp-replace (regexp (regexp-quote "[Inbox]")) plan-text "[DONE]"))
      (delete-file wave-path)
      (check-equal? (restore-plan-from-snapshot! dir campaign-id) '("waves/W0-alpha.md"))
      (check-equal? (file->string wave-path) wave-0)
      (check-true (string-contains? (file->string plan-path) "[DONE]"))
      (write-text! wave-path "# Wave 0\nStatus: DONE\n\nmalicious edit\n")
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-regexp-match #rx"malicious edit" (file->string wave-path))
      (delete-directory/files dir))))

(void (run-tests plan-snapshot-suite))
