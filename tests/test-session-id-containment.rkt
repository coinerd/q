#lang racket

;; @speed fast
;; @suite security
;; BOUNDARY: integration

(require rackunit
         racket/file
         racket/path
         racket/port
         "../runtime/session/session-path.rkt"
         "../runtime/session/session-manager.rkt"
         "../interfaces/sessions.rkt"
         "../util/message/protocol-types.rkt"
         "../runtime/agent-session.rkt"
         "../util/event/event-bus.rkt"
         (only-in "helpers/mock-provider.rkt" make-test-config make-simple-mock-provider))

(define (temp-dir pattern)
  (make-temporary-file pattern 'directory))

(define (sample-message)
  (make-message "m1" #f 'user 'message '() 0 (hasheq)))

(test-case "valid legacy and generated one-component session IDs resolve below root"
  (define root (temp-dir "q-contained-root-~a"))
  (for ([sid (in-list '("s1" "sess-info" "01JZZZZZZZZZZZZZZZZZZZZZZZ"))])
    (define p (resolve-session-path root sid))
    (check-true (session-path-contained? root p))
    (check-false (directory-exists? p)))
  (delete-directory/files root))

(test-case "invalid session IDs fail before filesystem effects"
  (define root (temp-dir "q-invalid-root-~a"))
  (define outside (temp-dir "q-invalid-outside-~a"))
  (define sentinel (build-path outside "sentinel.txt"))
  (display-to-file "unchanged" sentinel #:exists 'truncate)
  (for ([sid (in-list (list ""
                            "."
                            ".."
                            "../outside"
                            "a/b"
                            "a\\b"
                            "/tmp/outside"
                            "C:\\outside"
                            "C:/outside"
                            "C:outside"
                            "\\\\server\\share"
                            (string #\nul)))])
    (check-exn exn:fail? (lambda () (resolve-session-path root sid)))
    (check-equal? (file->string sentinel) "unchanged"))
  (delete-directory/files root)
  (delete-directory/files outside))

(test-case "existing session directory symlink cannot escape root"
  (define root (temp-dir "q-link-root-~a"))
  (define outside (temp-dir "q-link-outside-~a"))
  (display-to-file "outside" (build-path outside "session.jsonl") #:exists 'truncate)
  (make-file-or-directory-link outside (build-path root "escape"))
  (check-exn exn:fail? (lambda () (resolve-session-path root "escape")))
  (check-exn exn:fail? (lambda () (resolve-session-path root "escape" "session.jsonl")))
  (delete-directory/files root)
  (delete-directory/files outside))

(test-case "session artifact symlink cannot escape its contained directory"
  (define root (temp-dir "q-artifact-root-~a"))
  (define outside (temp-dir "q-artifact-outside-~a"))
  (define session-dir (build-path root "safe"))
  (make-directory session-dir)
  (define sentinel (build-path outside "sentinel.jsonl"))
  (display-to-file "outside" sentinel #:exists 'truncate)
  (make-file-or-directory-link sentinel (build-path session-dir "session.jsonl"))
  (check-exn exn:fail? (lambda () (resolve-session-path root "safe" "session.jsonl")))
  (check-equal? (file->string sentinel) "outside")
  (delete-directory/files root)
  (delete-directory/files outside))

(test-case "persistent manager rejects traversal before append load or fork effects"
  (define parent (temp-dir "q-manager-parent-~a"))
  (define root (build-path parent "sessions"))
  (define outside (build-path parent "outside"))
  (make-directory* outside)
  (define outside-log (build-path outside "session.jsonl"))
  (define outside-pending (build-path outside "session.jsonl.pending"))
  (display-to-file "log-unchanged" outside-log #:exists 'truncate)
  (display-to-file "pending-unchanged" outside-pending #:exists 'truncate)
  (define mgr (make-persistent-session-manager root))
  (for ([op (in-list (list (lambda () (sm-append! mgr "../outside" (sample-message)))
                           (lambda () (sm-load mgr "../outside"))
                           (lambda () (sm-fork! mgr "../outside" "safe-dest"))
                           (lambda () (sm-fork! mgr "missing" "../outside"))))])
    (check-exn exn:fail? op)
    (check-equal? (file->string outside-log) "log-unchanged")
    (check-equal? (file->string outside-pending) "pending-unchanged"))
  (check-false (directory-exists? (build-path root "safe-dest")))
  (delete-directory/files parent))

(test-case "persistent append rejects an escaping pending-marker symlink"
  (define parent (temp-dir "q-pending-link-parent-~a"))
  (define root (build-path parent "sessions"))
  (define session-dir (build-path root "safe"))
  (define outside (build-path parent "outside"))
  (make-directory* session-dir)
  (make-directory* outside)
  (define sentinel (build-path outside "pending-sentinel"))
  (display-to-file "unchanged" sentinel #:exists 'truncate)
  (make-file-or-directory-link sentinel (build-path session-dir "session.jsonl.pending"))
  (define mgr (make-persistent-session-manager root))
  (check-exn exn:fail? (lambda () (sm-append! mgr "safe" (sample-message))))
  (check-equal? (file->string sentinel) "unchanged")
  (check-false (file-exists? (build-path session-dir "session.jsonl")))
  (delete-directory/files parent))

(test-case "session info and delete reject traversal without prompt or deletion"
  (define parent (temp-dir "q-interface-parent-~a"))
  (define root (build-path parent "sessions"))
  (define outside (build-path parent "outside"))
  (make-directory* outside)
  (define sentinel (build-path outside "sentinel.txt"))
  (display-to-file "unchanged" sentinel #:exists 'truncate)
  (check-false (sessions-info root "../outside"))
  (define out (open-output-string))
  (check-equal?
   (sessions-delete root "../outside" #:confirm? #t #:in (open-input-string "y\n") #:out out)
   'not-found)
  (check-equal? (get-output-string out) "")
  (check-equal? (file->string sentinel) "unchanged")
  (delete-directory/files parent))

(test-case "list excludes a session-directory symlink escaping root"
  (define root (temp-dir "q-list-root-~a"))
  (define outside (temp-dir "q-list-outside-~a"))
  (display-to-file "{}\n" (build-path outside "session.jsonl") #:exists 'truncate)
  (make-file-or-directory-link outside (build-path root "escape"))
  (check-equal? (scan-session-dirs root) '())
  (delete-directory/files root)
  (delete-directory/files outside))

(test-case "resume rejects traversal before outside log or index effects"
  (define parent (temp-dir "q-resume-parent-~a"))
  (define root (build-path parent "sessions"))
  (define outside (build-path parent "outside"))
  (make-directory* outside)
  (define log-path (build-path outside "session.jsonl"))
  (display-to-file "sentinel-log\n" log-path #:exists 'truncate)
  (define bus (make-event-bus))
  (define cfg (make-test-config root bus (make-simple-mock-provider "unused")))
  (check-exn exn:fail? (lambda () (resume-agent-session "../outside" cfg)))
  (check-equal? (file->string log-path) "sentinel-log\n")
  (check-false (file-exists? (build-path outside "session.index")))
  (delete-directory/files parent))

(test-case "resume rejects an escaping session index symlink before publication"
  (define parent (temp-dir "q-resume-index-parent-~a"))
  (define root (build-path parent "sessions"))
  (define session-dir (build-path root "safe"))
  (define outside (build-path parent "outside"))
  (make-directory* session-dir)
  (make-directory* outside)
  (display-to-file "" (build-path session-dir "session.jsonl") #:exists 'truncate)
  (define sentinel (build-path outside "index-sentinel"))
  (display-to-file "index-unchanged" sentinel #:exists 'truncate)
  (make-file-or-directory-link sentinel (build-path session-dir "session.index"))
  (define cfg (make-test-config root (make-event-bus) (make-simple-mock-provider "unused")))
  (check-exn exn:fail? (lambda () (resume-agent-session "safe" cfg)))
  (check-equal? (file->string sentinel) "index-unchanged")
  (check-equal? (file->string (build-path session-dir "session.jsonl")) "")
  (delete-directory/files parent))
