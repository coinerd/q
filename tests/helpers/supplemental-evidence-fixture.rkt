#lang racket/base
(require racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)
(provide call-with-supplemental-fixture
         fixture-git
         fixture-put
         fixture-context
         fixture-copy
         fixture-helper-path
         fixture-test-path)
(define-runtime-path root "../..")
(define-runtime-path fixtures "../fixtures/supplemental-evidence")
(define fixture-helper-path "tests/helpers/tracker-evidence.rkt")
(define fixture-test-path "tests/probe.rkt")
(define (fixture-git repo . args)
  (define out (open-output-string))
  (unless (parameterize ([current-directory repo]
                         [current-output-port out]
                         [current-error-port (open-output-nowhere)])
            (apply system* (find-executable-path "git") args))
    (error 'fixture-git "failed: ~s" args))
  (string-trim (get-output-string out)))
(define (fixture-put repo path text)
  (define dest (build-path repo path))
  (make-parent-directory* dest)
  (display-to-file text dest #:exists 'truncate))
(define (fixture-copy repo name path)
  (fixture-put repo path (file->string (build-path fixtures name))))
(define (fixture-context repo)
  (hasheq 'plan-id
          (make-string 64 #\a)
          'wave
          1
          'attempt-id
          "fixture"
          'attempt-fence
          0
          'receipt-generation
          0
          'receipt-sha256
          (make-string 64 #\b)
          'evidence-revision
          0
          'repo
          (path->string repo)
          'branch
          "fixture"
          'origin
          "local"
          'head
          (fixture-git repo "rev-parse" "HEAD")
          'tree
          (fixture-git repo "rev-parse" "HEAD^{tree}")
          'snapshot-sha256
          (make-string 64 #\c)
          'green-proof-sha256
          (make-string 64 #\d)))
(define (call-with-supplemental-fixture proc)
  (define repo (make-temporary-file "supplemental-probe-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (fixture-git repo "init" "-q")
     (fixture-git repo "config" "user.name" "Fixture")
     (fixture-git repo "config" "user.email" "fixture@invalid")
     ;; Preserve the real, stable three-argument q-settings constructor and its
     ;; dependency bytes. Never call its config-loading entry points.
     (for ([p '("runtime/settings-core.rkt" "util/json/json-helpers.rkt"
                                            "util/credential-redaction.rkt"
                                            "util/config-paths.rkt"
                                            "util/error/errors.rkt"
                                            "util/error/deprecation.rkt")])
       (fixture-put repo p (file->string (build-path root p))))
     (fixture-copy repo "baseline-query.rkt" "runtime/settings-query.rkt")
     (fixture-git repo "add" "runtime" "util")
     (fixture-git repo "commit" "-qm" "pre-feature production")
     (define baseline (fixture-git repo "rev-parse" "HEAD"))
     (fixture-copy repo "candidate-query.rkt" "runtime/settings-query.rkt")
     (fixture-copy repo "probe.rkt" fixture-test-path)
     (fixture-copy repo "tracker-evidence.rkt" fixture-helper-path)
     (fixture-put repo
                  "scripts/run-tests.rkt"
                  (string-append
                   "#lang racket/base\n(for ([test (in-vector (current-command-line-arguments))])\n"
                   "  (dynamic-require `(submod ,(string->path test) test) #f))\n"))
     (fixture-git repo "add" "runtime/settings-query.rkt" "tests" "scripts")
     (fixture-git repo "commit" "-qm" "candidate and identical-byte overlay witness")
     (proc repo baseline))
   (lambda () (delete-directory/files repo))))
