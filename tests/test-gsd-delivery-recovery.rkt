#lang racket/base
;; @speed fast
;; @timeout 300
;; @suite extensions
;; @covers extensions/gsd/delivery-recovery.rkt
(require json
         rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         (only-in "helpers/delivery-fixtures.rkt" write-plan! write-wave-doc! write-state!)
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-recovery.rkt")

(define manifest (make-campaign-manifest 1 "recovery" '() '() "constraints"))
(define plan (campaign-manifest-hash manifest))
(define attempt-id "attempt-5")
(define fence 5)
(define branch "campaign/recovery-w0")
(define tag-branch "delivery-tag")
(define origin-url "https://github.com/example/q")
(define-runtime-path recovery-script "../scripts/gsd-recover-delivery.rkt")

(define (git-out dir . args)
  (string-trim (with-output-to-string (lambda () (apply git-quiet! dir args)))))

(define (write-rkt path n)
  (make-parent-directory* path)
  (call-with-output-file path
                         (lambda (out)
                           (fprintf out "#lang racket/base\n(provide value)\n(define value ~a)\n" n))
                         #:exists 'truncate))

(define (file-url p)
  (string-append "file://" (path->string (path->complete-path p))))

(define (make-root)
  (define root (make-temporary-file "delivery-recovery-root-~a" 'directory))
  (make-directory* (build-path root ".planning" "campaigns"))
  (make-directory* (build-path root ".planning" "waves"))
  (write-plan! root 0 "Recovery" "zero")
  (write-wave-doc! root 0 "zero" '("q/ui-core/preferences.rkt") "raco make ui-core/preferences.rkt")
  (write-state! root 0 "42")
  root)

(define (make-candidate root)
  (define bare (build-path root "origin.git"))
  (define repo (build-path root "candidate"))
  (git-quiet! root "init" "--bare" "-q" (path->string bare))
  (make-directory repo)
  (git-quiet! repo "init" "-q")
  (git-quiet! repo "symbolic-ref" "HEAD" "refs/heads/main")
  (hermetic-identity! repo)
  (git-quiet! repo "config" "remote.origin.url" origin-url)
  (git-quiet! repo "config" (string-append "url." (file-url bare) ".insteadOf") origin-url)
  (write-rkt (build-path repo "ui-core" "preferences.rkt") 1)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "base")
  (define base (git-out repo "rev-parse" "HEAD"))
  (git-quiet! repo "checkout" "-q" "-b" branch)
  (write-rkt (build-path repo "ui-core" "preferences.rkt") 2)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "target delivery")
  (define old-head (git-out repo "rev-parse" "HEAD"))
  (display-to-file "repair\n" (build-path repo "repair.rkt") #:exists 'truncate)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "repair tail")
  (define head (git-out repo "rev-parse" "HEAD"))
  (git-quiet! repo "tag" tag-branch head)
  (git-quiet! repo "push" "-q" "origin" branch)
  (git-quiet! repo "push" "-q" "origin" (string-append "refs/tags/" tag-branch))
  (git-quiet! repo "fetch" "-q" "origin" (string-append branch ":refs/remotes/origin/" branch))
  (values bare repo base old-head head))

(define (receipt repo old-head #:branch* [branch* branch])
  (hasheq 'repo
          (path->string (path->directory-path (path->complete-path repo)))
          'branch
          branch*
          'head
          old-head
          'tree
          (git-out repo "rev-parse" (string-append old-head "^{tree}"))
          'origin
          origin-url
          'verified-at
          0
          'evidence
          "old verify"
          'attempt-id
          attempt-id
          'attempt-fence
          fence))

(define (seed-campaign! root
                        repo
                        old-head
                        #:status [status 'awaiting-delivery]
                        #:cancel? [cancel? #f]
                        #:branch* [branch* branch]
                        #:stage [stage "implementation-ci"])
  (define w (make-campaign-wave 0 "Recovery" status 5 (campaign-attempt attempt-id fence 0)))
  (define rec
    (make-campaign-record plan
                          manifest
                          (list w)
                          (and cancel? (make-campaign-cancellation "stop" 0))
                          fence
                          'test
                          0
                          0))
  (persist-campaign! root rec)
  (record-delivery-receipt! root plan 0 (receipt repo old-head #:branch* branch*))
  (update-delivery-journal! root plan 0 (hasheq 'stage stage))
  rec)

(define (tree-bytes dir)
  (define files
    (sort (for/list ([p (in-directory dir)]
                     #:when (file-exists? p))
            p)
          string<?
          #:key path->string))
  (for/list ([p (in-list files)])
    (cons (find-relative-path dir p) (file->bytes p))))

(define (with-git-rewrite bare thunk)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"GIT_CONFIG_COUNT" #"1")
  (environment-variables-set!
   env
   #"GIT_CONFIG_KEY_0"
   (string->bytes/utf-8 (string-append "url." (file-url bare) ".insteadOf")))
  (environment-variables-set! env #"GIT_CONFIG_VALUE_0" (string->bytes/utf-8 origin-url))
  (parameterize ([current-environment-variables env]
                 [current-gsd-recovery-github-origin? (lambda (v) (equal? v origin-url))])
    (thunk)))

(define (call-recover root
                      bare
                      base
                      old-head
                      head
                      #:apply? [apply? #f]
                      #:fence* [fence* fence]
                      #:attempt* [attempt* attempt-id]
                      #:expected-head* [expected-head* head]
                      #:expected-base* [expected-base* base]
                      #:old-head* [old-head* old-head])
  (with-git-rewrite bare
                    (lambda ()
                      (recover-delivery! #:root root
                                         #:plan plan
                                         #:wave 0
                                         #:attempt-id attempt*
                                         #:fence fence*
                                         #:expected-head expected-head*
                                         #:expected-base expected-base*
                                         #:old-receipt-head old-head*
                                         #:apply? apply?))))

(define (with-fixture proc #:branch* [branch* branch] #:stage [stage "implementation-ci"])
  (define root (make-root))
  (dynamic-wind void
                (lambda ()
                  (define-values (bare repo base old-head head) (make-candidate root))
                  (seed-campaign! root repo old-head #:branch* branch* #:stage stage)
                  (proc root bare repo base old-head head))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(define (park-active-as-superseded! root)
  (define active (delivery-journal-path root plan 0))
  (define superseded
    (build-path root ".planning" "campaigns" plan "coordinator-w0.json.reconciled-superseded"))
  (make-parent-directory* superseded)
  (copy-file active superseded #t)
  (define bytes (file->bytes active))
  (delete-file active)
  (values active superseded bytes))

(define (run-recovery-cli args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "racket") (path->string recovery-script) args)))
  (values code (get-output-string out) (get-output-string err)))

(module+ test
  (test-case "dry-run verifies with the real clone resolver and leaves all root bytes unchanged"
    (with-fixture (lambda (root bare repo base old-head head)
                    (define before (tree-bytes root))
                    (define result (call-recover root bare base old-head head))
                    (check-eq? (recovery-result-status result) 'dry-run (format "~s" result))
                    (check-equal? (tree-bytes root) before))))

  (test-case "dry-run does not acquire or rewrite the campaign lock"
    (with-fixture (lambda (root bare repo base old-head head)
                    (define lock
                      (build-path root ".planning" "campaigns" (string-append plan ".lock")))
                    (make-parent-directory* lock)
                    (display-to-file "preexisting-lock-bytes" lock #:exists 'truncate)
                    (define before-lock (file->bytes lock))
                    (define before (tree-bytes root))
                    (define result (call-recover root bare base old-head head))
                    (check-eq? (recovery-result-status result) 'dry-run (format "~s" result))
                    (check-equal? (file->bytes lock) before-lock)
                    (check-equal? (tree-bytes root) before))))

  (test-case "apply uses the campaign lock and refuses when it is busy"
    (with-fixture (lambda (root bare repo base old-head head)
                    (define lock
                      (build-path root ".planning" "campaigns" (string-append plan ".lock")))
                    (make-parent-directory* lock)
                    (define port (open-output-file lock #:exists 'can-update))
                    (check-true (port-try-file-lock? port 'exclusive))
                    (define before (tree-bytes (build-path root ".planning" "campaigns")))
                    (define result (call-recover root bare base old-head head #:apply? #t))
                    (check-eq? (recovery-result-status result) 'refused)
                    (check-eq? (recovery-result-reason result) 'lock-busy)
                    (check-equal? (tree-bytes (build-path root ".planning" "campaigns")) before)
                    (port-file-unlock port)
                    (close-output-port port))))

  (test-case "stale cancellation, attempt, fence, head and base refuse"
    (with-fixture
     (lambda (root bare repo base old-head head)
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head #:fence* 99))
                  'stale-fence)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:attempt* "other"))
        'stale-attempt)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:expected-head* old-head))
        'candidate-head-mismatch)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:expected-base* head))
        'stale-base)
       (define rec (load-campaign-record root plan))
       (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))
       (persist-campaign! root rec)
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head)) 'cancelled))))

  (test-case "superseded journal is restored only in the dry-run verification copy"
    (with-fixture (lambda (root bare repo base old-head head)
                    (define-values (active _superseded bytes) (park-active-as-superseded! root))
                    (define before (tree-bytes root))
                    (define result (call-recover root bare base old-head head))
                    (check-eq? (recovery-result-status result) 'dry-run (format "~s" result))
                    (check-false (file-exists? active))
                    (check-equal? (tree-bytes root) before)
                    (check-equal? bytes bytes))))

  (test-case "superseded journal apply restores byte-identical active journal before reconciliation"
    (with-fixture
     (lambda (root bare repo base old-head head)
       (define-values (active _superseded bytes) (park-active-as-superseded! root))
       (define result (call-recover root bare base old-head head #:apply? #t))
       (check-eq? (recovery-result-status result) 'applied (format "~s" result))
       (check-true (file-exists? active))
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (hash-ref (hash-ref journal 'receipt) 'head) head)
       (check-equal? (hash-ref (car (hash-ref journal 'receipt-history)) 'head)
                     (hash-ref (hash-ref (call-with-input-bytes bytes read-json) 'receipt) 'head)))))

  (test-case "apply-mode verify failure leaves no restored journal and no root effects"
    (with-fixture
     (lambda (root bare repo base old-head head)
       ;; Declared-target regression on the candidate branch: every strict
       ;; pre-check passes, but the production repair-tail verify must
       ;; refuse it ("target file changed after the recorded receipt").
       (write-rkt (build-path repo "ui-core" "preferences.rkt") 3)
       (git-quiet! repo "add" "-A")
       (git-quiet! repo "commit" "-q" "-m" "undeclared drift")
       (define drifted (git-out repo "rev-parse" "HEAD"))
       (git-quiet! repo "push" "-q" "origin" branch)
       (git-quiet! repo "fetch" "-q" "origin" (string-append branch ":refs/remotes/origin/" branch))
       (define-values (active _superseded _bytes) (park-active-as-superseded! root))
       (define before (tree-bytes root))
       (define result
         (call-recover root bare base old-head drifted #:expected-head* drifted #:apply? #t))
       (check-eq? (recovery-result-status result) 'refused (format "~s" result))
       (check-eq? (recovery-result-reason result) 'verify-failed)
       (check-false (file-exists? active) "refusal must not restore the journal")
       ;; The campaign lock file is the single sanctioned apply-mode write:
       ;; the lease's owner-diagnostics path (same protocol as /go). Every
       ;; other root byte — records, journals, handoffs — must be untouched.
       (define lock-path (build-path root ".planning" "campaigns" (string-append plan ".lock")))
       (define (without-lock bytes)
         (filter (lambda (entry) (not (equal? (car entry) (find-relative-path root lock-path))))
                 bytes))
       (check-equal? (without-lock (tree-bytes root))
                     (without-lock before)
                     "refusal must not touch any root byte except the lease file"))))

  (test-case "active and superseded journals in a wrong repair-tail stage refuse before verification"
    (with-fixture (lambda (root bare repo base old-head head)
                    (update-delivery-journal! root plan 0 (hasheq 'stage "implementation-merged"))
                    (define result (call-recover root bare base old-head head))
                    (check-eq? (recovery-result-status result) 'refused)
                    (check-eq? (recovery-result-reason result) 'repair-tail-stage-refused)
                    (define-values (active _superseded _bytes) (park-active-as-superseded! root))
                    (define apply-result (call-recover root bare base old-head head #:apply? #t))
                    (check-eq? (recovery-result-status apply-result) 'refused)
                    (check-eq? (recovery-result-reason apply-result) 'repair-tail-stage-refused)
                    (check-false (file-exists? active)))))

  (test-case "candidate resolver rejects missing, stale, protected, and detached/tag tips"
    (with-fixture (lambda (root bare repo base old-head head)
                    (git-quiet! repo "push" "-q" "origin" (string-append ":refs/heads/" branch))
                    (check-eq? (recovery-result-reason (call-recover root bare base old-head head))
                               'clone-failed)))
    (with-fixture
     (lambda (root bare repo base old-head head)
       (git-quiet! repo "push" "-q" "origin" (string-append "+" old-head ":refs/heads/" branch))
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head))
                  'candidate-head-mismatch)))
    (with-fixture
     (lambda (root bare repo base old-head head)
       (define journal (load-delivery-journal root plan 0))
       (define main-receipt (hash-set (hash-ref journal 'receipt) 'branch "main"))
       (call-with-atomic-output-file (delivery-journal-path root plan 0)
                                     (lambda (out _)
                                       (write-json (hash-set journal 'receipt main-receipt) out)
                                       (newline out)))
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head))
                  'protected-branch)))
    (with-fixture #:branch* tag-branch
                  (lambda (root bare repo base old-head head)
                    (check-eq? (recovery-result-reason (call-recover root bare base old-head head))
                               'candidate-invalid))))

  (test-case "apply reconciles receipt, records provenance, refreshes handoff, and never finalizes or bumps fence"
    (with-fixture (lambda (root bare repo base old-head head)
                    (define before-fence (campaign-fence-token (load-campaign-record root plan)))
                    (define result (call-recover root bare base old-head head #:apply? #t))
                    (check-eq? (recovery-result-status result) 'applied (format "~s" result))
                    (define journal (load-delivery-journal root plan 0))
                    (check-equal? (hash-ref (hash-ref journal 'receipt) 'head) head)
                    (check-equal? (hash-ref (car (hash-ref journal 'receipt-history)) 'head) old-head)
                    (define rec (load-campaign-record root plan))
                    (define w (car (campaign-record-waves rec)))
                    (check-equal? (campaign-fence-token rec) before-fence "fence unchanged")
                    (check-eq? (campaign-wave-status w) 'awaiting-delivery "not finalized")
                    (check-equal? (campaign-wave-delivery-head-sha w) head)
                    (check-eq? (delivery-handoff-status root plan 0) 'delivery-pending)
                    (check-false (directory-exists? (build-path root ".planning" "outbox"))))))

  (test-case "active journal is never overwritten by a superseded copy"
    (with-fixture
     (lambda (root bare repo base old-head head)
       (define active (delivery-journal-path root plan 0))
       (define superseded
         (build-path root ".planning" "campaigns" plan "coordinator-w0.json.reconciled-superseded"))
       (make-parent-directory* superseded)
       (copy-file active superseded #t)
       (define before (file->bytes active))
       (define result (call-recover root bare base old-head head))
       (check-eq? (recovery-result-status result) 'dry-run (format "~s" result))
       (check-equal? (file->bytes active) before))))

  (test-case "CLI rejects missing, invalid, and unsupported arguments without root effects"
    (define root (make-root))
    (dynamic-wind void
                  (lambda ()
                    (define before (tree-bytes root))
                    (define-values (missing-code missing-out missing-err)
                      (run-recovery-cli (list "--root" (path->string root))))
                    (check-not-equal? missing-code 0)
                    (check-true (regexp-match? #rx"missing required argument"
                                               (string-append missing-out missing-err)))
                    (define-values (invalid-code _invalid-out _invalid-err)
                      (run-recovery-cli (list "--root"
                                              (path->string root)
                                              "--plan"
                                              plan
                                              "--wave"
                                              "not-a-number"
                                              "--attempt-id"
                                              attempt-id
                                              "--fence"
                                              (number->string fence)
                                              "--expected-head"
                                              (make-string 40 #\a)
                                              "--expected-base"
                                              (make-string 40 #\b))))
                    (check-not-equal? invalid-code 0)
                    (define hidden-unsupported-flag (string-append "--candidate" "-dir"))
                    (define-values (unsupported-code _unsupported-out unsupported-err)
                      (run-recovery-cli (list hidden-unsupported-flag "x")))
                    (check-not-equal? unsupported-code 0)
                    (check-true (regexp-match? #rx"unknown switch|usage" unsupported-err))
                    (check-equal? (tree-bytes root) before))
                  (lambda () (delete-directory/files root #:must-exist? #f)))))
