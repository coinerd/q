#lang racket/base
;; E-owned fixture builders (milestone #899, wave WE) for
;; tests/test-gsd-delivery-recovery-contract.rkt — the required-PR
;; counterpart of tests/test-gsd-delivery-recovery.rkt (now the slow deep
;; owner). It deliberately shares NO mutable surface with the slow owner's
;; inline builders (the slow owner keeps its own copies); everything
;; security-relevant stays on the real production path:
;;   * real Git repositories (bare origin + private candidate whose logical
;;     GitHub origin is re-pointed at the local bare remote via url.insteadOf,
;;     the same transport the slow owner uses);
;;   * real campaign record/fence/attempt/journal state (no fixture doubles);
;;   * the REAL clone resolver, REAL repair-tail/full candidate verify and
;;     REAL apply reconciliation inside `recover-delivery!` — honest
;;     green/RED exit-truth, never a mock-only security decision.
(require racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (only-in "private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         (only-in "delivery-fixtures.rkt" write-plan! write-wave-doc! write-state!)
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-recovery.rkt")

(provide recovery-manifest
         recovery-plan
         recovery-attempt-id
         recovery-fence
         recovery-branch
         recovery-origin-url
         git-out
         write-rkt
         make-root
         make-candidate
         receipt
         seed-campaign!
         tree-bytes
         with-git-rewrite
         call-recover
         with-fixture
         park-active-as-superseded!
         delivery-journal-active-path
         campaign-lock-path)

(define recovery-manifest (make-campaign-manifest 1 "recovery" '() '() "constraints"))
(define recovery-plan (campaign-manifest-hash recovery-manifest))
(define recovery-attempt-id "attempt-5")
(define recovery-fence 5)
(define recovery-branch "campaign/recovery-w0")
(define recovery-origin-url "https://github.com/example/q")

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
  (git-quiet! repo "config" "remote.origin.url" recovery-origin-url)
  (git-quiet! repo "config" (string-append "url." (file-url bare) ".insteadOf") recovery-origin-url)
  (write-rkt (build-path repo "ui-core" "preferences.rkt") 1)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "base")
  (define base (git-out repo "rev-parse" "HEAD"))
  (git-quiet! repo "checkout" "-q" "-b" recovery-branch)
  (write-rkt (build-path repo "ui-core" "preferences.rkt") 2)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "target delivery")
  (define old-head (git-out repo "rev-parse" "HEAD"))
  (display-to-file "repair\n" (build-path repo "repair.rkt") #:exists 'truncate)
  (git-quiet! repo "add" "-A")
  (git-quiet! repo "commit" "-q" "-m" "repair tail")
  (define head (git-out repo "rev-parse" "HEAD"))
  (git-quiet! repo "push" "-q" "origin" recovery-branch)
  (git-quiet! repo
              "fetch"
              "-q"
              "origin"
              (string-append recovery-branch ":refs/remotes/origin/" recovery-branch))
  (values bare repo base old-head head))

(define (receipt repo old-head)
  (hasheq 'repo
          (path->string (path->directory-path (path->complete-path repo)))
          'branch
          recovery-branch
          'head
          old-head
          'tree
          (git-out repo "rev-parse" (string-append old-head "^{tree}"))
          'origin
          recovery-origin-url
          'verified-at
          0
          'evidence
          "old verify"
          'attempt-id
          recovery-attempt-id
          'attempt-fence
          recovery-fence))

(define (seed-campaign! root
                        repo
                        old-head
                        #:status [status 'awaiting-delivery]
                        #:cancel? [cancel? #f]
                        #:stage [stage "implementation-ci"])
  (define w
    (make-campaign-wave 0
                        "Recovery"
                        status
                        recovery-fence
                        (campaign-attempt recovery-attempt-id recovery-fence 0)))
  (define rec
    (make-campaign-record recovery-plan
                          recovery-manifest
                          (list w)
                          (and cancel? (make-campaign-cancellation "stop" 0))
                          recovery-fence
                          'test
                          0
                          0))
  (persist-campaign! root rec)
  (record-delivery-receipt! root recovery-plan 0 (receipt repo old-head))
  (update-delivery-journal! root recovery-plan 0 (hasheq 'stage stage))
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
  (environment-variables-set! env #"GIT_CONFIG_VALUE_0" (string->bytes/utf-8 recovery-origin-url))
  (parameterize ([current-environment-variables env]
                 [current-gsd-recovery-github-origin? (lambda (v) (equal? v recovery-origin-url))])
    (thunk)))

(define (call-recover root
                      bare
                      base
                      old-head
                      head
                      #:apply? [apply? #f]
                      #:fence* [fence* recovery-fence]
                      #:attempt* [attempt* recovery-attempt-id]
                      #:expected-head* [expected-head* head]
                      #:expected-base* [expected-base* base]
                      #:old-head* [old-head* old-head])
  (with-git-rewrite bare
                    (lambda ()
                      (recover-delivery! #:root root
                                         #:plan recovery-plan
                                         #:wave 0
                                         #:attempt-id attempt*
                                         #:fence fence*
                                         #:expected-head expected-head*
                                         #:expected-base expected-base*
                                         #:old-receipt-head old-head*
                                         #:apply? apply?))))

(define (with-fixture proc)
  (define root (make-root))
  (dynamic-wind void
                (lambda ()
                  (define-values (bare repo base old-head head) (make-candidate root))
                  (seed-campaign! root repo old-head)
                  (proc root bare repo base old-head head))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(define (park-active-as-superseded! root)
  (define active (delivery-journal-path root recovery-plan 0))
  (define superseded
    (build-path root
                ".planning"
                "campaigns"
                recovery-plan
                "coordinator-w0.json.reconciled-superseded"))
  (make-parent-directory* superseded)
  (copy-file active superseded #t)
  (define bytes (file->bytes active))
  (delete-file active)
  (values active superseded bytes))

(define (delivery-journal-active-path root)
  (delivery-journal-path root recovery-plan 0))

(define (campaign-lock-path root)
  (build-path root ".planning" "campaigns" (string-append recovery-plan ".lock")))
