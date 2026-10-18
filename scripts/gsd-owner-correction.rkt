#lang racket/base
;; Trusted operator entry only. Not registered as a runtime/model tool and
;; never called by /go. First inspect --propose, then explicitly --authorize
;; that exact digest, then separately --apply it. No ambient approval policy.
(require racket/cmdline racket/pretty
         "../extensions/gsd/owner-correction.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" acquire-lease release-lease!)
         (only-in "../extensions/gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide main)
(define (main [argv (current-command-line-arguments)])
  (when (current-gsd-author-phase) (error 'gsd-owner-correction "unavailable in author phase"))
  (define root #f) (define plan #f) (define wave #f)
  (define operation #f) (define digest #f)
  (parameterize ([current-command-line-arguments argv])
    (command-line
     #:program "gsd-owner-correction"
     #:once-each
     [("--root") v "Campaign root" (set! root v)]
     [("--plan") v "Frozen 64-hex plan id" (set! plan v)]
     [("--wave") v "Wave index" (set! wave (string->number v))]
     #:once-any
     [("--propose") "Inspect exact independently derived correction" (set! operation 'propose)]
     [("--authorize") v "Authorize this inspected proposal digest under BUG0087 directive"
      (set! operation 'authorize) (set! digest v)]
     [("--apply") v "Apply/replay this separately authorized digest (no Verify or /go)"
      (set! operation 'apply) (set! digest v)]))
  (unless (and root (string? plan) (regexp-match? #px"^[0-9a-f]{64}$" plan)
               (exact-nonnegative-integer? wave) operation)
    (error 'gsd-owner-correction "require --root, --plan, --wave and one explicit operation"))
  ;; The same exclusive OS lease used by /go is quiescence authority: no
  ;; concurrent owned author launch can be running or admitted while held.
  (define lease (acquire-lease root plan #:session-id "trusted-operator-BUG0087"))
  (unless lease (error 'gsd-owner-correction "campaign owner/author is live; lease unavailable"))
  (dynamic-wind
   void
   (lambda ()
     (define result
       (case operation
         [(propose) (propose-owner-correction! root plan wave #:session-live? (lambda () #f))]
         [(authorize) (authorize-owner-correction! root plan wave digest #:session-live? (lambda () #f))]
         [(apply) (apply-owner-correction! root plan wave digest #:session-live? (lambda () #f))]))
     (pretty-write result)
     result)
   (lambda () (release-lease! lease))))
(module+ main (main) (void))
