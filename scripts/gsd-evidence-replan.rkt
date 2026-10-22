#!/usr/bin/env racket
#lang racket/base
;; scripts/gsd-evidence-replan.rkt — owner-only supplemental evidence replan
;; tool (BUG-0088 A2/C1 interface).
;;
;; TRUST BOUNDARY: this is the OPERATOR's tool, not a model/agent surface.
;; It retains inert proposals in the shared trusted-user default store
;; (~/.q/agent/supplemental-evidence), or an explicit embedding --authority-root
;; (absolute, mode 0700, outside project/control source), reads public
;; campaign state, and routes EVERY authorization through the existing
;; interactive approval broker (the trusted frontend). With no active
;; interactive channel it refuses honestly and exits 2 — there is no TTY,
;; stdin, policy or file-based grant path, and no serializable
;; `approved: true` authority is ever printed or accepted.
;;
;; Verbs (all record output is canonical JSON on stdout):
;;   propose    --campaign-root R --plan ID --wave N --repo DIR
;;              --test-path P --harness-path P --owner-paths "a,b,c"
;;              --baseline-head H --baseline-tree T
;;              Build the task proposal from authentic local facts and
;;              retain it. Retains only; approves nothing.
;;   propose-recovery --campaign-root R --plan ID --wave N --sha256 PARENT-T
;;                    --expires-at UNIX-SECONDS
;;              Retain one inert recovery child; never move the spent pointer.
;;   propose-disposition --campaign-root R --plan ID --wave N --sha256 PARENT-T
;;                       --expires-at UNIX-SECONDS
;;              Sole fixed amendment; inherit scope, retain inert v3 only.
;;   authorize  --stage task|candidate|supersession (plus the common args)
;;              Register the exact pending proposal digest + full immutable
;;              presentation on the active interactive channel, await a REAL
;;              opaque grant, consume it through the owner frontend, and pin
;;              the public pointer for the controller.
;;   inspect    --kind K --sha256 X   Print a retained record (public bytes).
;;   status     Print the public typed status of the campaign's task.
;;   revoke     --sha256 X            Retain + authorize a revocation.
(require racket/cmdline
         racket/file
         racket/format
         racket/path
         racket/port
         racket/string
         "../util/json/checksum.rkt"
         (only-in "../extensions/gsd/evidence-repair-format.rkt"
                  canonical-evidence-json
                  canonical-evidence-digest)
         (only-in "../extensions/gsd/campaign-repository.rkt" load-campaign-record)
         (only-in "../extensions/gsd/campaign-state.rkt" campaign-plan-id)
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-operator.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-controller.rkt"
                  build-supplemental-task-proposal!
                  propose-supplemental-recovery!
                  propose-supplemental-disposition!
                  load-supplemental-pointer
                  supplemental-operation-summary
                  write-supplemental-pointer!))

(define (refuse! message)
  (fprintf (current-error-port) "gsd-evidence-replan: ~a\n" message)
  (exit 2))
(define (emit value)
  (displayln (canonical-evidence-json value)))
(define (need args key)
  (hash-ref args
            (if (string? key)
                (string->symbol key)
                key)
            (lambda () (refuse! (format "missing required --~a" key)))))
(define (sha-arg args key)
  (define v (need args key))
  (unless (and (string? v) (= 64 (string-length v)) (regexp-match? #px"^[0-9a-f]+$" v))
    (refuse! (format "--~a must be a 64-hex digest" key)))
  v)
(define (wave-arg args)
  (define v (need args "wave"))
  (define n (string->number (format "~a" v)))
  (unless (and n (exact-nonnegative-integer? n))
    (refuse! "--wave must be a nonnegative integer"))
  n)
(define (absolute-dir! key)
  (simplify-path (string->path (need args key)) #f))

;; Common arguments. The default is shared with the trusted-user /go process.
;; An explicit override is for embeddings/tests that bind the runtime likewise;
;; it is never discovered from project settings or a campaign pointer.
(define args (make-hasheq))
(command-line
 #:program "gsd-evidence-replan"
 #:once-each [("--verb")
              v
              "propose|propose-recovery|propose-disposition|authorize|inspect|status|revoke"
              (hash-set! args 'verb v)]
 [("--authority-root")
  p
  "absolute private authority root (mode 0700)"
  (hash-set! args 'authority-root p)]
 [("--campaign-root") p "campaign base directory" (hash-set! args 'campaign-root p)]
 [("--plan") p "plan id (64-hex)" (hash-set! args 'plan p)]
 [("--wave") n "wave index" (hash-set! args 'wave n)]
 [("--repo") p "execution checkout path (propose)" (hash-set! args 'repo p)]
 [("--test-path") p "approved test file path" (hash-set! args 'test-path p)]
 [("--harness-path") p "approved helper file path" (hash-set! args 'harness-path p)]
 [("--owner-paths")
  p
  "comma-separated enumerated owner-generated paths"
  (hash-set! args 'owner-paths p)]
 [("--baseline-head") h "pinned baseline head (40-hex)" (hash-set! args 'baseline-head h)]
 [("--baseline-tree") t "pinned baseline tree (40-hex)" (hash-set! args 'baseline-tree t)]
 [("--kind") k "record kind (inspect)" (hash-set! args 'kind k)]
 [("--sha256")
  s
  "record digest / expected parent task proposal (propose-recovery)"
  (hash-set! args 'sha256 s)]
 [("--expires-at") s "fresh recovery expiry (Unix seconds)" (hash-set! args 'expires-at s)]
 [("--stage") s "task|candidate|supersession (authorize)" (hash-set! args 'stage s)])

(define (authority-root!)
  (define raw (hash-ref args 'authority-root (lambda () (current-supplemental-authority-root))))
  (unless (and (path-string? raw) (complete-path? raw))
    (refuse! "--authority-root must be an absolute path"))
  (when (and (not (hash-has-key? args 'authority-root)) (equal? (need args 'verb) "propose"))
    (initialize-supplemental-authority-root!))
  (unless (directory-exists? raw)
    (refuse! "authority root must exist; propose provisions the default private store"))
  (unless (= #o700 (bitwise-and #o7777 (file-or-directory-permissions raw 'bits)))
    (refuse! "--authority-root must have mode 0700"))
  (simplify-path raw #f))
(define (campaign!)
  (parameterize ([current-supplemental-authority-root (authority-root!)])
    (define root (need args 'campaign-root))
    (unless (directory-exists? root)
      (refuse! "--campaign-root must exist"))
    (values (string->path root) (sha-arg args "plan") (wave-arg args))))
(define (ref-kind! stage)
  (case stage
    [("task") "task-proposal"]
    [("candidate") "candidate-proposal"]
    [("supersession") "supersession-proposal"]
    [else (refuse! "--stage must be task|candidate|supersession")]))

;; Pending proposal discovery for authorize: task from the public pointer,
;; candidate/supersession from the protected operation's bound facts.
(define (pending-proposal-ref! base plan wave stage)
  (case stage
    [("task")
     (define pointer (load-supplemental-pointer base plan wave))
     (unless pointer
       (refuse! "no task proposal pointer; run propose first"))
     (hash-ref pointer 'task-proposal)]
    [else
     (define pointer (load-supplemental-pointer base plan wave))
     (unless pointer
       (refuse! "no supplemental pointer for this campaign"))
     (define operation
       (with-handlers ([exn:fail? (lambda (e) (refuse! (exn-message e)))])
         (parameterize ([current-supplemental-authority-root (authority-root!)])
           (load-supplemental-operation (hash-ref pointer 'task-approval)))))
     (unless operation
       (refuse! "no protected supplemental operation"))
     (define key (if (equal? stage "candidate") 'candidate-ref 'supersession-proposal-ref))
     (or (hash-ref (hash-ref operation 'facts) key #f)
         (refuse! (format "operation has no bound ~a fact yet" key)))]))

(case (need args 'verb)
  [("propose")
   (define-values (base plan wave) (campaign!))
   (unless (need args 'repo)
     (refuse! "propose requires --repo"))
   (define owner-paths
     (for/list ([p (in-list (string-split (or (hash-ref args 'owner-paths "") "") "," #:trim? #f))]
                #:when (not (equal? p "")))
       p))
   (define-values (tref task)
     (parameterize ([current-supplemental-authority-root (authority-root!)])
       (build-supplemental-task-proposal! base
                                          plan
                                          wave
                                          #:test-path (need args 'test-path)
                                          #:harness-path (need args 'harness-path)
                                          #:owner-paths owner-paths
                                          #:baseline-head (need args 'baseline-head)
                                          #:baseline-tree (need args 'baseline-tree))))
   (write-supplemental-pointer! base plan wave tref)
   (emit (hasheq 'retained "task-proposal" 'sha256 (hash-ref tref 'sha256)))]

  [("propose-recovery" "propose-disposition")
   (define-values (base plan wave) (campaign!))
   (for ([key '(repo test-path harness-path owner-paths baseline-head baseline-tree)])
     (when (hash-has-key? args key)
       (refuse! (format "~a does not accept --~a; scope is inherited from the parent"
                        (need args 'verb)
                        key))))
   (define expected-parent (sha-arg args "sha256"))
   (define expires-at (string->number (need args 'expires-at)))
   (unless (and (exact-positive-integer? expires-at) (> expires-at (current-seconds)))
     (refuse! "--expires-at must be a fresh positive Unix timestamp"))
   ;; The controller owns the campaign lease and retains only inert bytes.
   ;; In particular, do NOT point ordinary task authorization at this child.
   (define-values (tref task)
     (parameterize ([current-supplemental-authority-root (authority-root!)])
       ((if (equal? (need args 'verb) "propose-disposition")
            propose-supplemental-disposition!
            propose-supplemental-recovery!)
        base
        plan
        wave
        expected-parent
        expires-at)))
   (emit
    (hasheq
     'retained
     "task-proposal"
     'sha256
     (hash-ref tref 'sha256)
     'notice
     (if (equal? (need args 'verb) "propose-disposition")
         "Sole fixed disposition task/v3 requires exact keyboard authorization; root and parent stay spent."
         "One NEW additional dispatch requires exact recovery approval; old task stays spent.")
     'provider
     "Operator-selected; not pinned and not an availability or quota guarantee."))]

  [("authorize")
   (define-values (base plan wave) (campaign!))
   (define stage (need args 'stage))
   (parameterize ([current-supplemental-authority-root (authority-root!)])
     (unless (supplemental-operator-channel-available?)
       (refuse! (supplemental-operator-refusal)))
     (define ref (pending-proposal-ref! base plan wave stage))
     (when (and (equal? stage "task")
                (member (hash-ref
                         (hash-ref (hash-ref (load-supplemental-record ref) 'payload) 'task (hasheq))
                         'kind
                         #f)
                        '("supplemental-evidence-task/v2" "supplemental-evidence-task/v3")))
       (refuse! "recovery/disposition tasks require the exact keyboard owner authorization command"))
     (define grant (request-supplemental-approval! ref))
     (unless grant
       (refuse! "operator denied or the interactive approval was cancelled/timed out"))
     (define approval-ref (approve-supplemental-proposal! ref grant))
     (unless (and approval-ref (hash? approval-ref))
       (refuse! "approval grant could not be consumed for the exact proposal digest"))
     ;; Pin the pointer only for the task stage; candidate/supersession
     ;; approvals are bound inside the protected operation by the controller.
     (when (equal? stage "task")
       (write-supplemental-pointer! base
                                    plan
                                    wave
                                    (hash-ref (load-supplemental-pointer base plan wave)
                                              'task-proposal)
                                    approval-ref))
     (emit (hasheq 'approved-ref-kind
                   (hash-ref approval-ref 'kind)
                   'sha256
                   (hash-ref approval-ref 'sha256))))]

  [("inspect")
   (parameterize ([current-supplemental-authority-root (authority-root!)])
     (define kind (need args 'kind))
     (define ref (hasheq 'kind kind 'sha256 (sha-arg args "sha256")))
     (emit (load-supplemental-record ref)))]

  [("status")
   (define-values (base plan wave) (campaign!))
   (define summary
     (parameterize ([current-supplemental-authority-root (authority-root!)])
       (supplemental-operation-summary base plan wave)))
   (emit (or summary (hasheq 'state "none")))]

  [("revoke")
   (define-values (base plan wave) (campaign!))
   (parameterize ([current-supplemental-authority-root (authority-root!)])
     (define target (hasheq 'kind (need args 'kind) 'sha256 (sha-arg args "sha256")))
     (unless (supplemental-operator-channel-available?)
       (refuse! (supplemental-operator-refusal)))
     (define revocation
       (propose-retain! "revocation-proposal" (hasheq 'reason "owner revocation") (list target)))
     (define grant (request-supplemental-approval! revocation))
     (unless grant
       (refuse! "operator denied or the interactive revocation was cancelled/timed out"))
     (define revoked (revoke! revocation grant))
     (emit (hasheq 'revoked (hash-ref revoked 'sha256))))]

  [else
   (refuse!
    "--verb must be propose|propose-recovery|propose-disposition|authorize|inspect|status|revoke")])
