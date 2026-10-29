#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 7200
;; One real C0 ladder; all mutations below are isolated, negative inputs.
;; No fabricated positive proof, private record rewrite, or install claim.
;; Compile gate plus isolated manual matrix run (deadline 7200):
;; timeout 7200 racket tests/test-gsd-supplemental-disposition-eligibility.rkt
;; Redirect stdout/stderr to /tmp/bug88-disposition-eligibility-green.log.
(require json
         racket/runtime-path
         rackunit
         rackunit/text-ui
         racket/system
         "helpers/supplemental-disposition-fixture.rkt"
         "helpers/supplemental-go-fixture.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" ancestry-support)
         (only-in "../extensions/gsd/review-proof-format.rkt" valid-owned-proof-manifest?))

;; Source-anchored CLI path: the dispatcher lives at the repository root,
;; never under tests/. A plain relative string would resolve against this
;; file's directory and break the real subprocess-free CLI exercise.
(define-runtime-path cli-dispatcher "../scripts/gsd-evidence-replan.rkt")

(define (files root)
  (for/hash ([p (in-list (find-files file-exists? root))])
    (values p (file->bytes p))))
(define (git repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" repo args)))
  (unless (zero? code)
    (error 'eligibility "git ~s: ~a" args (get-output-string err)))
  (string-trim (get-output-string out)))
(define (put-json! path value)
  (call-with-output-file path (lambda (out) (write-json value out)) #:exists 'truncate))
;; Restore even when a rackunit assertion or the owner API raises.
(define (with-file-change path change thunk)
  (define before (file->bytes path))
  (dynamic-wind (lambda () (change path))
                thunk
                (lambda () (display-to-file before path #:exists 'truncate))))

(define eligibility-suite
  (test-suite "disposition owner eligibility is inert on refusal"
    (test-case "proposal AND genuine-approved install reauthenticate every mutable input"
      (call-with-disposition-fixture
       (lambda (df)
         (define fx (disposition-fixture-fx df))
         (define base (supplemental-fixture-root fx))
         (define repo (supplemental-fixture-repo fx))
         (define plan (supplemental-fixture-plan fx))
         (define private (supplemental-fixture-private fx))
         (define parent (disposition-fixture-parent-pointer df))
         (define allocation (hash-ref (disposition-fixture-amendment df) 'allocation))
         (define parent-sha (hash-ref (hash-ref parent 'task-proposal) 'sha256))
         (define pointer (supplemental-pointer-path base plan 1))
         (define campaign-path (build-path base ".planning" "campaigns" (string-append plan ".rktd")))
         (define journal (delivery-journal-path base plan 1))
         (define frozen (build-path base ".planning" "campaigns" plan "plan-snapshot"))
         (define source (build-path repo "runtime/local.rkt"))
         (define (wave rec)
           (findf (lambda (w) (= 1 (campaign-wave-index w))) (campaign-record-waves rec)))
         (define (propose [target base])
           (propose-supplemental-disposition! target plan 1 parent-sha (+ (current-seconds) 14400)))
         ;; Fail closed here if the real fixture is not eligible. Do not weaken
         ;; production policy or substitute literal attempt/fence allocations.
         (define-values (ref task) (propose))
         ;; Exercise the actual CLI dispatcher with the same isolated authority
         ;; binding. It must retain amendment-3, never select an ancestor.
         (define cli-output (open-output-string))
         (parameterize ([current-command-line-arguments (vector "--verb"
                                                                "propose-disposition"
                                                                "--authority-root"
                                                                (path->string private)
                                                                "--campaign-root"
                                                                (path->string base)
                                                                "--plan"
                                                                plan
                                                                "--wave"
                                                                "1"
                                                                "--sha256"
                                                                parent-sha
                                                                "--expires-at"
                                                                (number->string (+ (current-seconds)
                                                                                   14400)))]
                        [current-output-port cli-output])
           (dynamic-require cli-dispatcher #f))
         (define cli-result (read-json (open-input-string (get-output-string cli-output))))
         (define cli-proposal
           (load-supplemental-record (hasheq 'kind
                                             "task-proposal"
                                             'sha256
                                             (string->immutable-string (hash-ref cli-result
                                                                                 'sha256)))))
         (check-equal? (hash-ref (hash-ref (hash-ref (hash-ref cli-proposal 'payload) 'task)
                                           'disposition)
                                 'amendment-id)
                       (hash-ref (disposition-fixture-amendment df) 'sha256))
         (define approval (fixture-approve! ref))
         (define initial-private (files private))
         (define initial-pointer (file->bytes pointer))
         (define initial-campaign (file->bytes campaign-path))
         (define initial-rec (load-campaign-record base plan))
         (define a (campaign-wave-current-attempt (wave initial-rec)))
         (check-equal? (campaign-attempt-id a) (hash-ref allocation 'attempt-id))
         (check-equal? (campaign-attempt-fence-token a) (hash-ref allocation 'attempt-fence))
         (check-equal? (campaign-fence-token initial-rec) (hash-ref allocation 'coordinator-fence))
         ;; A fresh v3 approval has no claim: the reconciling loader refuses it
         ;; before reading an operation. Inspect paths, never use it as a peek.
         (define (check-no-operation!)
           (for ([suffix '(".json" ".intent.json" ".lock")])
             (define p
               (build-path private
                           "supplemental-operations"
                           (string-append (hash-ref approval 'sha256) suffix)))
             (check-false (or (file-exists? p) (directory-exists? p) (link-exists? p))
                          (path->string p))))
         (check-no-operation!)
         ;; Capture AFTER mutation, independently for each API invocation. Comparing
         ;; the whole private tree catches claims, new operations/proposals, and
         ;; byte changes to every historical v1/v2 record, not merely pointer drift.
         (define (refused! label thunk)
           (define private-before (files private))
           (define pointer-link-before (link-exists? pointer))
           (define public-before
             (for/hash ([p (in-list (list pointer campaign-path journal source))])
               (values p (file->bytes p))))
           (define frozen-before (files frozen))
           (define accounting-before (file->bytes campaign-path))
           (define proofs-before (files (current-owned-proof-root)))
           (define (physical-state)
             (for/list ([args '(("rev-parse" "HEAD") ("diff" "--cached" "--raw")
                                                     ("rev-parse" "--abbrev-ref" "HEAD")
                                                     ("remote" "get-url" "origin")
                                                     ("status" "--porcelain"
                                                               "--untracked-files=all"))])
               (apply git repo args)))
           (define physical-before (physical-state))
           (with-check-info (['mutation label])
                            (check-exn exn:fail? thunk)
                            (check-equal? (files private)
                                          private-before
                                          "no claim, retention, operation or old-record write")
                            (check-equal? (files frozen) frozen-before)
                            (check-equal? (files (current-owned-proof-root)) proofs-before)
                            (check-equal? (physical-state) physical-before)
                            (check-equal? (link-exists? pointer) pointer-link-before)
                            (for ([(p b) (in-hash public-before)])
                              (check-equal? (file->bytes p) b (path->string p)))
                            ;; The persisted bytes include current allocation, attempt count,
                            ;; cancellation, budget pause and all usage accounting fields.
                            (check-equal? (file->bytes campaign-path) accounting-before)))
         (refused! "old predecessor parent proposal install (production f4238535 analogue)"
                   (lambda ()
                     ;; The exhausted amendment-2 parent pair is neither a fresh
                     ;; child nor the active amendment's parent: both the active
                     ;; policy mismatch and the predecessor's permanent claim
                     ;; independently refuse it.
                     (install-supplemental-disposition!
                      base
                      plan
                      1
                      (hash-ref (disposition-predecessor) 'parent-proposal)
                      (hash-ref (disposition-predecessor) 'parent-approval))))
         (refused! "grand-predecessor v2 parent proposal install"
                   (lambda ()
                     (install-supplemental-disposition!
                      base
                      plan
                      1
                      (hash-ref (cadr (disposition-ancestors)) 'parent-proposal)
                      (hash-ref (cadr (disposition-ancestors)) 'parent-approval))))
         (define (both! label [target base])
           (refused! (string-append label "/proposal") (lambda () (propose target)))
           (refused! (string-append label "/install")
                     (lambda () (install-supplemental-disposition! target plan 1 ref approval))))
         (define (campaign-case label mutate)
           (list label
                 (lambda (thunk)
                   (with-file-change campaign-path
                                     (lambda (_)
                                       (define rec (load-campaign-record base plan))
                                       (mutate rec (wave rec))
                                       (persist-campaign! base rec))
                                     thunk))))
         (define (file-case label path change)
           (list label (lambda (thunk) (with-file-change path change thunk))))
         (define origin (git repo "remote" "get-url" "origin"))
         (define branch (git repo "symbolic-ref" "--short" "HEAD"))
         (define (git-case label enter leave)
           (list label (lambda (thunk) (dynamic-wind enter thunk leave))))
         (define cases
           (list
            (campaign-case "current attempt id"
                           (lambda (r w)
                             (define old (campaign-wave-current-attempt w))
                             (set-campaign-wave-current-attempt!
                              w
                              (campaign-attempt (string-append (campaign-attempt-id old) "-changed")
                                                (campaign-attempt-fence-token old)
                                                0))))
            (campaign-case "attempt fence"
                           (lambda (r w)
                             (define old (campaign-wave-current-attempt w))
                             (set-campaign-wave-current-attempt!
                              w
                              (campaign-attempt (campaign-attempt-id old)
                                                (add1 (campaign-attempt-fence-token old))
                                                0))))
            (campaign-case "coordinator fence"
                           (lambda (r w)
                             (set-campaign-fence-token! r (add1 (campaign-fence-token r)))))
            (campaign-case "cancelled"
                           (lambda (r w)
                             (set-campaign-cancellation!
                              r
                              (make-campaign-cancellation "negative eligibility" (current-seconds)))))
            ;; This exact current failed record stays paused/exhausted throughout
            ;; BOTH calls. No resume or automatic budget reset is involved.
            (campaign-case "exhausted budget pause"
                           (lambda (r w)
                             (stamp-wave-usage! r 1 (usage-datum 1 1 2 #f #f))
                             (define pause (budget-pause-violation? r #f 1))
                             (check-true (campaign-budget-pause? pause))
                             (pause-campaign-for-budget! r pause)))
            (campaign-case "not failed" (lambda (r w) (set-campaign-wave-status! w 'interrupted)))
            (file-case "dirty physical source"
                       source
                       (lambda (p)
                         (display-to-file "\n;; negative dirty input\n" p #:exists 'append)))
            (git-case "physical branch drift"
                      (lambda () (git repo "checkout" "-qb" "negative-disposition"))
                      (lambda ()
                        (git repo "checkout" "-q" branch)
                        (git repo "branch" "-D" "negative-disposition")))
            (git-case "physical origin drift"
                      (lambda ()
                        (git repo "remote" "set-url" "origin" "https://invalid.example/negative.git"))
                      (lambda () (git repo "remote" "set-url" "origin" origin)))
            (git-case "physical head drift"
                      (lambda () (git repo "checkout" "-q" "--detach" "HEAD^"))
                      (lambda () (git repo "checkout" "-q" branch)))
            (git-case "physical index tree drift"
                      (lambda () (git repo "read-tree" "HEAD^"))
                      (lambda () (git repo "read-tree" "HEAD")))
            (file-case
             "invalid receipt"
             journal
             (lambda (p)
               (define j (call-with-input-file p read-json))
               (put-json!
                p
                (hash-set j 'receipt (hash-set (hash-ref j 'receipt) 'head (make-string 40 #\0))))))
            (file-case
             "frozen snapshot file tamper"
             (car (find-files (lambda (p) (and (file-exists? p) (regexp-match? #rx"[.]md$" p)))
                              frozen))
             (lambda (p) (display-to-file "\nnegative snapshot tamper\n" p #:exists 'append)))
            (file-case "spent v2 pointer cannot select amendment-1"
                       pointer
                       (lambda (p)
                         (define a (disposition-fixture-amendment df))
                         (put-json! p
                                    (hasheq 'schema
                                            "supplemental-pointer/v1"
                                            'task-proposal
                                            (hash-ref a 'v2-proposal)
                                            'task-approval
                                            (hash-ref a 'v2-approval)))))
            (file-case "spent v3 pointer cannot select amendment-2"
                       pointer
                       (lambda (p)
                         (define a (disposition-fixture-amendment df))
                         (put-json! p
                                    (hasheq 'schema
                                            "supplemental-pointer/v1"
                                            'task-proposal
                                            (hash-ref a 'prior-parent-proposal)
                                            'task-approval
                                            (hash-ref a 'prior-parent-approval)))))
            (file-case "alternate pointer contents"
                       pointer
                       (lambda (p) (put-json! p (disposition-fixture-root-pointer df))))))
         (for ([entry (in-list cases)])
           ((second entry) (lambda () (both! (first entry)))))
         (define redirected-pointer (build-path base "negative-pointer.json"))
         (dynamic-wind (lambda ()
                         (rename-file-or-directory pointer redirected-pointer)
                         (make-file-or-directory-link redirected-pointer pointer))
                       (lambda () (both! "alternate pointer path via symlink"))
                       (lambda ()
                         (delete-file pointer)
                         (rename-file-or-directory redirected-pointer pointer)))
         ;; Corrupt proofs and a manifest with an invalid execution binding
         ;; are tested on COPIES, never by writing the protected proof store.
         ;; The latter retains the original content address, so rejection may be
         ;; at hash authentication (source unknown), not the physical-port branch.
         (define proof-copy (build-path base "negative-proof-copy"))
         (define proof-root (current-owned-proof-root))
         (copy-directory/files proof-root proof-copy)
         (define receipt (hash-ref (load-delivery-journal base plan 1) 'receipt))
         (define manifest
           (build-path proof-copy plan (hash-ref (hash-ref receipt 'owned-proof) 'path)))
         ;; Make only the disposable copy writable during edits. Every API sees
         ;; the ordinary immutable 0400 mode, including after restoration.
         (define (with-proof-change change thunk)
           (define before (file->bytes manifest))
           (dynamic-wind (lambda ()
                           (file-or-directory-permissions manifest #o600)
                           (change manifest)
                           (file-or-directory-permissions manifest #o400))
                         thunk
                         (lambda ()
                           (file-or-directory-permissions manifest #o600)
                           (display-to-file before manifest #:exists 'truncate)
                           (file-or-directory-permissions manifest #o400))))
         (parameterize ([current-owned-proof-root proof-copy])
           (with-proof-change (lambda (p) (display-to-file "invalid proof" p #:exists 'truncate))
                              (lambda () (both! "invalid owned proof")))
           (with-proof-change (lambda (p)
                                (define original (call-with-input-file p read-json))
                                (check-true (valid-owned-proof-manifest? original))
                                (define changed
                                  (hash-set original 'execution-cwd (path->string base)))
                                ;; The canonical command/job identity binds execution-cwd. Changing
                                ;; it alone must invalidate the manifest, not fabricate a proof.
                                (check-false (valid-owned-proof-manifest? changed))
                                ;; Owned manifests use ordinary JSON (including real timestamps),
                                ;; not the supplemental record canonicalizer's integer-only JSON.
                                (put-json! p changed))
                              (lambda () (both! "manifest execution binding tamper/source unknown"))))
         (define alternate (build-path base "alternate-base"))
         (make-directory alternate)
         (both! "alternate base/pointer path" alternate)
         (check-false (file-exists? (supplemental-pointer-path alternate plan 1)))
         ;; Restoration proves refusal did not spend the broker-approved T or
         ;; claim anything. Final ordinary proposal is still INERT (no install).
         (check-equal? (files private) initial-private)
         (check-equal? (file->bytes pointer) initial-pointer)
         (check-equal? (file->bytes campaign-path) initial-campaign)
         (define-values (restored-ref restored-task) (propose))
         (check-equal? restored-task task)
         (for ([(p bytes) (in-hash initial-private)])
           (check-equal? (file->bytes p) bytes (path->string p)))
         (check-equal? (file->bytes pointer) initial-pointer)
         (check-equal? (file->bytes campaign-path) initial-campaign)
         (check-no-operation!)
         ;; The live late checkpoint parks the spent attempt at
         ;; `awaiting-delivery`. That status is admitted ONLY for the exact
         ;; authenticated fixed third-generation late terminal already bound
         ;; above (blocked/revision-6; the amendment's attempt and fences are
         ;; the campaign's current ones). Admission never mutates the status.
         (with-file-change
          campaign-path
          (lambda (_)
            (define rec (load-campaign-record base plan))
            (set-campaign-wave-status! (wave rec) 'awaiting-delivery)
            (persist-campaign! base rec))
          (lambda ()
            (define awaiting-before (file->bytes campaign-path))
            (define-values (_awaiting-ref awaiting-task) (propose))
            (check-equal? awaiting-task task)
            (check-equal? (file->bytes campaign-path) awaiting-before)
            (check-equal? (campaign-wave-status (wave (load-campaign-record base plan)))
                          'awaiting-delivery)
            ;; Every other eligibility guard still applies under
            ;; awaiting-delivery: a changed current attempt
            ;; fence refuses before any retention or claim.
            (with-file-change campaign-path
                              (lambda (_)
                                (define rec (load-campaign-record base plan))
                                (define w (wave rec))
                                (define old (campaign-wave-current-attempt w))
                                (set-campaign-wave-current-attempt!
                                 w
                                 (campaign-attempt (campaign-attempt-id old)
                                                   (add1 (campaign-attempt-fence-token old))
                                                   0))
                                (persist-campaign! base rec))
                              (lambda () (check-exn exn:fail? (lambda () (propose)))))
            (check-equal? (file->bytes campaign-path) awaiting-before)))
         (check-equal? (campaign-usage-summary (load-campaign-record base plan))
                       (campaign-usage-summary initial-rec)))
       #:third? #t))))
(module+ main
  (exit (run-tests eligibility-suite)))
(module+ test
  (check-equal? (run-tests eligibility-suite) 0))
