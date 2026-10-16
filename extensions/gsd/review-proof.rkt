#lang racket/base
;; Trusted owner capture and content-addressed retention. Hashes are integrity,
;; not signatures: callers must protect the managed store from model writes.
(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         "review-proof-format.rkt"
         "delivery-verifier.rkt"
         "verification-job.rkt"
         "composition-root.rkt"
         "plan-snapshot.rkt"
         (only-in "plan-types.rkt" parse-wave-content)
         (only-in "wave-docs.rkt" parse-plan-index wave-index-entry-idx wave-index-entry-ref-path)
         (only-in "../../util/json/checksum.rkt" sha256-file sha256-string))
(provide current-owned-proof-root
         call-with-owned-proof-capture
         load-owned-review-proof
         receipt-owned-proof-blocker
         export-owned-review-proof!
         frozen-verify-binding
         owned-proof-capture-reference
         consume-owned-proof-refresh!
         validate-receipt-owned-proof!
         load-receipt-owned-proof
         run-owned-red-verification
         load-owned-red-proof
         owned-red-failure-summary
         find-owned-red-proofs
         export-owned-red-proof!
         (all-from-out "review-proof-format.rkt"))
;; Trusted embedding configuration, never a command/model supplied path.
(define current-owned-proof-root
  (make-parameter (build-path (find-system-path 'home-dir) ".q" "agent" "owned-review-proofs")))
(struct owned-proof-capture (reference used root command normalized))
(define (consume-owned-proof-refresh! capture ref)
  (and (owned-proof-capture? capture)
       (equal? ref (owned-proof-capture-reference capture))
       (not (unbox (owned-proof-capture-used capture)))
       (begin
         (load-owned-review-proof (owned-proof-capture-root capture)
                                  ref
                                  (hash-ref ref 'binding)
                                  #:command (owned-proof-capture-command capture)
                                  #:normalized-command (owned-proof-capture-normalized capture))
         (set-box! (owned-proof-capture-used capture) #t)
         #t)))
(define (refuse msg)
  (error 'review-proof "~a" msg))
(define (safe-path p)
  (define abs (simplify-path (path->complete-path p) #f))
  (for/fold ([part (build-path "/")]) ([piece (in-list (cdr (explode-path abs)))])
    (define next (build-path part piece))
    (when (link-exists? next)
      (refuse "symlinked proof/snapshot/log path"))
    next))
(define (outside! root cwd)
  (define rel
    (path->string (find-relative-path (path->directory-path (safe-path cwd)) (safe-path root))))
  (unless (or (absolute-path? (string->path rel)) (string-prefix? rel "../"))
    (refuse "managed proof store must be outside execution source")))
(define (store-root plan)
  (unless (proof-hex? plan 64)
    (refuse "invalid plan identity"))
  (safe-path (build-path (current-owned-proof-root) plan)))
;; Read-only frozen declaration authority; never accept a passing override.
(define (frozen-verify-binding base plan wave)
  (safe-path (snapshot-dir base plan))
  (define m (load-snapshot-manifest base plan))
  (unless m
    (refuse "missing frozen snapshot"))
  (unless (null? (snapshot-drift? base plan))
    (refuse "frozen declaration drift"))
  ;; BUG-0081: a W<n> filename is not authority for a custom arrow mapping.
  (define declared-path
    (for/first ([entry (in-list (parse-plan-index (file->string (build-path (snapshot-dir base plan)
                                                                            "PLAN.md"))))]
                #:when (= wave (wave-index-entry-idx entry)))
      (wave-index-entry-ref-path entry)))
  (define entries
    (filter (lambda (f)
              (if declared-path
                  (equal? declared-path (snapshot-file-path f))
                  (regexp-match? (regexp (format "^waves/W~a-" wave)) (snapshot-file-path f))))
            (plan-snapshot-manifest-files m)))
  (unless (= (length entries) 1)
    (refuse "ambiguous/missing frozen wave"))
  (define path (safe-path (build-path (snapshot-dir base plan) (snapshot-file-path (car entries)))))
  (define declaration (hash-ref (parse-wave-content (file->string path)) 'verify #f))
  (unless (and (string? declaration) (non-empty-string? declaration))
    (refuse "empty frozen Verify"))
  (define normalized (normalize-declared-verify declaration))
  (unless (non-empty-string? normalized)
    (refuse "frozen Verify has no executable command"))
  (hasheq 'snapshot-sha256 (snapshot-manifest-digest m) 'normalized-command normalized))
;; Atomic create-only publication. Existing addressed files must match; chmod
;; discourages accidents, while validation always rechecks bytes and paths.
(define (retain! root suffix writer)
  (safe-path root)
  (make-directory* root)
  (file-or-directory-permissions root #o700)
  (define tmp (make-temporary-file "retain-~a" #f root))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file tmp writer #:exists 'truncate #:mode 'binary)
                  (define sha (sha256-file tmp))
                  (define size (file-size tmp))
                  (define name (string-append sha suffix))
                  (define dest (safe-path (build-path root name)))
                  (cond
                    [(file-exists? dest)
                     (unless (and (= size (file-size dest)) (equal? sha (sha256-file dest)))
                       (refuse "content-address collision/tamper"))]
                    [else
                     (rename-file-or-directory tmp dest)
                     (file-or-directory-permissions dest #o400)])
                  (hasheq 'path name 'sha256 sha 'size size))
                (lambda ()
                  (when (file-exists? tmp)
                    (delete-file tmp)))))
(define (job-facts job fresh?)
  (hasheq 'id
          (symbol->string (verification-job-id job))
          'identity-key
          (verification-job-identity-key job)
          'pid
          (verification-job-pid job)
          'proc-start-ticks
          (verification-job-proc-start-ticks job)
          'start-ms
          (verification-job-start-ms job)
          'end-ms
          (verification-job-end-ms job)
          'state
          (symbol->string (verification-job-state job))
          'exit-code
          (verification-job-exit-code job)
          'reaped
          (verification-job-reaped? job)
          'fresh
          fresh?))
;; Failures and drift retain diagnostic bytes too, but NEVER receive an audit
;; reference/capability. This kind is not meaningful RED or successful Verify.
(define (retain-diagnostic! o plan attempt-id fence)
  (when (and (owned-verify-observation? o) (owned-verify-observation-fresh? o))
    (define root (store-root plan))
    (outside! root (owned-verify-observation-cwd o))
    (define job (owned-verify-observation-job o))
    (define raw (safe-path (verification-job-log-path job)))
    (define log-ref
      (retain! root
               ".log"
               (lambda (out)
                 (call-with-input-file raw (lambda (in) (copy-port in out)) #:mode 'binary))))
    (retain! root
             ".json"
             (lambda (out)
               (write-json (hasheq 'schema-version
                                   1
                                   'kind
                                   "declared-verify-terminal-diagnostic"
                                   'plan-id
                                   plan
                                   'wave
                                   (owned-verify-observation-wave o)
                                   'attempt-id
                                   attempt-id
                                   'attempt-fence
                                   fence
                                   'before
                                   (owned-verify-observation-before o)
                                   'after
                                   (owned-verify-observation-after o)
                                   'execution-cwd
                                   (owned-verify-observation-cwd o)
                                   'command
                                   (owned-verify-observation-command o)
                                   'command-sha256
                                   (verification-job-command-digest job)
                                   'job
                                   (job-facts job #t)
                                   'log
                                   log-ref)
                           out)
               (newline out)))))
(define (capture! observation base plan wave before attempt-id fence frozen expected-cwd)
  (unless (consume-owned-verify-observation! observation)
    (refuse "not a live unused owned observation"))
  (define job (owned-verify-observation-job observation))
  (define command (owned-verify-observation-command observation))
  (define cwd (owned-verify-observation-cwd observation))
  (unless (and (equal? cwd (path->string (simplify-path (path->complete-path expected-cwd))))
               (owned-verify-observation-fresh? observation)
               (owned-verify-observation-declared? observation)
               (equal? (owned-verify-observation-base observation)
                       (path->string (simplify-path (path->complete-path base))))
               (equal? (owned-verify-observation-wave observation) wave)
               (equal? before (owned-verify-observation-before observation))
               (equal? before (owned-verify-observation-after observation))
               (equal? frozen (frozen-verify-binding base plan wave))
               (equal? command (expand-project-base (hash-ref frozen 'normalized-command) base))
               (eq? (verification-job-state job) 'completed)
               (equal? (verification-job-exit-code job) 0)
               (verification-job-reaped? job)
               (verification-job-proc-start-ticks job))
    (refuse "stale/failed/override/drifted owned execution"))
  (define digest (sha256-string (string-join (list "/bin/sh" "-c" command) "\u0")))
  (unless (equal? digest (verification-job-command-digest job))
    (refuse "job command digest mismatch"))
  (define root (store-root plan))
  (outside! root cwd)
  (define raw (safe-path (verification-job-log-path job)))
  (define raw-sha (sha256-file raw))
  (define log-ref
    (retain! root
             ".log"
             (lambda (out)
               (call-with-input-file raw (lambda (in) (copy-port in out)) #:mode 'binary))))
  (unless (and (equal? raw-sha (hash-ref log-ref 'sha256)) (equal? raw-sha (sha256-file raw)))
    (refuse "raw log changed during retention"))
  (define binding
    (hash-set* before
               'logical-repository
               (proof-logical-repository (hash-ref before 'origin #f))
               'plan-id
               plan
               'wave
               wave
               'attempt-id
               attempt-id
               'attempt-fence
               fence
               'snapshot-sha256
               (hash-ref frozen 'snapshot-sha256)))
  (define manifest
    (hasheq 'schema-version
            1
            'kind
            "declared-verify-green"
            'verification-origin
            "owned-registry"
            'binding
            binding
            'before
            before
            'after
            (owned-verify-observation-after observation)
            'execution-cwd
            cwd
            'execution-base
            (owned-verify-observation-base observation)
            'operational-jobs
            (current-gsd-delivery-verify-jobs)
            'execution-worktree
            (and (current-gsd-delivery-branch-context)
                 (branch-delivery-context-ref (current-gsd-delivery-branch-context) 'worktree-path)
                 #t)
            'command
            command
            'argv
            (list "/bin/sh" "-c" command)
            'command-sha256
            digest
            'frozen-normalized-command
            (hash-ref frozen 'normalized-command)
            'log
            log-ref
            'job
            (job-facts job #t)))
  (unless (valid-owned-proof-manifest? manifest)
    (refuse "invalid owned manifest"))
  (define file-ref
    (retain! root
             ".json"
             (lambda (out)
               (write-json manifest out)
               (newline out))))
  (owned-proof-capture
   (hash-set* file-ref 'schema-version 1 'kind "declared-verify-green" 'binding binding)
   (box #f)
   root
   command
   (hash-ref frozen 'normalized-command)))
;; Returns two values. Failure to acquire proof does not relabel an ordinary
;; legacy Verify result; the receipt remains ineligible for proof audit.
(define (call-with-owned-proof-capture base plan wave before attempt-id fence thunk #:cwd cwd)
  (define proof #f)
  (define seen? #f)
  (define frozen
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (frozen-verify-binding base plan wave)))
  (define result
    (parameterize
        ([current-owned-verify-observer
          (lambda (o)
            (cond
              [seen? (set! proof #f)]
              [else
               (set! seen? #t)
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  (log-warning "owned proof withheld: ~a" (exn-message e))
                                  (with-handlers ([exn:fail? (lambda (d)
                                                               (log-warning
                                                                "diagnostic retention failed: ~a"
                                                                (exn-message d)))])
                                    (retain-diagnostic! o plan attempt-id fence)))])
                 (cond
                   [(and before frozen attempt-id (exact-nonnegative-integer? fence))
                    (set! proof (capture! o base plan wave before attempt-id fence frozen cwd))]
                   [else (retain-diagnostic! o plan attempt-id fence)]))]))])
      (thunk)))
  (values result proof))
(define (read-addressed root ref suffix)
  (unless (valid-proof-file-reference? ref suffix)
    (refuse "unsafe content-addressed reference"))
  (define path (safe-path (build-path root (hash-ref ref 'path))))
  (unless (and (file-exists? path)
               (= (file-size path) (hash-ref ref 'size))
               (equal? (sha256-file path) (hash-ref ref 'sha256)))
    (refuse "missing/tampered proof file"))
  path)
;; ROOT is the managed plan store, or an offline bundle directory. EXPECTED
;; must include every binding field, including frozen snapshot hash.
(define (load-owned-review-proof root ref expected #:command command #:normalized-command normalized)
  (unless (and (valid-owned-proof-reference? ref) (equal? (hash-ref ref 'binding) expected))
    (refuse "proof reference/live binding mismatch"))
  (define path (read-addressed root ref ".json"))
  (define m
    (call-with-input-file path
                          (lambda (in)
                            (define v (read-json in))
                            (unless (eof-object? (read-json in))
                              (refuse "trailing manifest data"))
                            v)))
  (unless (and (valid-owned-proof-manifest? m)
               (equal? (hash-ref m 'binding) expected)
               (equal? (hash-ref m 'command) command)
               (equal? (hash-ref m 'frozen-normalized-command) normalized))
    (refuse "manifest/command binding mismatch"))
  (read-addressed root (hash-ref m 'log) ".log")
  m)
;; Write boundary validator. Receipt readers can still inspect missing/legacy
;; proofs; newly written proof-bearing receipts must authenticate every byte.
(define (validate-receipt-owned-proof! receipt base plan wave)
  (load-receipt-owned-proof receipt base plan wave)
  (void))
(define (load-receipt-owned-proof receipt base plan wave)
  (define ref (hash-ref receipt 'owned-proof #f))
  (and ref
       (let ()
         (unless (proof-reference-matches-receipt? ref receipt)
           (refuse "receipt/proof mismatch"))
         (define frozen (frozen-verify-binding base plan wave))
         (define b (hash-ref ref 'binding))
         (unless (and (equal? (hash-ref b 'plan-id) plan)
                      (equal? (hash-ref b 'wave) wave)
                      (equal? (hash-ref b 'snapshot-sha256) (hash-ref frozen 'snapshot-sha256)))
           (refuse "receipt/frozen binding mismatch"))
         (define path (read-addressed (store-root plan) ref ".json"))
         (define m (call-with-input-file path read-json))
         (unless (valid-owned-proof-manifest? m)
           (refuse "invalid owned manifest"))
         (define normalized (hash-ref frozen 'normalized-command))
         (define command
           (expand-frozen-command normalized
                                  (hash-ref m 'execution-base)
                                  (hash-ref m 'execution-cwd)
                                  (hash-ref m 'execution-worktree)
                                  #:jobs (hash-ref m 'operational-jobs #f)))
         (load-owned-review-proof (store-root plan)
                                  ref
                                  b
                                  #:command command
                                  #:normalized-command normalized))))
(define (receipt-owned-proof-blocker receipt base plan wave #:command command)
  (define ref (and (hash? receipt) (hash-ref receipt 'owned-proof #f)))
  (cond
    [(not ref) 'missing-owned-proof]
    [else
     (with-handlers ([exn:fail? (lambda (_) 'invalid-owned-proof)])
       (unless (proof-reference-matches-receipt? ref receipt)
         (refuse "receipt identity mismatch"))
       (define frozen (frozen-verify-binding base plan wave))
       (define expected
         (hash-set* (for/hasheq ([k '(repo branch origin head tree)])
                      (values k (hash-ref receipt k)))
                    'logical-repository
                    (proof-logical-repository (hash-ref receipt 'origin #f))
                    'plan-id
                    plan
                    'wave
                    wave
                    'attempt-id
                    (hash-ref receipt 'attempt-id)
                    'attempt-fence
                    (hash-ref receipt 'attempt-fence)
                    'snapshot-sha256
                    (hash-ref frozen 'snapshot-sha256)))
       (load-owned-review-proof (store-root plan)
                                ref
                                expected
                                #:command command
                                #:normalized-command (hash-ref frozen 'normalized-command))
       #f)]))
;; Trusted export after validation; consumers publish these two portable files
;; into excluded validation directories. No head/generation rebinding occurs.
(define (export-owned-review-proof! root
                                    ref
                                    expected
                                    destination
                                    #:command command
                                    #:normalized-command normalized)
  (define m
    (load-owned-review-proof root ref expected #:command command #:normalized-command normalized))
  (safe-path destination)
  (for ([r (in-list (list (hash-ref m 'log) ref))]
        [suffix '(".log" ".json")])
    (define source (read-addressed root r suffix))
    (define exported
      (retain! destination
               suffix
               (lambda (out)
                 (call-with-input-file source (lambda (in) (copy-port in out)) #:mode 'binary))))
    (unless (equal? (hash-ref exported 'sha256) (hash-ref r 'sha256))
      (refuse "export hash mismatch")))
  ref)
;; ============================================================
;; Owned behavioral RED capture and validation
;; ============================================================
;; A genuine behavioral failure marker, as actually printed by the real
;; runners: raco test per-file summaries ("1/1 test failures"), rackunit
;; text-ui summaries ("N failure(s)") and rackunit check-failure blocks (a
;; line that is exactly FAILURE). Import/setup/compile errors produce none
;; of these and can never pass this check; a nonzero exit alone is not a
;; meaningful RED.
(define red-failure-marker-rx
  #px"(?m:^FAILURE$)|[1-9][0-9]*/[1-9][0-9]* test failures|[1-9][0-9]* failure\\(s\\)")
(define red-failure-summary-chars 600)
(define (log-shows-behavioral-failure? path)
  (call-with-input-file path (lambda (in) (regexp-match? red-failure-marker-rx in))))
(define (red-terminal-job? job fresh?)
  (and fresh?
       (eq? (verification-job-state job) 'failed)
       (exact-integer? (verification-job-exit-code job))
       (> (verification-job-exit-code job) 0)
       (not (= (verification-job-exit-code job) 124))
       (verification-job-reaped? job)))
;; Trusted owned RED producer: runs ONE declared focused stage of the frozen
;; Verify declaration through the singleton registry and retains an
;; authentic negative proof iff the job is a fresh, reaped, nonzero-EXIT
;; ASSERTION failure with the failure summary in the raw log. There is no
;; constructor from prose, model JSON, fixture templates or green receipts.
;; The live zero-argument guard (attempt/fence/cancellation) is mandatory.
(define (run-owned-red-verification base
                                    plan
                                    wave
                                    attempt-id
                                    fence
                                    cwd
                                    focused-normalized
                                    #:guard guard
                                    #:origin [origin "preimplementation"]
                                    #:timeout-sec [timeout-sec 600])
  (unless (and (string? attempt-id)
               (non-empty-string? attempt-id)
               (exact-nonnegative-integer? fence)
               (procedure? guard)
               (procedure-arity-includes? guard 0)
               (member origin '("preimplementation" "reproduction"))
               (owned-red-focused-command? focused-normalized))
    (refuse "invalid owned RED request"))
  (define base-dir (path->string (simplify-path (path->complete-path base))))
  (define cwd-dir (path->string (simplify-path (path->complete-path cwd))))
  (define frozen (frozen-verify-binding base plan wave))
  (define normalized (hash-ref frozen 'normalized-command))
  (unless (member focused-normalized (or (owned-chain-stages normalized) '()))
    (refuse "RED command is not a declared focused stage of the frozen Verify"))
  (define before (execution-snapshot cwd-dir))
  (unless before
    (refuse "owned RED requires a clean committed checkout"))
  (unless (guard)
    (refuse "live guard refused owned RED execution"))
  (define worktree?
    (and (current-gsd-delivery-branch-context)
         (branch-delivery-context-ref (current-gsd-delivery-branch-context) 'worktree-path)
         #t))
  (define command (expand-frozen-command focused-normalized base-dir cwd-dir worktree? #:jobs #f))
  (define reg (current-gsd-verification-registry))
  (define timeout-ms (* 1.0 timeout-sec 1000.0))
  (define identity
    (verification-identity (checkout-key base-dir)
                           (format "W~a" wave)
                           "delivery-red"
                           (checkout-key cwd-dir)
                           command))
  (define started
    (parameterize ([current-directory cwd-dir])
      (verification-start! reg identity "/bin/sh" (list "-c" command) #:timeout-ms timeout-ms)))
  (define job (verification-wait reg (start-result-job-id started) timeout-ms))
  (cond
    [(not (red-terminal-job? job (start-result-started? started))) (values job #f)]
    [(not (log-shows-behavioral-failure? (safe-path (verification-job-log-path job))))
     (values job #f)]
    [else
     (define root (store-root plan))
     (outside! root cwd-dir)
     (define raw (safe-path (verification-job-log-path job)))
     (define log-ref
       (retain! root
                ".log"
                (lambda (out)
                  (call-with-input-file raw (lambda (in) (copy-port in out)) #:mode 'binary))))
     (define binding
       (hash-set* before
                  'logical-repository
                  (proof-logical-repository (hash-ref before 'origin #f))
                  'plan-id
                  plan
                  'wave
                  wave
                  'attempt-id
                  attempt-id
                  'attempt-fence
                  fence
                  'snapshot-sha256
                  (hash-ref frozen 'snapshot-sha256)))
     (define manifest
       (hasheq 'schema-version
               1
               'kind
               "declared-red-failure"
               'verification-origin
               "owned-registry"
               'binding
               binding
               'before
               before
               'after
               (execution-snapshot cwd-dir)
               'execution-cwd
               cwd-dir
               'execution-base
               base-dir
               'execution-worktree
               worktree?
               'command
               command
               'focused-normalized-command
               focused-normalized
               'frozen-normalized-command
               normalized
               'red-origin
               origin
               'argv
               (list "/bin/sh" "-c" command)
               'command-sha256
               (sha256-string (string-join (list "/bin/sh" "-c" command) "\u0"))
               'log
               log-ref
               'job
               (job-facts job #t)))
     (unless (and (equal? (hash-ref manifest 'before) (hash-ref manifest 'after))
                  (valid-owned-red-manifest? manifest)
                  (equal? (verification-job-command-digest job) (hash-ref manifest 'command-sha256)))
       (refuse "invalid owned RED manifest"))
     (define file-ref
       (retain! root
                ".json"
                (lambda (out)
                  (write-json manifest out)
                  (newline out))))
     (values job
             (hash-set* file-ref 'schema-version 1 'kind "declared-red-failure" 'binding binding))]))
;; Offline-safe load of one red reference: structural laws, exact binding,
;; command/focused-stage identity AND a behavioral failure marker in the
;; retained raw bytes. Rejected kinds never load as RED.
(define (load-owned-red-proof root ref expected #:command command #:focused-normalized focused)
  (unless (and (valid-owned-red-reference? ref) (equal? (hash-ref ref 'binding) expected))
    (refuse "red reference/live binding mismatch"))
  (define path (read-addressed root ref ".json"))
  (define m
    (call-with-input-file path
                          (lambda (in)
                            (define v (read-json in))
                            (unless (eof-object? (read-json in))
                              (refuse "trailing red manifest data"))
                            v)))
  (unless (and (valid-owned-red-manifest? m)
               (equal? (hash-ref m 'binding) expected)
               (equal? (hash-ref m 'command) command)
               (equal? (hash-ref m 'focused-normalized-command) focused))
    (refuse "red manifest/command binding mismatch"))
  (unless (log-shows-behavioral-failure? (read-addressed root (hash-ref m 'log) ".log"))
    (refuse "retained red log carries no behavioral failure"))
  m)
;; Honest bounded excerpt of the ACTUAL retained raw red output, used as the
;; red-first failure fact. Never synthesized from prose or expectations.
(define (owned-red-failure-summary root manifest)
  (define path (read-addressed root (hash-ref manifest 'log) ".log"))
  (define data (file->bytes path))
  (define excerpt
    (if (> (bytes-length data) red-failure-summary-chars)
        (subbytes data (- (bytes-length data) red-failure-summary-chars))
        data))
  (string-trim (bytes->string/utf-8 excerpt #\?)))
;; All retained red manifests that honestly link to this green execution
;; (same plan/wave/live attempt/fence/spec, distinct source tree, declared
;; chronology the recorded job timestamps satisfy), newest first. Each
;; element is (list owner-addressed-reference manifest).
(define (find-owned-red-proofs root green)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (sort (for/fold ([linked '()])
                    ([name (in-list (directory-list root))]
                     #:when (equal? (filename-extension name) #"json"))
            (with-handlers ([exn:fail? (lambda (_) linked)])
              (define path (build-path root name))
              (define sha (sha256-file path))
              (cond
                [(not (equal? (path->string name) (string-append sha ".json"))) linked]
                [else
                 (define m (call-with-input-file path read-json))
                 (define binding (and (hash? m) (hash-ref m 'binding #f)))
                 (define ref
                   (and (hash? binding)
                        (hasheq 'path
                                (string-append sha ".json")
                                'sha256
                                sha
                                'size
                                (file-size path)
                                'schema-version
                                1
                                'kind
                                "declared-red-failure"
                                'binding
                                binding)))
                 (cond
                   [(not (and ref (valid-owned-red-reference? ref))) linked]
                   [else
                    (define validated
                      (load-owned-red-proof root
                                            ref
                                            binding
                                            #:command (hash-ref m 'command)
                                            #:focused-normalized
                                            (hash-ref m 'focused-normalized-command)))
                    (if (red-links-green-proof? validated green)
                        (cons (list ref validated) linked)
                        linked)])])))
          (lambda (a b)
            (> (hash-ref (hash-ref (second a) 'job) 'end-ms)
               (hash-ref (hash-ref (second b) 'job) 'end-ms))))))
;; Portable export of one validated red proof (manifest + raw log), the same
;; addressed-copy discipline as the green export. REF is the owner-recorded
;; addressed reference for this manifest (never model input).
(define (export-owned-red-proof! root manifest ref destination)
  (safe-path destination)
  (for ([r (in-list (list (hash-ref manifest 'log) ref))]
        [suffix '(".log" ".json")])
    (define source (read-addressed root r suffix))
    (define exported
      (retain! destination
               suffix
               (lambda (out)
                 (call-with-input-file source (lambda (in) (copy-port in out)) #:mode 'binary))))
    (unless (equal? (hash-ref exported 'sha256) (hash-ref r 'sha256))
      (refuse "red export hash mismatch")))
  ref)
