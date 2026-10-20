#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; Simulated trusted frontend drives the REAL broker (no grant constructor or
;; substitute guard). No live channel, proposal, campaign or authorization used.
(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/system
         "../runtime/approval/broker.rkt"
         (only-in "../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-json)
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         (only-in "../extensions/gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide authority-suite)

(define (with-root proc)
  (define workspace (make-temporary-file "supplemental-authority-~a" 'directory))
  (define root (build-path workspace "private"))
  (dynamic-wind (lambda ()
                  (make-directory root)
                  (file-or-directory-permissions root #o700))
                (lambda ()
                  (parameterize ([current-supplemental-authority-root root])
                    (proc root workspace)))
                (lambda () (delete-directory/files workspace))))
(define (path-for root ref)
  (build-path root (string-append (hash-ref ref 'sha256) ".json")))
(define (replace-bytes! path bytes)
  (call-with-output-file path (lambda (out) (write-bytes bytes out)) #:exists 'truncate))
(define owner-verbs (list approve-supplemental-proposal! revoke!))
(define (with-channel proc)
  (check-false (current-approval-channel) "tests must not replace a live frontend")
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 100)))
                proc
                clear-approval-channel!))
;; Test-only simulation of the trusted frontend, using the real active registry,
;; immutable presentation, exact decision and await. It cannot fabricate a grant.
(define (interactive-grant ref [approved? #t])
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel!
     (current-approval-channel)
     digest
     (hasheq 'proposal ref 'record (load-supplemental-record ref))))
  (check-not-false id)
  (check-equal? (canonical-evidence-json (hash-ref (approval-request-view id digest) 'proposal))
                (canonical-evidence-json ref))
  (check-false (approval-decide! id (make-string 64 #\f) approved?))
  (check-true (approval-decide! id digest approved?))
  (define-values (outcome grant) (approval-await-grant id digest 100))
  (check-equal? outcome (if approved? 'approved 'denied))
  grant)
(define (approve ref)
  (approve-supplemental-proposal! ref (interactive-grant ref)))
(define sample (hasheq 'objective "test only" 'scope '("tests/new.rkt") 'generation 3))
(define-runtime-path installed-source "../extensions/gsd")

(define authority-suite
  (test-suite "Supplemental authority: inert proposals and real interactive owner grants"
    (test-case "no root is discovered from cwd, stdin or environment"
      (parameterize ([current-supplemental-authority-root #f])
        (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample))))
      (check-equal? (hash-ref (supplemental-authority-status) 'status) "interactive-broker-required"))
    (test-case "retention returns immutable generic refs and deeply immutable data"
      (with-root (lambda (root _)
                   (define text (string-copy "original"))
                   (define payload (make-hasheq (list (cons 'text text))))
                   (define ref (propose-retain! "task-proposal" payload))
                   (string-set! text 0 #\X)
                   (hash-set! payload 'text "changed")
                   (define loaded (load-supplemental-record ref))
                   (check-true (supplemental-reference? ref))
                   (check-true (immutable? loaded))
                   (check-true (immutable? (hash-ref loaded 'payload)))
                   (check-true (immutable? (hash-ref (hash-ref loaded 'payload) 'text)))
                   (check-equal? (hash-ref (hash-ref loaded 'payload) 'text) "original")
                   (check-equal?
                    (bitwise-and #o7777 (file-or-directory-permissions (path-for root ref) 'bits))
                    #o600))))
    (test-case "canonical retention replay is idempotent and independent of key insertion order"
      (with-root (lambda (root _)
                   (define first (propose-retain! "task-proposal" (hasheq 'b 2 'a 1)))
                   (define path (path-for root first))
                   (define bytes (file->bytes path))
                   (check-equal? first (propose-retain! "task-proposal" (hasheq 'a 1 'b 2)))
                   (check-equal? bytes (file->bytes path))
                   (check-equal? (length (directory-list root)) 1)
                   (check-false (validate-supplemental-approval first)))))
    (test-case "generic refs retain links but convey no approval"
      (with-root
       (lambda (_root _)
         (define task (propose-retain! "task-proposal" sample))
         (define candidate
           (propose-retain! "candidate-proposal" (hasheq 'head "fixture") (list task)))
         (check-equal? (hash-ref (load-supplemental-record candidate) 'refs) (list task))
         (check-false (validate-supplemental-approval candidate))
         (check-exn exn:fail?
                    (lambda ()
                      (propose-retain! "task-proposal" sample (list (hasheq 'kind "grant"))))))))
    (test-case "forged grants, approved booleans and caller guards never confer authority"
      (with-root
       (lambda (_root _)
         (for ([forgery
                (in-list (list #t
                               (hasheq 'approved #t)
                               (hasheq 'kind "task-grant" 'approved #t 'guard (lambda () #t))))])
           (check-false (validate-supplemental-approval forgery)))
         (for ([kind '("task-grant" "task-approval"
                                    "candidate-approval"
                                    "supersession-approval"
                                    "revocation")])
           (check-exn exn:fail? (lambda () (propose-retain! kind (hasheq 'approved #t)))))
         (for ([verb (in-list owner-verbs)])
           (check-exn #rx"interactive broker grant required"
                      (lambda () (verb (hasheq 'approved #t) #t)))))))
    (test-case "author/model phase cannot write proposals, authorize or revoke"
      (with-root (lambda (root _)
                   (parameterize ([current-gsd-author-phase (hasheq 'phase 'test-author)])
                     (check-exn #rx"author phase"
                                (lambda () (propose-retain! "task-proposal" sample)))
                     (for ([verb (in-list owner-verbs)])
                       (check-exn #rx"author phase" (lambda () (verb sample #t))))
                     (check-false (validate-supplemental-approval sample)))
                   ;; The lock handle may exist; no authority record may.
                   (check-equal? (filter (lambda (entry)
                                           (regexp-match? #px"^[0-9a-f]"
                                                          (path->string entry)))
                                         (directory-list root))
                                 '()))))
    (test-case "stdin, reviewer/unbound phase, guards and policy grants are not authority"
      (with-root
       (lambda (root _)
         (define ref (propose-retain! "task-proposal" sample))
         (define before (directory-list root))
         (parameterize ([current-gsd-author-phase #f]
                        [current-input-port (open-input-string (make-string 64 #\a))])
           (for* ([verb (in-list owner-verbs)]
                  [fake (in-list (list #t
                                       #f
                                       (lambda () #t)
                                       (hasheq 'approved #t)
                                       (make-policy-approval-grant (hash-ref ref 'sha256))))])
             (check-exn #rx"interactive broker grant required" (lambda () (verb ref fake)))))
         (check-equal? before (directory-list root)))))
    (test-case "root inside cwd is rejected even if private"
      (with-root (lambda (_root workspace)
                   (parameterize ([current-directory workspace])
                     (check-exn #rx"inside project"
                                (lambda () (propose-retain! "task-proposal" sample)))))))
    (test-case "relative and traversing roots are rejected"
      (with-root (lambda (root workspace)
                   (for ([bad (list "private" (build-path workspace "private" 'up "private"))])
                     (parameterize ([current-supplemental-authority-root bad])
                       (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample))))))))
    (test-case "root mode drift prevents both writes and reads"
      (with-root (lambda (root _)
                   (define ref (propose-retain! "task-proposal" sample))
                   (file-or-directory-permissions root #o755)
                   (check-exn #rx"0700" (lambda () (load-supplemental-record ref)))
                   (check-exn #rx"0700" (lambda () (propose-retain! "task-proposal" sample))))))
    (test-case "record mode drift refuses read and idempotent replay"
      (with-root (lambda (root _)
                   (define ref (propose-retain! "task-proposal" sample))
                   (file-or-directory-permissions (path-for root ref) #o644)
                   (check-exn #rx"0600" (lambda () (load-supplemental-record ref)))
                   (check-exn #rx"0600" (lambda () (propose-retain! "task-proposal" sample))))))
    (test-case "tamper refuses read and replay without silently replacing bytes"
      (with-root (lambda (root _)
                   (define ref (propose-retain! "task-proposal" sample))
                   (define path (path-for root ref))
                   (define bytes #"{\"approved\":true}")
                   (replace-bytes! path bytes)
                   (check-exn exn:fail? (lambda () (load-supplemental-record ref)))
                   (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample)))
                   (check-equal? (file->bytes path) bytes))))
    (test-case "trailing whitespace and a second JSON value are not canonical"
      (with-root (lambda (root _)
                   (define ref (propose-retain! "task-proposal" sample))
                   (define path (path-for root ref))
                   (define bytes (file->bytes path))
                   (for ([suffix (list #"\n" #"{}")])
                     (replace-bytes! path (bytes-append bytes suffix))
                     (check-exn exn:fail? (lambda () (load-supplemental-record ref)))))))
    (test-case "symlinked root and ancestor are rejected"
      (with-root (lambda (root workspace)
                   (define link (build-path workspace "alias"))
                   (make-file-or-directory-link root link)
                   (for ([bad (list link (path->directory-path link))])
                     (parameterize ([current-supplemental-authority-root bad])
                       (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample)))))
                   (define nested (build-path root "nested"))
                   (make-directory nested)
                   (file-or-directory-permissions nested #o700)
                   (parameterize ([current-supplemental-authority-root (build-path link "nested")])
                     (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample)))))))
    (test-case "symlinked record and directory substitution are rejected"
      (with-root (lambda (root workspace)
                   (define ref (propose-retain! "task-proposal" sample))
                   (define path (path-for root ref))
                   (define outside (build-path workspace "copied-record"))
                   (rename-file-or-directory path outside)
                   (make-file-or-directory-link outside path)
                   (check-exn exn:fail? (lambda () (load-supplemental-record ref)))
                   (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" sample)))
                   (delete-file path)
                   (make-directory path)
                   (check-exn exn:fail? (lambda () (load-supplemental-record ref))))))
    (test-case "wrong kind, malformed digest and mutable ref strings are rejected"
      (with-root (lambda (_root _)
                   (define ref (propose-retain! "task-proposal" sample))
                   (for ([bad (list (hash-set ref 'kind "candidate-proposal")
                                    (hash-set ref 'sha256 "../escape")
                                    (hash-set ref 'sha256 (string-copy (hash-ref ref 'sha256))))])
                     (check-exn exn:fail? (lambda () (load-supplemental-record bad)))))))
    (test-case "control source remains excluded even when cwd is elsewhere"
      (with-root
       (lambda (_root workspace)
         (parameterize ([current-directory workspace]
                        [current-supplemental-authority-root (simplify-path installed-source #f)])
           (check-exn #rx"inside project" (lambda () (propose-retain! "task-proposal" sample)))))))
    (test-case "hardlinks and nonregular FIFO records are refused before reading"
      (with-root
       (lambda (root workspace)
         (define ref (propose-retain! "task-proposal" sample))
         (define path (path-for root ref))
         (check-true (system* (find-executable-path "ln") path (build-path workspace "hardlink")))
         (check-exn #rx"hardlinked" (lambda () (load-supplemental-record ref)))
         (delete-file path)
         (check-true (system* (find-executable-path "mkfifo") path))
         (file-or-directory-permissions path #o600)
         (check-exn #rx"not a regular file" (lambda () (load-supplemental-record ref))))))
    (test-case "unsupported JSON, duplicate-name symbols and cycles cannot leave records"
      (with-root (lambda (root _)
                   (define cycle (make-hasheq))
                   (hash-set! cycle 'self cycle)
                   (for ([payload (list (hasheq 'float 1.25)
                                        (hasheq (string->uninterned-symbol "key") 1 'key 2)
                                        cycle
                                        (make-string (add1 (* 1024 1024)) #\a))])
                     (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" payload))))
                   (check-equal? (directory-list root) '()))))
    (test-case "concurrent identical retention has one complete canonical object"
      (with-root (lambda (root _)
                   (define results (make-channel))
                   (define workers
                     (for/list ([i (in-range 8)])
                       (thread (lambda ()
                                 (with-handlers ([exn:fail? (lambda (e) (channel-put results e))])
                                   (channel-put results (propose-retain! "task-proposal" sample)))))))
                   (define refs
                     (for/list ([i (in-range 8)])
                       (sync/timeout 5 results)))
                   (for-each thread-wait workers)
                   (check-true (andmap supplemental-reference? refs))
                   (check-equal? (length (remove-duplicates refs)) 1)
                   (check-equal? (length (directory-list root)) 1)
                   (check-equal? (hash-ref (load-supplemental-record (car refs)) 'payload) sample))))
    (test-case "all proposal kinds require exact real grants; returned owner records are immutable"
      (with-root
       (lambda (_root _)
         (with-channel
          (lambda ()
            (for ([proposal-kind '("task-proposal" "candidate-proposal" "supersession-proposal")]
                  [approval-kind '("task-approval" "candidate-approval" "supersession-approval")])
              (define proposal (propose-retain! proposal-kind sample))
              (define grant (interactive-grant proposal))
              (define ref (approve-supplemental-proposal! proposal grant))
              (check-true (supplemental-reference? ref))
              (check-equal? (hash-ref ref 'kind) approval-kind)
              (define record (validate-supplemental-approval ref proposal))
              (check-true (immutable? record))
              (check-true (immutable? (hash-ref record 'owner-origin)))
              (check-equal? (hash-ref record 'proposal) proposal)
              (check-equal? record (load-supplemental-record ref))
              ;; Immutable hash comparator choice is not part of a wire identity.
              (define generic-ref
                (for/hash ([(k v) (in-hash ref)])
                  (values k v)))
              (define generic-proposal
                (for/hash ([(k v) (in-hash proposal)])
                  (values k v)))
              (check-equal? record (validate-supplemental-approval generic-ref generic-proposal))
              (check-false
               (validate-supplemental-approval ref (hash-set proposal 'kind "unrelated-proposal")))
              (check-false (approve-supplemental-proposal! proposal grant)))
            ;; Completed owner approvals persist independently of frontend lifetime.
            (define proposal (propose-retain! "task-proposal" (hasheq 'persist #t)))
            (define ref (approve proposal))
            (clear-pending-approvals!)
            (check-not-false (validate-supplemental-approval ref proposal)))))))
    (test-case "denied, wrong-digest, stale and twice-consumed grants cannot issue"
      (with-root (lambda (root _)
                   (with-channel
                    (lambda ()
                      (define proposal (propose-retain! "task-proposal" sample))
                      (define other (propose-retain! "task-proposal" (hasheq 'other #t)))
                      (define before (directory-list root))
                      (check-exn #rx"interactive broker"
                                 (lambda ()
                                   (approve-supplemental-proposal! proposal
                                                                   (interactive-grant proposal #f))))
                      (define wrong (interactive-grant other))
                      (check-false (approve-supplemental-proposal! proposal wrong))
                      (define stale (interactive-grant proposal))
                      (set-approval-channel! (make-approval-channel #:timeout-ms 100))
                      (check-false (approve-supplemental-proposal! proposal stale))
                      (define cleared (interactive-grant proposal))
                      (clear-pending-approvals!)
                      (check-false (approve-supplemental-proposal! proposal cleared))
                      (check-equal? before (directory-list root))
                      (define grant (interactive-grant proposal))
                      (parameterize ([current-gsd-author-phase (hasheq 'phase 'test-author)])
                        (check-exn #rx"author phase"
                                   (lambda () (approve-supplemental-proposal! proposal grant))))
                      ;; Refusal in model phase did not consume the trusted frontend grant.
                      (define ref (approve-supplemental-proposal! proposal grant))
                      (check-not-false (validate-supplemental-approval ref))
                      (check-false (approve-supplemental-proposal! proposal grant)))))))
    (test-case "re-approving the same proposal returns the existing approval; revoked never re-approves"
      (with-root
       (lambda (root _)
         (with-channel
          (lambda ()
            (define proposal (propose-retain! "task-proposal" sample))
            (define digest-files
              (lambda ()
                (filter (lambda (n) (regexp-match? #px"^[0-9a-f]{64}\\.json$" n))
                        (map path->string (directory-list root)))))
            (define ref (approve proposal))
            (check-equal? (length (digest-files)) 2) ; proposal + one approval
            ;; A second authentic owner decision is consumed but deduplicated:
            ;; the EXISTING approval identity is returned, no fresh nonce.
            (define ref2 (approve proposal))
            (check-equal? ref2 ref)
            (check-equal? (length (digest-files)) 2)
            (check-not-false (validate-supplemental-approval ref proposal))
            ;; Once any approval of the proposal is revoked the proposal is
            ;; permanently dead: re-approval fails closed.
            (define revocation
              (propose-retain! "revocation-proposal" (hasheq 'reason "dedup") (list ref)))
            (check-not-false (revoke! revocation (interactive-grant revocation)))
            (check-exn #rx"revoked proposal can never be re-approved"
                       (lambda () (approve proposal)))
            ;; proposal + approval + revocation proposal + revocation record;
            ;; the refused re-approval added no new approval identity.
            (check-equal? (length (digest-files)) 4)
            (check-false (validate-supplemental-approval ref proposal)))))))
    (test-case "concurrent same-proposal approvals yield exactly one admissible approval"
      (with-root
       (lambda (root _)
         (with-channel
          (lambda ()
            (define proposal (propose-retain! "task-proposal" sample))
            (define results (make-channel))
            (define workers
              (for/list ([i (in-range 6)])
                (thread
                 (lambda ()
                   (with-handlers ([exn:fail? (lambda (e) (channel-put results e))])
                     (channel-put
                      results
                      (approve-supplemental-proposal! proposal (interactive-grant proposal))))))))
            (define refs
              (for/list ([i (in-range 6)]) (sync/timeout 10 results)))
            (for-each thread-wait workers)
            (define ok (filter supplemental-reference? refs))
            (check-false (null? ok))
            (check-equal? (length (remove-duplicates ok)) 1)
            (define approval-files
              (filter (lambda (n) (regexp-match? #px"^[0-9a-f]{64}\\.json$" n))
                      (map path->string (directory-list root))))
            (check-equal? (length approval-files) 2) ; proposal + exactly one approval
            (check-not-false (validate-supplemental-approval (car ok) proposal)))))))
    (test-case "the authority lock is reentrant per thread and excludes other threads"
      (with-root
       (lambda (_root _)
         (define inner-result #f)
         (check-equal?
          (call-with-supplemental-authority-lock
           (lambda ()
             (set! inner-result
                   (call-with-supplemental-authority-lock (lambda () 'inner)))
             'outer))
          'outer)
         (check-equal? inner-result 'inner)
         ;; Mutual exclusion between threads: B cannot enter while A holds.
         (define order (make-channel))
         (define release (make-semaphore))
         (define counter 0)
         (define a
           (thread
            (lambda ()
              (call-with-supplemental-authority-lock
               (lambda ()
                 (channel-put order 'a-start)
                 (sync/timeout 5 release)
                 (set! counter (add1 counter))
                 (channel-put order 'a-end))))))
         (check-equal? (sync/timeout 5 order) 'a-start)
         (define b
           (thread
            (lambda ()
              (call-with-supplemental-authority-lock
               (lambda ()
                 (set! counter (+ counter 10))
                 (channel-put order 'b-end))))))
         (sleep 0.1) ; give a broken lock every chance to let B in
         (check-equal? counter 0 "thread B must wait while A holds the lock")
         (semaphore-post release)
         (check-equal? (sync/timeout 5 order) 'a-end)
         (check-equal? (sync/timeout 5 order) 'b-end)
         (thread-wait a)
         (thread-wait b)
         (check-equal? counter 11))))
    (test-case "admission revalidates live approvals under the lock and fails closed"
      (with-root
       (lambda (_root _)
         (with-channel
          (lambda ()
            (define proposal (propose-retain! "task-proposal" sample))
            (define ref (approve proposal))
            (define effect #f)
            (check-equal? (call-with-supplemental-admission (list ref) (lambda () (set! effect #t) 'ran))
                          'ran)
            (check-true effect)
            (check-exn #rx"admission refs must be immutable"
                       (lambda () (call-with-supplemental-admission (list (hasheq 'approved #t)) void)))
            (check-exn #rx"admission requires a live unrevoked"
                       (lambda () (call-with-supplemental-admission (list proposal) void)))
            (define revocation
              (propose-retain! "revocation-proposal" (hasheq) (list ref)))
            (revoke! revocation (interactive-grant revocation))
            (set! effect #f)
            (check-exn #rx"admission requires a live unrevoked"
                       (lambda () (call-with-supplemental-admission (list ref) void)))
            (check-false effect "a failed admission must never run its thunk"))))))
    (test-case "foreign references, copied approvals and content tamper fail revalidation"
      (with-root (lambda (root _)
                   (with-channel
                    (lambda ()
                      (define proposal (propose-retain! "task-proposal" sample))
                      (define ref (approve proposal))
                      (define approval-bytes (file->bytes (path-for root ref)))
                      (with-root (lambda (foreign _)
                                   (check-false (validate-supplemental-approval ref))
                                   (check-equal? proposal (propose-retain! "task-proposal" sample))
                                   (define own (approve proposal))
                                   (copy-file (path-for root ref) (path-for foreign ref))
                                   (check-false (validate-supplemental-approval ref))
                                   (define foreign-revocation
                                     (propose-retain! "revocation-proposal" (hasheq) (list ref)))
                                   (check-exn #rx"foreign owner origin"
                                              (lambda ()
                                                (revoke! foreign-revocation
                                                         (interactive-grant foreign-revocation))))
                                   (check-not-false (validate-supplemental-approval own))))
                      (replace-bytes! (path-for root ref) #"{}")
                      (check-false (validate-supplemental-approval ref))
                      (replace-bytes! (path-for root ref) approval-bytes)
                      (check-not-false (validate-supplemental-approval ref))
                      (replace-bytes! (path-for root proposal) #"{}")
                      (check-false (validate-supplemental-approval ref)))))))
    (test-case "revocation requires its own exact grant and replay never resurrects approval"
      (with-root (lambda (root _)
                   (with-channel
                    (lambda ()
                      (define proposal (propose-retain! "task-proposal" sample))
                      (define ref (approve proposal))
                      (define revocation
                        (propose-retain! "revocation-proposal" (hasheq 'reason "test") (list ref)))
                      (check-not-false (validate-supplemental-approval ref))
                      (check-false (revoke! revocation (interactive-grant proposal)))
                      (check-exn #rx"interactive broker"
                                 (lambda ()
                                   (revoke! revocation
                                            (make-policy-approval-grant (hash-ref revocation
                                                                                  'sha256)))))
                      (define grant (interactive-grant revocation))
                      (parameterize ([current-gsd-author-phase (hasheq 'phase 'test-author)])
                        (check-exn #rx"author phase" (lambda () (revoke! revocation grant))))
                      (define revoked (revoke! revocation grant))
                      (check-equal? (hash-ref revoked 'kind) "revocation")
                      (check-true (immutable? (load-supplemental-record revoked)))
                      (check-false (validate-supplemental-approval ref))
                      (check-false (revoke! revocation grant))
                      (check-equal? proposal (propose-retain! "task-proposal" sample))
                      (check-false (validate-supplemental-approval ref))
                      (check-equal? revoked (revoke! revocation (interactive-grant revocation)))
                      (check-false (validate-supplemental-approval ref))
                      ;; A revoked proposal is permanently dead: re-approval
                      ;; fails closed instead of minting a fresh nonce approval
                      ;; that would admit a second operation.
                      (check-exn #rx"revoked proposal can never be re-approved"
                                 (lambda () (approve proposal)))
                      (check-false (validate-supplemental-approval ref))
                      (replace-bytes! (path-for root revoked) #"{}")
                      (check-false (validate-supplemental-approval ref)))))))
    (test-case "public payloads and references cannot manufacture private owner records"
      (with-root
       (lambda (_root _)
         (with-channel
          (lambda ()
            (define proposal (propose-retain! "task-proposal" sample))
            (define ref (approve proposal))
            (define record (load-supplemental-record ref))
            (define inert (propose-retain! "task-proposal" record (list ref)))
            (check-false (validate-supplemental-approval inert))
            (check-false (validate-supplemental-approval (hash-set inert 'kind "task-approval")))
            (check-false (validate-supplemental-approval record))
            (check-exn #rx"only inert" (lambda () (propose-retain! "task-approval" record)))
            (check-exn #rx"not an approvable"
                       (lambda () (approve-supplemental-proposal! ref (interactive-grant ref))))
            (check-not-false (validate-supplemental-approval ref)))))))
    (test-case "one interactive grant cannot issue twice under concurrent consumption"
      (with-root
       (lambda (_root _)
         (with-channel
          (lambda ()
            (define proposal (propose-retain! "task-proposal" sample))
            (define grant (interactive-grant proposal))
            (define results (make-channel))
            (define workers
              (for/list ([i (in-range 8)])
                (thread (lambda ()
                          (with-handlers ([exn:fail? (lambda (e) (channel-put results e))])
                            (channel-put results (approve-supplemental-proposal! proposal grant)))))))
            (define refs
              (for/list ([i (in-range 8)])
                (sync/timeout 5 results)))
            (for-each thread-wait workers)
            (check-equal? (count supplemental-reference? refs) 1)
            (check-equal? (count not refs) 7)
            (check-not-false (validate-supplemental-approval (findf supplemental-reference?
                                                                    refs))))))))
    (test-case "malformed and stale revocation requests remain inert"
      (with-root (lambda (_root _)
                   (with-channel
                    (lambda ()
                      (define proposal (propose-retain! "task-proposal" sample))
                      (define ref (approve proposal))
                      (for ([targets (in-list (list '() (list proposal) (list ref ref)))])
                        (define bad (propose-retain! "revocation-proposal" (hasheq) targets))
                        (check-exn exn:fail? (lambda () (revoke! bad (interactive-grant bad))))
                        (check-not-false (validate-supplemental-approval ref)))
                      (define revocation (propose-retain! "revocation-proposal" (hasheq) (list ref)))
                      (define grant (interactive-grant revocation))
                      (clear-pending-approvals!)
                      (check-false (revoke! revocation grant))
                      (check-not-false (validate-supplemental-approval ref))
                      (check-exn #rx"not an approvable"
                                 (lambda ()
                                   (approve-supplemental-proposal!
                                    revocation
                                    (interactive-grant revocation)))))))))
    (test-case "origin loss/mode drift is not silently repaired by validation"
      (with-root (lambda (root _)
                   (with-channel (lambda ()
                                   (define proposal (propose-retain! "task-proposal" sample))
                                   (define ref (approve proposal))
                                   (file-or-directory-permissions (path-for root ref) #o644)
                                   (check-false (validate-supplemental-approval ref))
                                   (file-or-directory-permissions (path-for root ref) #o600)
                                   (define origin (build-path root ".owner-origin.json"))
                                   (file-or-directory-permissions origin #o644)
                                   (check-false (validate-supplemental-approval ref))
                                   (file-or-directory-permissions origin #o600)
                                   (check-not-false (validate-supplemental-approval ref))
                                   (delete-file origin)
                                   (check-false (validate-supplemental-approval ref))
                                   (check-false (file-exists? origin)))))))))
(module+ test
  (exit (run-tests authority-suite)))
