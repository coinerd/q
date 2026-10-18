#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 3600
;; Each negative retains a genuine owned receipt and isolated execution checkout;
;; budget the real ownership lifecycle rather than replacing it with fake proofs.
;; @covers extensions/gsd/owner-correction.rkt extensions/gsd/generated-readme.rkt extensions/gsd/author-session-recovery.rkt
;; @covers scripts/gsd-owner-correction.rkt
(require rackunit json racket/async-channel racket/runtime-path
         "helpers/lost-author-fixture.rkt"
         "../extensions/gsd/owner-correction.rkt"
         "../extensions/gsd/generated-readme.rkt"
         "../extensions/gsd/author-session-recovery.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/evidence-author-journal.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd-planning/execution-policy.rkt"
         (prefix-in cli: "../scripts/gsd-owner-correction.rkt")
         (only-in "../extensions/gsd/go-orchestrator.rkt" acquire-lease release-lease!))
(define-runtime-path owner-module "../extensions/gsd/owner-correction.rkt")
(define-runtime-path proof-module "../extensions/gsd/review-proof.rkt")
(define stopped (lambda () #f))
(define (with-fixture proc #:metrics-suffix [metrics-suffix ""])
  (define store (make-temporary-file "private-correction-~a" 'directory))
  (file-or-directory-permissions store #o700)
  (dynamic-wind void
    (lambda () (parameterize ([current-owner-correction-root store])
                 (with-lost-author proc #:metrics? #t #:metrics-suffix metrics-suffix)))
    (lambda () (delete-directory/files store))))
(define (propose f) (propose-owner-correction! (lost-author-root f) (lost-author-plan f) 0 #:session-live? stopped))
(define (authorize f digest)
  (authorize-owner-correction! (lost-author-root f) (lost-author-plan f) 0 digest #:session-live? stopped))
(define (apply! f digest)
  (apply-owner-correction! (lost-author-root f) (lost-author-plan f) 0 digest #:session-live? stopped))
(define (recover f)
  (recover-lost-author-session! (lost-author-root f) (lost-author-plan f) 0 #:session-live? stopped))
(define (delta result) (hash-ref (hash-ref result 'proposal) 'delta))
(define (cli f op . args)
  (parameterize ([current-output-port (open-output-string)])
    (cli:main (list->vector (append (list "--root" (path->string (lost-author-root f))
                                         "--plan" (lost-author-plan f) "--wave" "0" op) args)))))
(define (unchanged f j head)
  (check-equal? (journal f) j)
  (check-equal? (git-output (lost-author-cwd f) "rev-parse" "HEAD") head))
;; Every case has an independent campaign, private store, and owned registry.
;; Bounded fixture concurrency avoids multiplying baseline lifecycle wall time;
;; production recovery still serializes its own effects under recovery-lock.
;; Optional focused reruns after a local edit; unset means the FULL suite.
(define pending-cases (box '()))
(define-syntax-rule (correction-case name body ...)
  (let ([n name] [only (getenv "BUG0087_CASE")])
    (when (or (not only) (string-contains? n only))
      (set-box! pending-cases (cons (lambda () (test-case n body ...)) (unbox pending-cases))))))
(define (run-correction-cases!)
  (when (null? (unbox pending-cases))
    (error 'BUG0087_CASE "no correction cases match ~s" (getenv "BUG0087_CASE")))
  (define jobs (make-async-channel))
  (for ([job (in-list (reverse (unbox pending-cases)))]) (async-channel-put jobs job))
  (for ([_ (in-range 3)]) (async-channel-put jobs 'stop))
  (define workers
    (for/list ([_ (in-range 3)])
      (thread (lambda ()
                (let loop ()
                  (define job (async-channel-get jobs))
                  (unless (eq? job 'stop) (job) (loop)))))))
  (dynamic-wind void
    (lambda () (for-each thread-wait workers))
    (lambda ()
      (for ([worker (in-list workers)] #:unless (thread-dead? worker)) (break-thread worker))
      (for-each thread-wait workers))))
(module+ test
  (correction-case "BUG0087 GREEN: inspected exact digest, private authority, real full frozen owned Verify"
    (with-fixture
     (lambda (f)
       (define cwd (lost-author-cwd f))
       (define before (journal f))
       (define parent (git-output cwd "rev-parse" "HEAD"))
       (define original (file->bytes (build-path cwd "README.md")))
       ;; Actual pre-fix failure was captured in VALIDATION.md. A project JSON
       ;; declaration or /go alone must still NEVER authorize correction.
       (define r (cli f "--propose"))
       (define digest (hash-ref r 'digest))
       (define d (delta r))
       (check-equal? (hash-ref d 'parent) parent)
       (unchanged f before parent)
       (check-equal? (file->bytes (build-path cwd "README.md")) original)
       (check-exn #rx"absent.*owner authority" (lambda () (apply! f digest)))
       (cli f "--authorize" digest)
       (check-equal? (cli f "--apply" digest) (hash-ref d 'commit))
       (check-equal? (apply! f digest) (hash-ref d 'commit) "idempotent replay")
       (check-equal? (git-output cwd "rev-list" "--parents" "-n" "1" "HEAD")
                     (string-append (hash-ref d 'commit) " " parent))
       (check-equal? (git-output cwd "diff" "--name-only" parent "HEAD") "README.md")
       (check-equal? (git-output cwd "status" "--porcelain") "")
       (check-equal? (journal f) before "correction is NOT Verify or allowance refresh")
       (check-equal? (recover f) #t)
       (define j (journal f))
       (check-equal? (hash-ref j 'receipt-history) (list (lost-author-old f)))
       (check-equal? (hash-ref (journal-evidence-author-repair j) 'state) "completed")
       (define next (hash-ref j 'receipt))
       (check-equal? (hash-ref next 'head) (hash-ref d 'commit))
       (define proof (load-receipt-owned-proof next (lost-author-root f) (lost-author-plan f) 0))
       (check-not-false proof)
       (check-regexp-match #rx"racket scripts/metrics.rkt --lint" (hash-ref proof 'command))
       (check-equal? (hash-ref j 'stage) "context-ready" "fresh independent review still required")
       (check-equal? (git-output cwd "rev-parse" (string-append parent "^{tree}")) (hash-ref d 'parent-tree))
       ;; Confirmed publication replay must revalidate the same authority.
       (call-with-output-file (delivery-journal-path (lost-author-root f) (lost-author-plan f) 0)
         (lambda (out) (write-json before out)) #:exists 'truncate)
       (define replay (author-publication-recovery (lost-author-root f) (lost-author-plan f) 0))
       (check-not-false replay)
       (parameterize ([current-gsd-delivery-verify-command "exit 99"]) (replay))
       (check-equal? (journal f) j)
       (call-with-output-file (delivery-journal-path (lost-author-root f) (lost-author-plan f) 0)
         (lambda (out) (write-json before out)) #:exists 'truncate)
       (define a (for/first ([p (in-directory (current-owner-correction-root))]
                             #:when (string-suffix? (path->string p) ".authority")) p))
       (rename-file-or-directory a (path-replace-extension a #".missing"))
       (define refused-replay (author-publication-recovery (lost-author-root f) (lost-author-plan f) 0))
       (check-not-false refused-replay)
       (check-exn #rx"owner authority" refused-replay)
       (check-equal? (journal f) before))))

  (correction-case "proposal, authorization, and application reject live author and author phase"
    (with-fixture
     (lambda (f)
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (define lease (acquire-lease (lost-author-root f) (lost-author-plan f) #:session-id "live-author-fixture"))
       (dynamic-wind void
         (lambda () (check-exn #rx"live.*lease unavailable" (lambda () (cli f "--propose"))))
         (lambda () (release-lease! lease)))
       (for ([operation (list (lambda () (propose f)) (lambda () (authorize f digest))
                             (lambda () (apply! f digest)))])
         (parameterize ([current-gsd-author-phase 'implementation])
           (check-exn #rx"author phase" operation)))
       (check-exn #rx"live" (lambda () (propose-owner-correction! (lost-author-root f) (lost-author-plan f) 0
                                                                                #:session-live? (lambda () #t))))
       (check-exn #rx"live" (lambda () (authorize-owner-correction! (lost-author-root f) (lost-author-plan f) 0 digest
                                                                                  #:session-live? (lambda () #t))))
       (authorize f digest)
       (check-exn #rx"live" (lambda () (apply-owner-correction! (lost-author-root f) (lost-author-plan f) 0 digest
                                                                              #:session-live? (lambda () #t))))
       (parameterize ([current-owner-correction-root (build-path (lost-author-root f) "fake-authority")])
         (check-exn #rx"outside" (lambda () (propose f)))))))

  (correction-case "absent/fake authority, arbitrary prose, mixed dirt, staged bytes and mode refuse"
    (with-fixture
     (lambda (f)
       (define cwd (lost-author-cwd f))
       (define j (journal f))
       (define parent (git-output cwd "rev-parse" "HEAD"))
       (define original (file->string (build-path cwd "README.md")))
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (check-exn exn:fail? (lambda () (authorize f (make-string 64 #\0))))
       (define store (current-owner-correction-root))
       (file-or-directory-permissions store #o777)
       (check-exn #rx"not private" (lambda () (propose f)))
       (file-or-directory-permissions store #o700)
       (define alias (build-path store "symlink-authority"))
       (make-file-or-directory-link store alias)
       (parameterize ([current-owner-correction-root alias])
         (check-exn #rx"symlinked" (lambda () (propose f))))
       (delete-file alias)
       (put! (build-path cwd "README.md") (string-append original "arbitrary prose\n"))
       (check-exn #rx"prose/bytes/mode" (lambda () (propose f)))
       (put! (build-path cwd "README.md") (hash-ref (delta r) 'after))
       (check-equal? (hash-ref (propose f) 'digest) digest "only exact generated dirt is acceptable")
       (put! (build-path cwd "untracked") "must preserve\n")
       (check-exn #rx"mixed/dirty" (lambda () (propose f)))
       (delete-file (build-path cwd "untracked"))
       (file-or-directory-permissions (build-path cwd "README.md") #o755)
       (check-exn #rx"mode differ" (lambda () (propose f)))
       (file-or-directory-permissions (build-path cwd "README.md") #o644)
       (git-output cwd "add" "README.md")
       (check-exn #rx"index differs" (lambda () (authorize f digest)))
       (git-output cwd "restore" "--staged" "--" "README.md")
       ;; Project records are not a substitute for protected authority.
       (put! (build-path (lost-author-root f) ".planning" "owner-correction.json")
             (jsexpr->string (hasheq 'authorized #t 'digest digest)))
       (check-exn #rx"absent.*owner authority" (lambda () (apply! f digest)))
       (unchanged f j parent)
       (check-equal? (file->string (build-path cwd "README.md")) (hash-ref (delta r) 'after)))))

  (for ([kind '(journal generation receipt oldproof checkout snapshot fence cancellation origin branch parent)])
    (correction-case (format "immutable ~a binding drift refuses authorization and application" kind)
      (with-fixture
       (lambda (f)
         (define r (propose f)) (define digest (hash-ref r 'digest))
         (authorize f digest)
         (define cwd (lost-author-cwd f))
         (define j (journal f))
         (define rec (load-campaign-record (lost-author-root f) (lost-author-plan f)))
         (case kind
           [(journal generation receipt oldproof checkout)
            (define marker (journal-evidence-author-repair j))
            (define changed
              (case kind
                [(journal) (hash-set j 'operator-injected "not authority")]
                [(generation)
                 (hash-set j 'evidence-author-repair
                           (hash-set marker 'context (hash-set (hash-ref marker 'context) 'receipt-generation 1)))]
                [(oldproof)
                 (hash-set j 'evidence-author-repair
                           (hash-set marker 'old-receipt (hash-set (lost-author-old f) 'owned-proof #f)))]
                [(checkout)
                 (define clone (build-path (lost-author-root f) "clone"))
                 (git-output cwd "clone" "-q" "--no-hardlinks" (path->string cwd) (path->string clone))
                 (git-output clone "remote" "set-url" "origin" (hash-ref (lost-author-old f) 'origin))
                 (hash-set j 'evidence-author-repair
                           (hash-set marker 'context (hash-set (hash-ref marker 'context) 'repo (path->string clone))))]
                [(receipt) (hash-set j 'receipt (hash-set (hash-ref j 'receipt) 'tree (make-string 40 #\0)))]))
            (call-with-output-file (delivery-journal-path (lost-author-root f) (lost-author-plan f) 0)
              (lambda (out) (write-json changed out)) #:exists 'truncate)]
           [(snapshot) (define p (build-path (lost-author-root f) ".planning/waves/W0-checkpoint.md"))
                       (put! p (string-append (file->string p) "\nChanged frozen declaration\n"))]
           [(fence) (set-campaign-fence-token! rec 3)
                    (persist-campaign! (lost-author-root f) rec)]
           [(cancellation) (set-campaign-cancellation! rec (make-campaign-cancellation "cancelled" 0))
                           (persist-campaign! (lost-author-root f) rec)]
           [(origin) (git-output cwd "remote" "set-url" "origin" "https://github.com/other/q.git")]
           [(branch) (git-output cwd "checkout" "-qb" "wrong-branch")]
           [(parent) (commit! cwd "tests/second.rkt" "#lang racket/base\n")])
         ;; Invalid generation/receipt/proof injections make the journal
         ;; intentionally unreadable to production. Compare raw bytes rather
         ;; than asking the strict loader to accept the corrupted fixture.
         (define journal-path (delivery-journal-path (lost-author-root f) (lost-author-plan f) 0))
         (define injected-bytes (file->bytes journal-path))
         (check-exn exn:fail? (lambda () (authorize f digest)))
         (check-exn exn:fail? (lambda () (apply! f digest)))
         (check-equal? (file->bytes journal-path) injected-bytes)))))

  (for ([cut '(intent cas index worktree)])
    (correction-case (format "private-index/CAS crash after ~a is exactly replayable" cut)
      (with-fixture
       (lambda (f)
         (define r (propose f)) (define digest (hash-ref r 'digest))
         (define j (journal f))
         (authorize f digest)
         (parameterize ([current-owner-correction-cut
                         (lambda (where) (when (eq? where cut) (error 'injected "crash")))])
           (check-exn #rx"crash" (lambda () (apply! f digest))))
         (check-equal? (apply! f digest) (hash-ref (delta r) 'commit))
         (check-equal? (git-output (lost-author-cwd f) "status" "--porcelain") "")
         (check-equal? (journal f) j)))))

  ;; Real process exit deliberately bypasses dynamic-wind cleanup: replay must
  ;; tolerate an abandoned partial staging file, never a partial README.
  (for ([cut '(readme-writing readme-pre-rename worktree)])
    (correction-case (format "BUG0087-boundary: process crash at ~a leaves old/new regular README" cut)
      (with-fixture
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (define path (build-path cwd "README.md"))
         (file-or-directory-permissions path #o640)
         (define r (propose f)) (define digest (hash-ref r 'digest))
         (define d (delta r))
         (define before (journal f))
         (authorize f digest)
         (define expression
           `(begin
              (require (file ,(path->string owner-module)) (file ,(path->string proof-module)))
              (parameterize ([current-owner-correction-root ,(path->string (current-owner-correction-root))]
                             [current-owned-proof-root ,(path->string (current-owned-proof-root))]
                             [current-owner-correction-cut
                              (lambda (where) (when (eq? where ',cut) (exit 87)))])
                (apply-owner-correction! ,(path->string (lost-author-root f)) ,(lost-author-plan f) 0
                                         ,digest #:session-live? (lambda () #f)))))
         (check-equal? (system*/exit-code (find-executable-path "racket") "-e" (format "~s" expression))
                       87 "child must reach the exact injected crash boundary")
         (check-equal? (file->string path) (hash-ref d (if (eq? cut 'worktree) 'after 'before)))
         (check-equal? (file-or-directory-permissions path 'bits) #o640)
         (check-false (link-exists? path))
         (define staging (hash-ref (hash-ref (hash-ref r 'proposal) 'binding) 'execution-gitdir))
         (define leftovers
           (for/list ([p (in-directory staging)]
                      #:when (string-prefix? (path->string (file-name-from-path p)) "owner-readme-")) p))
         (if (eq? cut 'worktree)
             (check-equal? leftovers '())
             (begin
               (check-equal? (length leftovers) 1)
               (check-equal? (file->bytes (car leftovers))
                             (let ([after (string->bytes/utf-8 (hash-ref d 'after))])
                               (if (eq? cut 'readme-writing)
                                   (subbytes after 0 (quotient (bytes-length after) 2)) after)))))
         (check-equal? (apply! f digest) (hash-ref d 'commit))
         (check-equal? (file->string path) (hash-ref d 'after))
         (check-equal? (file-or-directory-permissions path 'bits) #o640)
         (check-equal? (git-output cwd "status" "--porcelain") "")
         (check-equal? (journal f) before)))))

  (for ([cut '(intent cas index readme-pre-rename)])
    (correction-case (format "BUG0087-boundary: authority revoked at ~a refuses next mutation" cut)
      (with-fixture
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (define r (propose f)) (define digest (hash-ref r 'digest))
         (define d (delta r))
         (define before (journal f))
         (authorize f digest)
         (define reached? #f)
         (define index-at-cut #f)
         (parameterize ([current-owner-correction-cut
                         (lambda (where)
                           (when (eq? where cut)
                             (set! reached? #t)
                             (set! index-at-cut (git-output cwd "ls-files" "--stage" "--" "README.md"))
                             (for ([a (in-directory (current-owner-correction-root))]
                                   #:when (string-suffix? (path->string a) ".authority"))
                               (delete-file a))))])
           (check-exn #rx"owner authority" (lambda () (apply! f digest))))
         (check-true reached? "revocation must occur inside the actual apply boundary")
         (check-equal? (git-output cwd "rev-parse" "HEAD")
                       (hash-ref d (if (eq? cut 'intent) 'parent 'commit)))
         (check-equal? (git-output cwd "ls-files" "--stage" "--" "README.md") index-at-cut)
         (check-equal? (file->string (build-path cwd "README.md")) (hash-ref d 'before))
         (check-equal? (journal f) before)))))

  (correction-case "BUG0087-boundary: source race immediately before rename preserves operator dirt"
    (with-fixture
     (lambda (f)
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (authorize f digest)
       (define path (build-path (lost-author-cwd f) "README.md"))
       (define reached? #f)
       (parameterize ([current-owner-correction-cut
                       (lambda (where)
                         (when (eq? where 'readme-pre-rename)
                           (set! reached? #t)
                           (put! path "operator prose at rename\n")))])
         (check-exn #rx"prose/bytes/mode differ" (lambda () (apply! f digest))))
       (check-true reached?)
       (check-equal? (file->string path) "operator prose at rename\n"))))

  (correction-case "source race after durable intent refuses CAS without discarding dirt"
    (with-fixture
     (lambda (f)
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (authorize f digest)
       (parameterize ([current-owner-correction-cut
                       (lambda (where)
                         (when (eq? where 'intent)
                           (put! (build-path (lost-author-cwd f) "tests/second.rkt") "operator dirt\n")))])
         (check-exn #rx"mixed/dirty" (lambda () (apply! f digest))))
       (check-equal? (git-output (lost-author-cwd f) "rev-parse" "HEAD") (hash-ref (delta r) 'parent))
       (check-equal? (file->string (build-path (lost-author-cwd f) "tests/second.rkt")) "operator dirt\n"))))

  (for ([kind '(forbidden reverted-forbidden unrecognized-generator)])
    (correction-case (format "~a can never buy an allowance expansion" kind)
      (with-fixture
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (case kind
           [(forbidden reverted-forbidden)
            (commit! cwd "outside.rkt" "#lang racket/base\n")
            (when (eq? kind 'reverted-forbidden) (git-output cwd "revert" "--no-edit" "HEAD"))]
           [(unrecognized-generator) (commit! cwd "scripts/metrics.rkt" "#lang racket/base\n(error 'unsafe)\n")])
         (check-exn #rx"outside frozen FILES" (lambda () (propose f)))))))

  (for ([kind '(arbitrary-commit nonterminal dirty wrong-tree merge reverted-unchanged mode-hidden assume-hidden)])
    (correction-case (format "recovery refuses ~a even with a separately authorized proposal" kind)
      (with-fixture
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (define r (propose f)) (define digest (hash-ref r 'digest))
         (authorize f digest)
         (define j (journal f))
         (case kind
           [(arbitrary-commit)
            (commit! cwd "README.md" (hash-ref (delta r) 'after))]
           [(reverted-unchanged)
            (commit! cwd "outside.rkt" "forbidden\n")
            (git-output cwd "revert" "--no-edit" "HEAD")
            (git-output cwd "revert" "--no-edit" (hash-ref (delta r) 'parent))
            (check-equal? (git-output cwd "rev-parse" "HEAD^{tree}") (hash-ref (lost-author-old f) 'tree))]
           [(nonterminal)
            (apply! f digest)
            (commit! cwd "tests/second.rkt" "#lang racket/base\n")]
           [(dirty) (apply! f digest) (put! (build-path cwd "README.md") "operator dirt\n")]
           [(mode-hidden)
            (apply! f digest)
            (git-output cwd "config" "core.filemode" "false")
            (file-or-directory-permissions (build-path cwd "README.md") #o755)]
           [(assume-hidden)
            (apply! f digest)
            (git-output cwd "update-index" "--assume-unchanged" "scripts/metrics.rkt")
            (put! (build-path cwd "scripts/metrics.rkt") "#lang racket/base\n(exit 0)\n")]
           [(wrong-tree) (apply! f digest) (commit! cwd "README.md" "not generated\n")]
           [(merge)
            (apply! f digest)
            (git-output cwd "checkout" "-qb" "other")
            (commit! cwd "tests/second.rkt" "#lang racket/base\n")
            (git-output cwd "checkout" (hash-ref (lost-author-old f) 'branch))
            (git-output cwd "merge" "--no-ff" "-qm" "not a correction" "other")])
         (define result (recover f))
         (check-true (string? result) (format "must refuse ~a: ~s" kind result))
         (check-equal? (journal f) j)))))

  (correction-case "actual unrecognized generator in authentic old receipt refuses without execution"
    (with-fixture
     (lambda (f)
       (define before (journal f))
       (check-exn #rx"unrecognized project metrics generator" (lambda () (propose f)))
       (check-equal? (journal f) before))
     #:metrics-suffix "\n; Benign rule-version identity drift in the authentic baseline.\n"))

  (correction-case "forged proposal fields cannot authorize wrong parent/tree/commit/blobs/modes/inputs/generator"
    (with-fixture
     (lambda (f)
       (define r (propose f))
       (define p (hash-ref r 'proposal))
       (define d (delta r))
       (for ([key '(parent parent-tree tree commit before-entry after-entry before after inputs generators)])
         (define changed
           (case key
             [(parent parent-tree tree commit) (make-string 40 #\0)]
             [(before-entry) (list "README.md" "100755" (caddr (hash-ref d 'before-entry)))]
             [(after-entry) (list "README.md" "100644" (make-string 40 #\0))]
             [(before after) (string-append (hash-ref d key) "forbidden prose\n")]
             [(inputs) (cdr (hash-ref d 'inputs))]
             [(generators) '(("scripts/metrics.rkt" . "fake"))]))
         (define forged (hash-set p 'delta (hash-set d key changed)))
         (define digest (correction-digest forged))
         ;; Deliberate test-only corruption of even the protected proposal
         ;; store cannot bypass independent derivation and binding checks.
         (define path (build-path (current-owner-correction-root) (string-append digest ".proposal")))
         (call-with-output-file path (lambda (out) (write forged out)))
         (file-or-directory-permissions path #o400)
         (check-exn exn:fail? (lambda () (authorize f digest))))
       (check-equal? (hash-ref (journal f) 'receipt) (lost-author-old f)))))

  (correction-case "authority revoked during real publication cannot certify or replay"
    (with-fixture
     (lambda (f)
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (authorize f digest) (apply! f digest)
       (define before (journal f))
       (parameterize ([current-gsd-approved-head-publisher
                       (lambda args
                         (define result (apply default-approved-head-publisher args))
                         (for ([path (in-directory (current-owner-correction-root))]
                               #:when (string-suffix? (path->string path) ".authority"))
                           (delete-file path))
                         result)])
         (check-regexp-match #rx"owner authority" (recover f)))
       (check-equal? (journal f) before)
       (check-equal? (hash-ref (journal f) 'receipt) (lost-author-old f)))))

  (correction-case "full Verify RED keeps the exact old receipt, original author commit and spent allowance"
    (with-fixture
     (lambda (f)
       (define cwd (lost-author-cwd f))
       (commit! cwd "tests/second.rkt" "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 2))\n")
       (define r (propose f)) (define digest (hash-ref r 'digest))
       (define j (journal f))
       (authorize f digest) (apply! f digest)
       (check-regexp-match #rx"full frozen Verify failed" (recover f))
       (check-equal? (journal f) j)
       (check-equal? (hash-ref (journal f) 'receipt) (lost-author-old f))
       (check-not-false (load-receipt-owned-proof (lost-author-old f) (lost-author-root f) (lost-author-plan f) 0)))))
  (run-correction-cases!))
