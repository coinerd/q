#lang racket/base
;; STABILITY: internal
;; Candidate-preserving metadata settlement: the PURE/KNOWN-SOURCE validator
;; leaf for disposition.preserved-candidate.
;;
;; This module NEVER grants authority, never mutates a record, never runs a
;; model and never treats a metadata commit as verification/approval. It
;; authenticates the exact spent third-generation ancestry through the SAME
;; fixed amendment policy the claim owner uses, authenticates the genuine old
;; candidate proposal/approval from the private store, and proves the Git
;; relation:
;;
;;   original K commit == candidate.payload.source (the candidate's LOGICAL repo)
;;   E_old.head        == strict ancestor of K, changing only task test/harness
;;   M                 == single-parent descendant of K on the same branch
;;   K -> M diff       == exactly the binding's inherited owner-path entries
;;   every other tree entry (including model-authored source) is byte-identical
;;   the current clean checkout is exactly M in the authenticated PHYSICAL
;;   context.repo (a linked worktree sharing git-common-dir with the logical
;;   candidate source), never a substituted repo/head.
;;
;; The optional retained-receipt authentication is a SECOND layer
;; (validate-preserved-candidate!): it loads the authenticated E_old context
;; through evidence-readiness's factored strict receipt loader and requires it
;; to equal the authenticated parent task context. The ordinary
;; current-evidence-context physical source gate is never weakened or reused
;; with a substituted head.
;;
;; Module graph: claim -> this leaf is static; this leaf reaches claim and
;; evidence-readiness only through their existing known-source lazy loads, so
;; first use cannot re-enter a partially instantiated module and no caller
;; supplies an authentication procedure.
(require racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         (only-in file/sha1 bytes->hex-string)
         openssl
         "supplemental-evidence-format.rkt"
         "supplemental-disposition-policy.rkt"
         "supplemental-evidence-authority.rkt")
(provide preserved-candidate-binding
         valid-preserved-candidate?
         preserved-candidate-admission?
         preserved-candidate-admission-amendment
         preserved-candidate-admission-parent-task
         preserved-candidate-admission-old-context
         preserved-candidate-admission-terminal
         preserved-candidate-admission-candidate
         preserved-candidate-admission-candidate-approval
         preserved-candidate-admission-original-source
         preserved-candidate-admission-descendant-source
         preserved-candidate-admission-owner-entries
         preserved-candidate-admission-task
         validate-preserved-candidate-claim-binding!
         validate-preserved-candidate!)
(define-runtime-path claim-module "supplemental-disposition-claim.rkt")
(define-runtime-path readiness-module "evidence-readiness.rkt")
;; Returned immutable facts for the integration lane. Data only: the
;; constructor is NOT provided, so callers cannot mint an admission.
(struct preserved-candidate-admission
        (amendment parent-task
                   old-context
                   terminal
                   candidate
                   candidate-approval
                   original-source
                   descendant-source
                   owner-entries
                   task)
  #:transparent)
(define (refuse message)
  (error 'supplemental-preserved-candidate "~a" message))
(define (exact-keys? h keys)
  (and (hash? h)
       (immutable? h)
       (= (hash-count h) (length keys))
       (andmap (lambda (k) (hash-has-key? h k)) keys)))
(define (hex? v n)
  (and (string? v) (= (string-length v) n) (regexp-match? #px"^[0-9a-f]+$" v)))
(define (relative-path? p)
  (and (string? p)
       (regexp-match? #px"^[A-Za-z0-9_][A-Za-z0-9_./-]*$" p)
       (for/and ([part (in-list (string-split p "/" #:trim? #f))])
         (and (not (member part '("" "." ".."))) (not (string-prefix? part "-"))))))
(define (nonempty-string? v)
  (and (string? v) (positive? (string-length v))))
;; ---------------------------------------------------------------------------
;; Git facts (fixed argument vectors only; no caller-controlled options)
;; ---------------------------------------------------------------------------
(define (git-run trim? repo args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" repo args)))
  (unless (zero? code)
    (refuse (string-append "git authentication failed: " (string-trim (get-output-string err)))))
  (define text (get-output-string out))
  (if trim?
      (string-trim text)
      text))
(define (git repo . args)
  (git-run #t repo args))
(define (git-raw repo . args)
  (git-run #f repo args))
;; Byte-exact Git object read. blob-sha256 hashes the EXACT raw blob bytes;
;; decoding to a UTF-8 string first would re-encode non-text blobs.
(define (git-run-bytes repo args)
  (define out (open-output-bytes))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" repo args)))
  (unless (zero? code)
    (refuse (string-append "git authentication failed: " (string-trim (get-output-string err)))))
  (get-output-bytes out))
(define (git-succeeds? repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (zero? (parameterize ([current-output-port out]
                        [current-error-port err])
           (apply system*/exit-code (find-executable-path "git") "-C" repo args))))
(define (commit-id repo value what)
  (define oid (git repo "rev-parse" "--verify" (string-append value "^{commit}")))
  (unless (equal? oid value)
    (refuse (format "~a is not the exact commit identity" what)))
  oid)
(define (tree-id repo value)
  (git repo "rev-parse" "--verify" (string-append value "^{tree}")))
(define (tree-entries repo commit)
  (for/list ([item (in-list
                    (string-split (git-raw repo "ls-tree" "-r" "-z" commit) "\u0000" #:trim? #f))]
             #:unless (equal? item ""))
    (define m (regexp-match #px"^([0-9]{6}) ([a-z]+) ([0-9a-f]{40})\t(.*)$" item))
    (unless m
      (refuse "malformed tree entry"))
    (hasheq 'mode (list-ref m 1) 'type (list-ref m 2) 'oid (list-ref m 3) 'path (list-ref m 4))))
(define (index-entries entries)
  (for/hash ([e (in-list entries)])
    (values (hash-ref e 'path) e)))
(define (same-entry? a b)
  (and (equal? (hash-ref a 'mode) (hash-ref b 'mode))
       (equal? (hash-ref a 'type) (hash-ref b 'type))
       (equal? (hash-ref a 'oid) (hash-ref b 'oid))))
(define (blob-sha256 repo oid)
  (bytes->hex-string (sha256-bytes (open-input-bytes (git-run-bytes repo
                                                                    (list "cat-file" "blob" oid))))))
(define (check-single-parent! repo k m)
  (define line (git repo "rev-list" "--parents" "-n" "1" m))
  (define parts (string-split line))
  (unless (and (= 2 (length parts)) (equal? (car parts) m) (equal? (cadr parts) k))
    (refuse "metadata descendant must be a direct single-parent child of original K")))
;; Current-checkout facts ALWAYS come from the authenticated physical context
;; repo. The candidate's logical repo may be a different path (a linked
;; worktree) and is never substituted for the live command target.
(define (canonical-directory p)
  (path->string (simplify-path (path->complete-path p) #f)))
(define (git-common-dir repo)
  (canonical-directory (git repo "rev-parse" "--path-format=absolute" "--git-common-dir")))
(define (check-shared-repository! source context)
  (define source-repo (hash-ref source 'repo))
  (unless (and (equal? (git source-repo "rev-parse" "--is-inside-work-tree") "true")
               (equal? (git-common-dir source-repo) (git-common-dir (hash-ref context 'repo))))
    (refuse "original candidate source is not a worktree of the authenticated physical repository")))
(define (check-source-context! source context)
  (unless (and (nonempty-string? (hash-ref source 'repo #f))
               (nonempty-string? (hash-ref source 'branch #f))
               (nonempty-string? (hash-ref source 'origin #f))
               (equal? (hash-ref source 'branch) (hash-ref context 'branch))
               (equal? (hash-ref source 'origin) (hash-ref context 'origin))
               (equal? (git (hash-ref source 'repo) "config" "--get" "remote.origin.url")
                       (hash-ref source 'origin)))
    (refuse "original source branch/origin is not the authenticated old context")))
(define (changed-paths repo a b)
  (define ta (index-entries (tree-entries repo a)))
  (define tb (index-entries (tree-entries repo b)))
  (for/list ([path (in-list (sort (remove-duplicates (append (hash-keys ta) (hash-keys tb)))
                                  string<?))]
             #:unless (and (hash-ref ta path #f)
                           (hash-ref tb path #f)
                           (same-entry? (hash-ref ta path) (hash-ref tb path))))
    path))
;; The old authenticated receipt context must be a STRICT ancestor of the
;; original candidate K. Only the task's two model-authorable paths may differ;
;; production/source and owner metadata are not silently rewritten.
(define (check-old-ancestor! repo old-head k-head allowed)
  (when (equal? old-head k-head)
    (refuse "original K must be a strict descendant of the authenticated old E context"))
  (unless (git-succeeds? repo "merge-base" "--is-ancestor" old-head k-head)
    (refuse "original K is not a descendant of the authenticated old E context"))
  (define changed (changed-paths repo old-head k-head))
  (unless (and (pair? changed)
               (for/and ([path (in-list changed)])
                 (member path allowed)))
    (refuse "E_old->K changes paths outside the approved test/harness paths")))
(define (check-clean-current! repo context descendant m-tree)
  (define expected (canonical-directory (hash-ref context 'repo)))
  (define actual (canonical-directory (git repo "rev-parse" "--show-toplevel")))
  (unless (equal? actual expected)
    (refuse "current checkout root is not the authenticated physical context"))
  ;; Read-only inspection must not take optional index/output locks.
  (unless (string=? (git repo "--no-optional-locks" "status" "--porcelain=v1" "--untracked-files=all")
                    "")
    (refuse "current source is dirty or untracked"))
  (unless (equal? (git repo "symbolic-ref" "--quiet" "--short" "HEAD") (hash-ref context 'branch))
    (refuse "current source branch changed"))
  (unless (equal? (git repo "rev-parse" "--verify" "HEAD") (hash-ref descendant 'head))
    (refuse "current source is not the metadata descendant M"))
  (unless (equal? (git repo "rev-parse" "--verify" "HEAD^{tree}") m-tree)
    (refuse "current source tree is not the metadata descendant M tree"))
  (unless (equal? (git repo "config" "--get" "remote.origin.url") (hash-ref context 'origin))
    (refuse "current source origin changed")))
;; ---------------------------------------------------------------------------
;; Binding access
;; ---------------------------------------------------------------------------
(define (preserved-candidate-binding task)
  (define d (and (hash? task) (hash-ref task 'disposition #f)))
  (and (hash? d) (hash-ref d 'preserved-candidate #f)))
;; ---------------------------------------------------------------------------
;; Candidate / approval authentication against the private store
;; ---------------------------------------------------------------------------
(define candidate-payload-fields
  '(kind context
         task-sha256
         task-proposal
         task-approval
         source
         attempt-id
         attempt-fence
         execution-cwd
         manifest
         sha256))
(define manifest-entry-fields '(path mode blob sha256))
(define (manifest-path-set entries)
  (map (lambda (e) (hash-ref e 'path)) entries))
(define (validate-manifest! repo k-commit expected entries)
  (unless (and (list? entries)
               (= (length entries) (length expected))
               (equal? (sort (manifest-path-set entries) string<?) (sort expected string<?))
               (= (length expected) (length (remove-duplicates expected))))
    (refuse "candidate manifest path set changed"))
  (define trees (index-entries (tree-entries repo k-commit)))
  (for ([entry (in-list entries)])
    (unless (exact-keys? entry manifest-entry-fields)
      (refuse "candidate manifest entry is malformed"))
    (define path (hash-ref entry 'path))
    (unless (relative-path? path)
      (refuse "candidate manifest path is not a literal relative path"))
    (define tree (hash-ref trees path #f))
    (unless (and tree
                 (equal? (hash-ref tree 'type) "blob")
                 (equal? (hash-ref entry 'mode) "100644")
                 (equal? (hash-ref tree 'mode) "100644")
                 (equal? (hash-ref entry 'blob) (hash-ref tree 'oid))
                 (equal? (hash-ref entry 'sha256) (blob-sha256 repo (hash-ref tree 'oid))))
      (refuse "candidate manifest does not match the ORIGINAL K Git tree"))))
(define (authenticate-candidate! binding parent-task a)
  (define candidate-ref (hash-ref binding 'candidate))
  (define approval-ref (hash-ref binding 'candidate-approval))
  (define record (load-supplemental-record candidate-ref))
  (define payload (hash-ref record 'payload #f))
  (unless (and (equal? (hash-ref record 'kind) "candidate-proposal")
               (equal? (hash-ref record 'refs)
                       (list (hash-ref a 'parent-proposal) (hash-ref a 'parent-approval)))
               (hash? payload)
               (exact-keys? payload candidate-payload-fields)
               (equal? (hash-ref payload 'kind) "supplemental-evidence-candidate/v1")
               (equal? payload (seal-supplemental-record payload)))
    (refuse "original candidate proposal is not the genuine spent candidate"))
  (define context (hash-ref parent-task 'context))
  (define allocation (hash-ref a 'allocation))
  (define source (hash-ref binding 'original-source))
  (unless (and (equal? (hash-ref payload 'context) context)
               (equal? (hash-ref payload 'task-sha256) (hash-ref parent-task 'sha256))
               (equal? (hash-ref payload 'task-proposal) (hash-ref a 'parent-proposal))
               (equal? (hash-ref payload 'task-approval) (hash-ref a 'parent-approval))
               (equal? (hash-ref payload 'source) source)
               (equal? (hash-ref payload 'attempt-id) (hash-ref allocation 'attempt-id))
               (equal? (hash-ref payload 'attempt-fence) (hash-ref allocation 'attempt-fence))
               (equal? (hash-ref payload 'execution-cwd) (hash-ref context 'repo)))
    (refuse "original candidate is not bound to the spent task/allocation/source"))
  (define approval (validate-supplemental-approval approval-ref candidate-ref))
  (unless (and (hash? approval)
               (equal? (hash-ref approval 'kind) "candidate-approval")
               (equal? (hash-ref approval 'proposal) candidate-ref))
    (refuse "original candidate approval is absent, revoked or foreign"))
  (values candidate-ref approval-ref payload))
;; ---------------------------------------------------------------------------
;; Full binding validation (fixed third-generation ancestry, no caller auth)
;; ---------------------------------------------------------------------------
(define (strip-preserved d)
  (hash-remove d 'preserved-candidate))
(define (inherited-scope? parent task)
  (for/and ([(key value) (in-hash parent)]
            #:unless (memq key '(kind sha256 control-plane-head disposition)))
    (equal? value (hash-ref task key #f))))
(define (entry-for map path what)
  (or (hash-ref map path #f) (refuse (format "missing ~a tree entry ~s" what path))))
(define (check-owner-entries! binding repo k-commit m-commit inherited-paths)
  (define entries (hash-ref binding 'owner-paths))
  (define k (index-entries (tree-entries repo k-commit)))
  (define m (index-entries (tree-entries repo m-commit)))
  (define changed
    (begin
      (unless (equal? (sort (hash-keys k) string<?) (sort (hash-keys m) string<?))
        (refuse "K->M tree path set changed (rename/addition/removal refused)"))
      (for/list ([path (in-list (sort (hash-keys k) string<?))]
                 #:unless (same-entry? (hash-ref k path) (hash-ref m path)))
        path)))
  (define bound-paths (sort (manifest-path-set entries) string<?))
  (unless (equal? bound-paths (sort changed string<?))
    (refuse "binding owner-path set is not exactly the K->M diff"))
  (for ([path (in-list bound-paths)])
    (unless (member path inherited-paths)
      (refuse "binding changed a path outside the inherited owner paths")))
  (for ([entry (in-list entries)])
    (define path (hash-ref entry 'path))
    (define before (entry-for k path "original K"))
    (define after (entry-for m path "metadata descendant M"))
    (unless (and (equal? (hash-ref before 'mode) "100644")
                 (equal? (hash-ref after 'mode) "100644")
                 (equal? (hash-ref before 'type) "blob")
                 (equal? (hash-ref after 'type) "blob")
                 (not (equal? (hash-ref before 'oid) (hash-ref after 'oid)))
                 (equal? (hash-ref entry 'before-blob) (hash-ref before 'oid))
                 (equal? (hash-ref entry 'after-blob) (hash-ref after 'oid))
                 (equal? (hash-ref entry 'before-sha256) (blob-sha256 repo (hash-ref before 'oid)))
                 (equal? (hash-ref entry 'after-sha256) (blob-sha256 repo (hash-ref after 'oid))))
      (refuse "owner path blob/sha256/mode before/after mismatch"))))
(define (validate-preserved-candidate-claim-binding! task)
  (define a (disposition-amendment))
  (unless (disposition-third-generation? a)
    (refuse "preserved candidate requires the fixed third-generation amendment"))
  (define d (and (hash? task) (hash-ref task 'disposition #f)))
  (define binding (and (hash? d) (hash-ref d 'preserved-candidate #f)))
  (unless (and (valid-supplemental-task? task)
               (equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
               binding
               (valid-preserved-candidate? binding))
    (refuse "preserved candidate binding is absent or malformed"))
  ;; Fixed-a3 ancestry/terminal authentication is the claim owner's known
  ;; source; the lazy load only breaks the static claim -> leaf -> claim cycle.
  (define inspect (dynamic-require claim-module 'inspect-supplemental-disposition-parent))
  (define-values (parent-proposal parent-task terminal) (inspect (hash-ref a 'parent-approval)))
  (define old (disposition-pointer parent-proposal (hash-ref a 'parent-approval)))
  (unless (and (equal? (strip-preserved d) (disposition-descriptor a old))
               (inherited-scope? parent-task task))
    (refuse "preserved binding widened or changed the fixed amendment scope"))
  (define context (hash-ref parent-task 'context))
  (define source (hash-ref binding 'original-source))
  (define descendant (hash-ref binding 'descendant-source))
  ;; source.repo is the candidate's LOGICAL repo identity (exactly what
  ;; candidate.payload.source carried). Its branch/origin must agree with the
  ;; authenticated old context; its path need not equal the PHYSICAL context
  ;; repo, but both must be worktrees of one git-common-dir.
  (check-source-context! source context)
  (when (equal? (hash-ref source 'head) (hash-ref descendant 'head))
    (refuse "metadata descendant must differ from original K"))
  ;; The spent terminal's own candidate links must be exactly the bound pair.
  (define facts (hash-ref terminal 'facts))
  (unless (and (equal? (hash-ref facts 'candidate-ref #f) (hash-ref binding 'candidate))
               (equal? (hash-ref facts 'candidate-approval-ref #f)
                       (hash-ref binding 'candidate-approval)))
    (refuse "bound candidate is not the terminal's spent candidate"))
  (define-values (candidate-ref approval-ref payload) (authenticate-candidate! binding parent-task a))
  (define physical (hash-ref context 'repo))
  (check-shared-repository! source context)
  (define old-commit (commit-id physical (hash-ref context 'head) "authenticated old E context"))
  (define k-commit (commit-id physical (hash-ref source 'head) "original K"))
  (define m-commit (commit-id physical (hash-ref descendant 'head) "metadata descendant M"))
  (unless (equal? (tree-id physical old-commit) (hash-ref context 'tree))
    (refuse "authenticated old E context tree identity changed"))
  (unless (equal? (tree-id physical k-commit) (hash-ref source 'tree))
    (refuse "original K tree identity changed"))
  (unless (equal? (tree-id physical m-commit) (hash-ref descendant 'tree))
    (refuse "metadata descendant M tree identity changed"))
  (check-old-ancestor! physical
                       old-commit
                       k-commit
                       (list (hash-ref parent-task 'test-path) (hash-ref parent-task 'harness-path)))
  (check-single-parent! physical k-commit m-commit)
  (check-clean-current! physical context descendant (hash-ref descendant 'tree))
  (check-owner-entries! binding physical k-commit m-commit (hash-ref parent-task 'owner-paths))
  (validate-manifest! physical
                      k-commit
                      (append (list (hash-ref parent-task 'test-path)
                                    (hash-ref parent-task 'harness-path))
                              (hash-ref parent-task 'owner-paths))
                      (hash-ref payload 'manifest))
  ;; Integration compares the descendant to a committed-delivery-snapshot,
  ;; whose repo is the logical common repository. Keep that logical identity in
  ;; the returned facts; the live Git commands above remain physical.
  (define descendant-source
    (hasheq 'repo
            (hash-ref source 'repo)
            'branch
            (hash-ref source 'branch)
            'origin
            (hash-ref source 'origin)
            'head
            (hash-ref descendant 'head)
            'tree
            (hash-ref descendant 'tree)))
  (preserved-candidate-admission a
                                 parent-task
                                 context
                                 terminal
                                 candidate-ref
                                 approval-ref
                                 source
                                 descendant-source
                                 (hash-ref binding 'owner-paths)
                                 task))
;; The full admission re-authenticates the retained E_old receipt/context and
;; requires it to equal the fixed-a3 parent task context. It still leaves the
;; ordinary current-evidence-context strict gate untouched.
(define (validate-preserved-candidate! base plan wave task)
  (define admission (validate-preserved-candidate-claim-binding! task))
  (define context (hash-ref (preserved-candidate-admission-parent-task admission) 'context))
  (unless (and (equal? plan (hash-ref context 'plan-id)) (equal? wave (hash-ref context 'wave)))
    (refuse "preserved binding is outside the authenticated campaign/wave"))
  (define load-receipt (dynamic-require readiness-module 'load-authenticated-evidence-receipt))
  (define get-context (dynamic-require readiness-module 'authenticated-evidence-receipt-context))
  (define receipt (load-receipt base plan wave))
  (unless (equal? (get-context receipt) context)
    (refuse "retained authenticated E context changed"))
  admission)
