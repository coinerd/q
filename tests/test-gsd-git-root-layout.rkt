#lang racket/base
;; @covers extensions/gsd/git-root-layout.rkt
;; @speed fast
;; @suite extensions
;; @boundary integration

;; tests/test-gsd-git-root-layout.rkt — R76 (BUG-0076) delivery-root
;; resolution contract, red-first against the name/marker-based selectors.
;;
;; The pinned W0 algorithm under test (git-root-layout.rkt):
;;   1. an explicit authenticated branch-delivery context wins (validated by
;;      real git toplevel/realpath identity, never falls through silently);
;;   2. otherwise Git's OWN rev-parse answers select a direct repository or
;;      a genuinely nested q repository (nested-q two-tier workspace);
;;   3. a decoy outer repository beside an independent nested q — and any
;;      broken .git marker — refuse with typed root+reason diagnostics;
;;      directory names and marker existence are never authority;
;;   4. worktree .git files and linked worktrees resolve to their physical
;;      toplevel; no-repository stays #f (unchanged);
;;   5. every resolver call site (plan-context-builder, system git port,
;;      wave-executor, verifier evidence root, coordinator artifact/controller
;;      roots, handoff readback root) agrees on ONE authenticated repository
;;      identity for the same layout, and declared targets outside the
;;      selected delivery root refuse naming the root.
;;
;; Fixtures are REAL git repositories (hermetic private environment);
;; git-dependent cases are gated on GIT availability (logged skip, not fail).

(require json
         racket/file
         racket/list
         racket/path
         racket/set
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         (only-in "helpers/private-fixture-templates.rkt" git-quiet!)
         (only-in "../extensions/gsd/git-root-layout.rkt"
                  exn:fail:git-root?
                  exn:fail:git-root-root
                  exn:fail:git-root-reason
                  resolve-delivery-root
                  git-root-resolution
                  git-root-resolution?
                  git-root-resolution-kind
                  git-root-resolution-root
                  git-root-resolution-reason
                  delivery-root-or-raise
                  resolve-explicit-delivery-root!
                  git-root-marker?
                  require-delivery-target-in-root!)
         (only-in "../extensions/gsd/plan-context-builder.rkt"
                  find-git-root-dir
                  find-git-root
                  get-diff-excerpt
                  current-git-root)
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/composition-root.rkt"
         (only-in "../extensions/gsd/wave-executor.rkt" find-repo-root)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  evidence-git-root
                  current-gsd-delivery-branch-context
                  make-branch-delivery-context
                  committed-branch-changed-files
                  check-git-available)
         (only-in "../extensions/gsd/delivery-coordinator.rkt"
                  resolve-coordinator-repo
                  default-artifact-provenance-root
                  default-delivery-controller
                  delivery-effect-result-kind
                  delivery-effect-result-data)
         (only-in "../extensions/gsd/delivery-handoff.rkt"
                  delivery-readback
                  delivery-readback-repo)
         (only-in "../extensions/gsd/delivery-journal.rkt" record-delivery-receipt!)
         (only-in "../extensions/gsd/delivery-publication.rkt"
                  default-approved-head-publisher))

(define GIT (find-executable-path "git"))

;; ============================================================
;; Real-git fixtures (hermetic, temp-rooted, self-cleaning)
;; ============================================================

;; The canonical two-tier workspace: outer planning dir WITHOUT any git
;; repository + a genuine nested q/ repository carrying the committed target
;; change. Returns (values outer q).
(define (make-two-tier! #:outer-repo? [outer-repo? #f]
                        #:dir-name [dir-name "ws"]
                        #:ignore-q? [ignore-q? #f])
  (define outer (make-temporary-file (format "~a-~~a" dir-name) 'directory))
  (define q (build-path outer "q"))
  (make-directory* q)
  (when outer-repo?
    (git-quiet! outer "init" "--quiet")
    (when ignore-q?
      (display-to-file "q/\n" (build-path outer ".gitignore") #:exists 'truncate)))
  ;; genuine nested product repository with a committed docs target
  (git-quiet! q "init" "--quiet" "-b" "main")
  (git-quiet! q "config" "user.email" "r76@test.local")
  (git-quiet! q "config" "user.name" "R76 Test")
  (make-directory* (build-path q "docs"))
  (display-to-file "target body\n" (build-path q "docs" "target.md") #:exists 'truncate)
  (git-quiet! q "add" "-A")
  (git-quiet! q "commit" "-qm" "baseline")
  (git-quiet! q "update-ref" "refs/remotes/origin/main" "HEAD")
  (when outer-repo?
    ;; the same-named OUTER target: the decoy carries docs/target.md too
    (make-directory* (build-path outer "docs"))
    (display-to-file "DECOY BODY\n" (build-path outer "docs" "target.md") #:exists 'truncate)
    (git-quiet! outer "add" "-A")
    (git-quiet! outer "commit" "-qm" "decoy baseline"))
  (values outer q))

;; A committed delivery change on a fresh wave branch of the nested repo.
;; Returns (values branch base-commit).
(define (git-out dir . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port err]
                 [current-directory dir])
    (apply system*/exit-code GIT args))
  (string-trim (get-output-string out)))

;; A committed delivery change on the nested repo. Returns (values branch base-commit).
(define (commit-nested-change! q)
  (display-to-file "changed body\n" (build-path q "docs" "target.md") #:exists 'truncate)
  (git-quiet! q "add" "-A")
  (git-quiet! q "commit" "-qm" "wave delivery")
  (values "main" (git-out q "rev-parse" "main")))

;; A durable delivery journal carrying the minimal receipt contract
;; (branch + 40-hex head) that the controller seams read BEFORE dispatching.
(define (write-receipt-journal! base plan wave)
  (record-delivery-receipt!
   base plan wave
   (hasheq 'repo
           "git@github.com:example/q.git"
           'branch
           "campaign/r76/test"
           'origin
           "git@github.com:example/q.git"
           'evidence
           "docs/reports/gsd-wave-evidence/r76-fixture.rktd"
           'head
           (make-string 40 #\a)
           'tree
           (make-string 40 #\b)
           'verified-at
           0)))

(define (cleanup! dir)
  (delete-directory/files dir #:must-exist? #f))

;; every resolver entry that must agree on ONE repository identity
(define (all-resolver-roots outer)
  (define git-port (gsd-git-port-find-root (gsd-effect-ports-git (current-gsd-effect-ports))))
  (list (cons "plan-context-builder.find-git-root-dir" (find-git-root-dir outer))
        (cons "plan-context-builder.find-git-root" (find-git-root outer))
        (cons "system-adapters.git-port.find-root" (git-port outer))
        (cons "wave-executor.find-repo-root" (find-repo-root outer))
        (cons "delivery-verifier.evidence-git-root" (evidence-git-root outer))
        (cons "delivery-coordinator.controller/artifact repo" (resolve-coordinator-repo outer))
        (cons "delivery-handoff.readback repo" (delivery-readback-repo outer))))

(define (assert-refused! thunk label)
  (with-handlers ([exn:fail:git-root?
                   (lambda (e)
                     (check-true (string-contains? (exn-message e) "git-root-refusal")
                                 (format "~a: refusal message must be typed" label))
                     (check-pred path-string? (exn:fail:git-root-root e)
                                 (format "~a: refusal must name the root" label))
                     (check-pred string? (exn:fail:git-root-reason e)
                                 (format "~a: refusal must carry the reason" label))
                     #t)]
                  [exn:fail?
                   (lambda (e)
                     (fail (format "~a: raised a NON-typed error: ~a" label (exn-message e))))])
    (define v (thunk))
    (fail (format "~a: expected a git-root refusal, got ~s" label v))))

;; ============================================================
;; Suite
;; ============================================================
(define suite
  (test-suite "R76 git-root layout resolution"

      ;; ----------------------------------------------------------
      ;; 1. Two-tier workspace (outer NOT a repo + genuine nested q)
      ;; ----------------------------------------------------------
      (test-case "two-tier workspace: every resolver call site selects the nested q repository"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping two-tier agreement case")
            (let-values ([(outer q) (make-two-tier!)])
              (dynamic-wind
               void
               (lambda ()
                 ;; ONE identity across all call sites
                 (define roots (all-resolver-roots outer))
                 (for ([pair (in-list roots)])
                   (check-equal? (path->string (path->complete-path (cdr pair)))
                                 (path->string (path->complete-path q))
                                 (format "~a must select the nested q repository" (car pair))))
                 ;; git cwd parity: git rooted at the selected root reports q
                 (check-equal? (git-out q "rev-parse" "--show-toplevel")
                               (git-out (find-git-root-dir outer) "rev-parse" "--show-toplevel"))
                 ;; q/docs -> docs mapping: the declared root-relative target
                 ;; maps to the nested repository's committed file, and the
                 ;; base-relative q/ declaration maps to the same file
                 (check-true (file-exists? (build-path (find-git-root-dir outer) "docs" "target.md")))
                 (check-false (directory-exists? (build-path (find-git-root-dir outer) "q")))
                 ;; changed-file inclusion through the evidence gate: the
                 ;; base commit is the PRE-change baseline, the delivery
                 ;; commit lands on the branch afterwards
                 (define base (git-out q "rev-parse" "main"))
                 (define-values (branch _ignored) (commit-nested-change! q))
                 (define ctx (make-branch-delivery-context #:repo-root q
                                                           #:branch branch
                                                           #:base-commit base
                                                           #:worktree-path #f))
                 (define changed
                   (parameterize ([current-gsd-delivery-branch-context ctx])
                     (committed-branch-changed-files)))
                 (check-true (set-member? changed "docs/target.md")
                             "committed nested change must be included")
                 ;; diff excerpt inclusion via the plan-context seam (the
                 ;; process-level override stays out of the way)
                 (parameterize ([current-git-root #f])
                   (check-true
                    (string-contains? (get-diff-excerpt outer '("docs/target.md")) "target.md")
                    "diff evidence must come from the nested repository"))
                 ;; no decoy outer target exists to hijack anything
                 (check-false (file-exists? (build-path outer "docs"))))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 2. Decoy outer repository (gitignoring q) + genuine nested q:
      ;;    ambiguous identity REFUSES everywhere; the decoy's same-named
      ;;    outer target can never hijack changed-files/prompt/receipts.
      ;; ----------------------------------------------------------
      (test-case "decoy outer repo over genuine nested q: typed refusal at every call site"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping decoy refusal case")
            (let-values ([(outer q) (make-two-tier! #:outer-repo? #t
                                                    #:dir-name "decoy"
                                                    #:ignore-q? #t)])
              (dynamic-wind
               void
               (lambda ()
                 ;; the decoy really does carry the same-named target
                 (check-true (file-exists? (build-path outer "docs" "target.md")))
                 (check-equal? (git-out outer "status" "--porcelain") "")
                 ;; pure resolution refuses with root+reason
                 (define res (resolve-delivery-root outer))
                 (check-eq? (git-root-resolution-kind res) 'refused)
                 (check-true (string-contains? (git-root-resolution-reason res) "ambiguous"))
                 (check-true (string-contains? (git-root-resolution-reason res) (path->string q)))
                 ;; every call site refuses — none selects the decoy
                 (for ([pair (in-list
                              (list (cons "pcb" (lambda () (find-git-root-dir outer)))
                                    (cons "system" (lambda ()
                                                     ((gsd-git-port-find-root
                                                       (gsd-effect-ports-git
                                                        (current-gsd-effect-ports)))
                                                      outer)))
                                    (cons "wave-executor" (lambda () (find-repo-root outer)))
                                    (cons "verifier" (lambda () (evidence-git-root outer)))
                                    (cons "coordinator" (lambda () (resolve-coordinator-repo outer)))
                                    (cons "artifact-root"
                                          (lambda ()
                                            (default-artifact-provenance-root outer "plan" 0)))
                                    (cons "readback" (lambda () (delivery-readback-repo outer)))))])
                   (assert-refused! (cdr pair) (car pair)))
                 ;; readback maps the refusal to its typed pending stop with
                 ;; the redacted root+reason — never queries the decoy
                 (define plan (make-string 64 #\a))
                 (define rb (delivery-readback outer plan 0))
                 (check-equal? (hash-ref rb 'status) "delivery-pending")
                 (check-true (string-contains? (hash-ref rb 'reason) "git-root-refusal"))
                 ;; the controller maps the refusal to a typed blocked stop:
                 ;; with the minimal durable receipt in place the preflight
                 ;; dispatch reaches the shared root seam, refuses, and the
                 ;; decoy repository is never used
                 (write-receipt-journal! outer plan 0)
                 (define ctl (default-delivery-controller outer plan 0 "delivery-preflight"))
                 (check-eq? (delivery-effect-result-kind ctl) 'blocked)
                 (check-true
                  (string-contains?
                   (hash-ref (delivery-effect-result-data ctl) 'reason "")
                   "git-root-refusal")))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 3. .git FILE worktrees and linked worktrees are supported
      ;; ----------------------------------------------------------
      (test-case "linked worktree (.git file) resolves to its own physical toplevel"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping linked-worktree case")
            (let-values ([(outer q) (make-two-tier!)])
              (dynamic-wind
               void
               (lambda ()
                 (define wt (build-path outer "wt-campaign-r76deadbeef-w1"))
                 (git-quiet! q "commit" "--allow-empty" "-qm" "base")
                 (git-quiet! q "worktree" "add" "-q" "-b" "campaign/r76deadbeef/w1"
                             (path->string wt))
                 (check-true (file-exists? (build-path wt ".git")) "worktree carries a .git FILE")
                 ;; physical worktree role: resolves to the worktree itself
                 (check-equal? (path->string (path->complete-path (find-git-root-dir wt)))
                               (path->string (path->complete-path wt)))
                 ;; explicit authenticated context: worktree root validates
                 (check-equal? (path->string (path->complete-path
                                              (resolve-explicit-delivery-root! wt)))
                               (path->string (path->complete-path wt)))
                 ;; controlling-repository role: the nested repo validates too
                 (check-equal? (path->string (path->complete-path
                                              (resolve-explicit-delivery-root! q)))
                               (path->string (path->complete-path q)))
                 ;; receipt-shaped roots carry a trailing separator
                 ;; (path-only + simplify-path); identity strips it
                 (check-equal? (path->string (path->complete-path
                                              (resolve-explicit-delivery-root!
                                               (path->string (path->complete-path q)))))
                               (path->string (path->complete-path q))))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 4. Broken .git markers refuse (never return a bogus root)
      ;; ----------------------------------------------------------
      (test-case "broken nested .git marker refuses; broken root marker refuses"
        (let ()
          (define outer (make-temporary-file "broken-~a" 'directory))
          (dynamic-wind
           void
           (lambda ()
             ;; nested marker with an unusable gitdir pointer
             (define q (build-path outer "q"))
             (make-directory* q)
             (display-to-file "gitdir: /nonexistent/r76\n" (build-path q ".git"))
             (check-true (git-root-marker? q))
             (assert-refused! (lambda () (find-git-root-dir outer)) "broken nested marker")
             (delete-directory/files q)
             ;; bare root marker with an unusable gitdir pointer
             (display-to-file "gitdir: /nonexistent/r76\n" (build-path outer ".git"))
             (assert-refused! (lambda () (find-git-root-dir outer)) "broken root marker")
             ;; a fake .git directory is equally unusable
             (delete-file (build-path outer ".git"))
             (make-directory (build-path outer ".git"))
             (assert-refused! (lambda () (find-git-root-dir outer)) "fake root marker dir")
             ;; verifier availability answers #f (fail closed, no crash)
             (check-false (car (cdr (check-git-available outer)))))
           (lambda () (cleanup! outer)))))

      ;; ----------------------------------------------------------
      ;; 5. Starting inside nested source resolves the nested repository
      ;; ----------------------------------------------------------
      (test-case "start inside nested source (q/docs) resolves the q repository"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping nested-source case")
            (let-values ([(outer q) (make-two-tier!)])
              (dynamic-wind
               void
               (lambda ()
                 (define start (build-path q "docs"))
                 (check-equal? (path->string (path->complete-path (find-git-root-dir start)))
                               (path->string (path->complete-path q)))
                 (check-equal? (path->string (path->complete-path (find-repo-root start)))
                               (path->string (path->complete-path q))))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 6. Spaces in paths: exact string handling end to end
      ;; ----------------------------------------------------------
      (test-case "workspace and target paths with spaces resolve and include changes"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping spaces case")
            (let ()
              (define outer (make-temporary-file "my r76 ws-~a" 'directory))
              (dynamic-wind
               void
               (lambda ()
                 (define q (build-path outer "q"))
                 (make-directory* (build-path q "docs"))
                 (git-quiet! q "init" "--quiet")
                 (display-to-file "body\n" (build-path q "docs" "with space.md"))
                 (git-quiet! q "add" "-A")
                 (git-quiet! q "commit" "-qm" "baseline")
                 (check-equal? (path->string (path->complete-path (find-git-root-dir outer)))
                               (path->string (path->complete-path q)))
                 (parameterize ([current-git-root #f])
                   (check-true
                    (string-contains? (get-diff-excerpt outer '("docs/with space.md"))
                                      "with space.md"))))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 7. No repository anywhere: #f, unchanged (and the explicit
      ;;    current-git-root override still applies as the last resort)
      ;; ----------------------------------------------------------
      (test-case "no repository: all resolvers answer #f without raising"
        (let ()
          (define plain (make-temporary-file "plain-~a" 'directory))
          (dynamic-wind
           void
           (lambda ()
             (check-false (find-git-root-dir plain))
             (check-false (find-repo-root plain))
             (check-false
              ((gsd-git-port-find-root (gsd-effect-ports-git (current-gsd-effect-ports))) plain))
             ;; the coordinator/readback seams keep the LEGACY unscoped child
             ;; q/ naming for marker-less synthetic trees (enumeration then
             ;; decides, per the existing coordinator law)
             (check-equal? (path->string (path->complete-path (resolve-coordinator-repo plain)))
                           (path->string (build-path plain "q")))
             (check-equal? (path->string (path->complete-path (delivery-readback-repo plain)))
                           (path->string (build-path plain "q")))
             ;; the F-7 explicit override remains the no-repo fallback
             ;; (the override keeps its historical marker check)
             (define ov (build-path plain "override-ws"))
             (make-directory* (build-path ov ".git"))
             (check-equal? (path->string
                            (path->complete-path
                             (parameterize ([current-git-root ov]) (find-git-root plain))))
                           (path->string (path->complete-path ov))))
           (lambda () (cleanup! plain)))))

      ;; ----------------------------------------------------------
      ;; 8. Explicit authenticated branch-delivery context wins
      ;; ----------------------------------------------------------
      (test-case "explicit context wins over layout; unverifiable context refuses"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping explicit-context case")
            (let ()
              (define outer-a (make-temporary-file "ctx-a-~a" 'directory))
              (define outer-b (make-temporary-file "ctx-b-~a" 'directory))
              (dynamic-wind
               void
               (lambda ()
                 (define qa (build-path outer-a "q"))
                 (define qb (build-path outer-b "q"))
                 (for ([q (list qa qb)])
                   (make-directory* q)
                   (git-quiet! q "init" "--quiet"))
                 ;; start resolution at outer-b picks qb, but the ACTIVE
                 ;; authenticated context binds the campaign to qa: context
                 ;; wins, layout never silently switches the root.
                 (check-equal? (path->string
                                (parameterize ([current-gsd-delivery-branch-context
                                                (make-branch-delivery-context
                                                 #:repo-root qa
                                                 #:branch "campaign/abcd1234/w0"
                                                 #:base-commit (make-string 40 #\a))])
                                  (evidence-git-root outer-b)))
                               (path->string (path->complete-path qa)))
                 ;; an unusable context root refuses instead of falling through
                 (assert-refused!
                  (lambda ()
                    (parameterize ([current-gsd-delivery-branch-context
                                    (make-branch-delivery-context
                                     #:repo-root (build-path outer-b "not-a-repo")
                                     #:branch "campaign/abcd1234/w0"
                                     #:base-commit (make-string 40 #\a))])
                      (evidence-git-root outer-b)))
                  "unusable context root")
                 ;; a context root that is a SUBDIRECTORY of a repository does
                 ;; not match its own toplevel: refuse (identity, not prefix)
                 (make-directory* (build-path qb "sub"))
                 (assert-refused!
                  (lambda ()
                    (parameterize ([current-gsd-delivery-branch-context
                                    (make-branch-delivery-context
                                     #:repo-root (build-path qb "sub")
                                     #:branch "campaign/abcd1234/w0"
                                     #:base-commit (make-string 40 #\a))])
                      (evidence-git-root outer-b)))
                  "subdirectory context root"))
               (lambda ()
                 (cleanup! outer-a)
                 (cleanup! outer-b))))))

      ;; ----------------------------------------------------------
      ;; 9. Targets outside the selected delivery root refuse, naming the root
      ;; ----------------------------------------------------------
      (test-case "outside/escaping/symlink targets refuse naming the root; in-root anchors pass"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping containment case")
            (let-values ([(outer q) (make-two-tier!)])
              (dynamic-wind
               void
               (lambda ()
                 (define root-s (path->string (path->complete-path q)))
                 (define (refused-reason target)
                   (with-handlers ([exn:fail:git-root?
                                    (lambda (e) (exn:fail:git-root-reason e))]
                                   [exn:fail?
                                    (lambda (e) (fail (format "non-typed refusal for ~a: ~a" target e)))])
                     (require-delivery-target-in-root! q target #:base outer)
                     (fail (format "target ~a should have been refused" target))))
                 ;; root-relative declaration passes
                 (check-not-exn
                  (lambda () (require-delivery-target-in-root! q "docs/target.md" #:base outer)))
                 ;; base-relative q/ declaration passes (dual anchor)
                 (check-not-exn
                  (lambda () (require-delivery-target-in-root! q "q/docs/target.md" #:base outer)))
                 ;; ../ escape refuses naming the root
                 (check-true (string-contains? (refused-reason "../escape.md") root-s))
                 ;; absolute outside target refuses naming the root
                 (check-true (string-contains? (refused-reason outer) root-s))
                 ;; symlinked target pointing outside refuses
                 (define outside (make-temporary-file "outside-target-~a"))
                 (dynamic-wind
                  void
                  (lambda ()
                    (make-file-or-directory-link outside (build-path q "link-out.md"))
                    (check-true (string-contains? (refused-reason "link-out.md") root-s)))
                  (lambda () (delete-file outside))))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 10. Plain q/ SUBDIRECTORY of one repository is not ambiguity
      ;; ----------------------------------------------------------
      (test-case "plain q subdirectory inside one repository selects that repository"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping plain-q case")
            (let ()
              (define repo (make-temporary-file "monorepo-~a" 'directory))
              (dynamic-wind
               void
               (lambda ()
                 (git-quiet! repo "init" "--quiet")
                 (make-directory* (build-path repo "q" "src"))
                 (display-to-file "x" (build-path repo "q" "src" "m.rkt"))
                 (git-quiet! repo "add" "-A")
                 (git-quiet! repo "commit" "-qm" "baseline")
                 (check-equal? (path->string (path->complete-path (find-git-root-dir repo)))
                               (path->string (path->complete-path repo)))
                 (check-equal? (path->string (path->complete-path (find-repo-root repo)))
                               (path->string (path->complete-path repo))))
               (lambda () (cleanup! repo))))))

      ;; ----------------------------------------------------------
      ;; 11. Publication selector: an unverifiable snapshot repo blocks
      ;; ----------------------------------------------------------
      (test-case "publication --repo selector validates the snapshot root or blocks typed"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping publication selector case")
            (let ()
              (define outer (make-temporary-file "pub-~a" 'directory))
              (dynamic-wind
               void
               (lambda ()
                 (define snapshot (hasheq 'repo (path->string (build-path outer "no-such-repo"))
                                          'branch "campaign/r76/w0"
                                          'head (make-string 40 #\a)
                                          'tree (make-string 40 #\b)
                                          'origin "git@github.com:example/q.git"))
                 (define result
                   (default-approved-head-publisher outer
                                                    "plan"
                                                    0
                                                    snapshot
                                                    "attempt"
                                                    "fence"
                                                    "coordinator-fence"
                                                    (hasheq)))
                 (check-equal? (hash-ref result 'status) "blocked")
                 (check-true (string-contains? (hash-ref result 'reason) "git-root-refusal")))
               (lambda () (cleanup! outer))))))

      ;; ----------------------------------------------------------
      ;; 12. Symlinked start path into the repository resolves the repo
      ;; ----------------------------------------------------------
      (test-case "symlinked start path into the repository resolves the repository"
        (if (not GIT)
            (log-warning "r76: git unavailable; skipping symlink-start case")
            (let-values ([(outer q) (make-two-tier!)])
              (dynamic-wind
               void
               (lambda ()
                 (define link (build-path outer "q-link"))
                 (make-file-or-directory-link q link)
                 (check-equal? (path->string (path->complete-path (find-git-root-dir link)))
                               (path->string (path->complete-path q))))
               (lambda () (cleanup! outer))))))))

(module+ test
  (exit (run-tests suite)))
