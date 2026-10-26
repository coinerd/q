#lang racket/base
;; STABILITY: internal
;;
;; extensions/gsd/git-root-layout.rkt — R76 (BUG-0076) shared delivery-root
;; resolution. Single leaf module (imports only racket/base I/O + path
;; utilities) so every resolver call site can delegate to it without a
;; circular dependency: plan-context-builder, system-adapters, wave-executor,
;; delivery-verifier, delivery-coordinator, delivery-handoff and
;; delivery-publication all sit ABOVE this module in the import graph.
;;
;; The W0-pinned recognition/validation algorithm (delivery-inventory §6):
;;
;;   1. An explicit authenticated branch-delivery context wins: when the
;;      caller supplies one (the verifier's current-gsd-delivery-branch-context,
;;      passed here as a VALUE so this module stays a leaf), its repo-root is
;;      used only after `git rev-parse` proves realpath identity with its
;;      actual toplevel. Unverifiable → typed refusal, never silent fall-through.
;;   2. Otherwise the candidates are Git's OWN answers, never marker sniffing:
;;      `git -C <start> rev-parse --path-format=absolute --show-toplevel
;;      --git-common-dir` for the start path and for the nested `q/` child.
;;      The nested-q two-tier layout is recognized ONLY when the child is a
;;      real nested repository (validated toplevel/realpath, not marker
;;      existence); a mere directory named q is not identity.
;;   3. Selection:
;;        - start inside a repo, q not an independent repo → that repo
;;          (direct checkout; q a plain subdirectory or a linked worktree of
;;          the SAME repository — shared git-common-dir — is one identity);
;;        - start outside any repo, q a real repository rooted at <start>/q
;;          → the nested repository (two-tier workspace layout);
;;        - start inside a repo AND q an independent repository (different
;;          git-common-dir, e.g. a decoy outer repo ignoring q/) → REFUSE:
;;          ambiguous identity; directory names and markers are never authority;
;;        - a .git marker whose repository is unusable (broken gitdir file,
;;          corrupt directory) → typed refusal, never a bogus root;
;;        - no marker and no repository → absent (#f), the unchanged no-repo
;;          contract.
;;   4. Declared targets must resolve INSIDE the selected delivery root;
;;      an outside or escaping target fails explicitly naming the root and
;;      never switches roots implicitly.
;;
;; Physical worktree vs controlling-repository roles are preserved: a linked
;; worktree (.git FILE) resolves to its own physical toplevel, while the
;; controlling repository identity is the shared git-common-dir.
;;
;; Refusals raise exn:fail:git-root carrying root + reason. Each call site
;; maps that typed error onto its own typed-stop shape (verifier gate
;; failure, blocked delivery-effect-result, delivery-pending readback,
;; blocked publication); nothing falls through to an unauthenticated root.

(require racket/file
         racket/path
         racket/port
         racket/string
         racket/system)

(provide exn:fail:git-root
         exn:fail:git-root?
         exn:fail:git-root-root
         exn:fail:git-root-reason
         (struct-out git-root-probe)
         (struct-out git-root-resolution)
         current-git-root-layout-run-git
         resolve-delivery-root
         delivery-root-or-raise
         resolve-explicit-delivery-root!
         git-root-refusal-message
         git-root-marker?
         require-delivery-target-in-root!)

;; ============================================================
;; Typed refusal
;; ============================================================

(struct exn:fail:git-root exn:fail (root reason) #:transparent)

(define (git-root-refusal root reason)
  (exn:fail:git-root
   (git-root-refusal-message root reason)
   (current-continuation-marks)
   root
   reason))

(define (git-root-refusal-message root reason)
  (format "git-root-refusal: ~a (delivery root: ~a)" reason root))

;; ============================================================
;; Git invocation seam
;; ============================================================

;; (dir args) -> (list exit-code stdout stderr). The default shells REAL git;
;; the parameter exists so tests can pin the boundary without a filesystem.
;; Production callers leave it untouched: resolution authenticity REQUIRES
;; the real git answers (validated toplevel, not marker existence).
(define current-git-root-layout-run-git
  (make-parameter
   (lambda (dir args)
     (define git (find-executable-path "git"))
     (cond
       [(not git) (list 127 "" "git executable not found on PATH")]
       [else
        (define out (open-output-string))
        (define err (open-output-string))
        (define code
          (parameterize ([current-output-port out]
                         [current-error-port err])
            (apply system*/exit-code
                   git
                   "-C"
                   (path->string (path->complete-path dir))
                   args)))
        (list code (get-output-string out) (get-output-string err))]))
   (lambda (f)
     (unless (and (procedure? f) (procedure-arity-includes? f 2))
       (raise-argument-error 'current-git-root-layout-run-git
                             "(procedure-arity-includes/c 2)"
                             f))
     f)))

(define (run-git-probe dir)
  ((current-git-root-layout-run-git)
   dir
   (list "rev-parse" "--path-format=absolute" "--show-toplevel" "--git-common-dir")))

;; One successful rev-parse yields BOTH the physical toplevel and the
;; controlling repository identity (git common dir). #f when dir is not
;; inside a usable work tree (bare repositories included: delivery requires
;; a work tree).
(struct git-root-probe (toplevel common-dir stderr) #:transparent)

(define (probe-repository dir)
  (define r (run-git-probe dir))
  (if (and (integer? (car r)) (zero? (car r)))
      (let ([lines (string-split (string-trim (cadr r)) "\n")])
        (and (= 2 (length lines))
             (git-root-probe (path->string (string->path (list-ref lines 0)))
                             (path->string (string->path (list-ref lines 1)))
                             (string-trim (caddr r)))))
      (git-root-probe #f #f (string-trim (caddr r)))))

;; ============================================================
;; Realpath normalization
;; ============================================================

;; Fully resolve symlinks of an EXISTING path; for paths that do not exist
;; (synthetic fixtures, declared-but-absent targets) fall back to lexical
;; simplification. Both branches yield complete paths without trailing
;; separators so identity/containment comparisons are exact string work.
(define (normalized-path p)
  (define cp (path->complete-path p))
  (with-handlers ([exn:fail? (lambda (_) (simplify-path cp))])
    (simplify-path (resolve-path cp))))

(define (normalized-path-string p)
  (path->string (normalized-path p)))

(define (strip-trailing-separator s)
  (let loop ([s s])
    (if (and (> (string-length s) 1) (string-suffix? s "/"))
        (loop (substring s 0 (- (string-length s) 1)))
        s)))

(define (dir-string p)
  (strip-trailing-separator (normalized-path-string p)))

(define (same-real-path? a b)
  (string=? (dir-string a) (dir-string b)))

;; Resolved roots are Git's OWN absolute toplevel answers, normalized to
;; complete path objects — the physical checkout is the truth even when the
;; caller reached it through a symlinked path.
(define (as-root-path p)
  (simplify-path (path->complete-path p)))

(define (contained-in? target-string root-string)
  (or (string=? target-string root-string)
      (string-prefix? target-string (string-append root-string "/"))))

;; ============================================================
;; Marker classification (refusal diagnostics ONLY — never authority)
;; ============================================================

(define (git-root-marker? dir)
  (define g (build-path dir ".git"))
  (or (directory-exists? g) (file-exists? g)))

;; ============================================================
;; Resolution result
;; ============================================================

(struct git-root-resolution (kind root reason) #:transparent)
;; kind = 'resolved | 'refused | 'absent

(define (resolved-at root) (git-root-resolution 'resolved root #f))

(define (refused-at root reason) (git-root-resolution 'refused root reason))

(define absent-resolution (git-root-resolution 'absent #f #f))

;; Select the returned root path for an enclosing-repository resolution:
;; Git's absolute toplevel (physical checkout truth).
(define (selected-toplevel-path toplevel-string _start)
  (as-root-path toplevel-string))

;; Step 1 of the pinned algorithm: an explicit authenticated delivery root is
;; used only when real git proves realpath identity with its own toplevel.
(define (explicit-root-resolution ctx-root)
  (define probe (probe-repository ctx-root))
  (cond
    [(not (git-root-probe-toplevel probe))
     (refused-at ctx-root
                 (format "explicit delivery root is not a usable git work tree: ~a"
                         (git-root-probe-stderr probe)))]
    [(not (same-real-path? (git-root-probe-toplevel probe) ctx-root))
     (refused-at ctx-root
                 (format "explicit delivery root does not match its git toplevel ~a"
                         (git-root-probe-toplevel probe)))]
    [else (resolved-at (as-root-path ctx-root))]))

;; Step 2/3: layout resolution from Git's own answers for <start> and
;; <start>/q. See the module header for the case matrix.
(define (layout-resolution start)
  (define q-path (build-path start "q"))
  (define q-exists? (directory-exists? q-path))
  (define outer (probe-repository start))
  (define nested (and q-exists? (probe-repository q-path)))
  (define outer-ok? (and (git-root-probe-toplevel outer) #t))
  (define nested-ok? (and nested (git-root-probe-toplevel nested) #t))
  (define marker-start? (git-root-marker? start))
  (define marker-q? (and q-exists? (git-root-marker? q-path)))
  (cond
    ;; Both probes found repositories.
    [(and outer-ok? nested-ok?)
     (if (string=? (git-root-probe-common-dir outer) (git-root-probe-common-dir nested))
         ;; Same controlling repository: q is a plain subdirectory or a linked
         ;; worktree of the start repository. One identity — no ambiguity.
         (resolved-at (selected-toplevel-path (git-root-probe-toplevel outer) start))
         ;; Two independent repositories (decoy outer + nested q): ambiguous.
         ;; Refuse with root+reason; never treat the directory name as authority.
         (refused-at start
                     (format
                      (string-append
                       "ambiguous repository identity: ~a resolves inside repository ~a (git common dir ~a) "
                       "while ~a is an independent repository ~a (git common dir ~a); "
                       "refusing rather than treating a directory name or marker as authority")
                      (dir-string start)
                      (git-root-probe-toplevel outer)
                      (git-root-probe-common-dir outer)
                      (dir-string q-path)
                      (git-root-probe-toplevel nested)
                      (git-root-probe-common-dir nested))))]
    ;; Start inside a repository; q is not an independent repository.
    [outer-ok?
     (if marker-q?
         (refused-at q-path
                     (format "q/.git marker present but not a usable git work tree: ~a"
                             (git-root-probe-stderr nested)))
         (resolved-at (selected-toplevel-path (git-root-probe-toplevel outer) start)))]
    ;; Start outside any repository; q probed successfully.
    [nested-ok?
     (cond
       [marker-start?
        (refused-at start
                    (format ".git marker present but not a usable git work tree: ~a"
                            (git-root-probe-stderr outer)))]
       ;; Two-tier recognition requires the nested repository's PHYSICAL
       ;; toplevel to be exactly <start>/q (validated, not named).
       [(same-real-path? (git-root-probe-toplevel nested) q-path)
        (resolved-at q-path)]
       [else
        (refused-at start
                    (format "~a resolves inside unrelated repository ~a; not a recognized nested-q layout"
                            (dir-string q-path)
                            (git-root-probe-toplevel nested)))])]
    ;; No repository anywhere.
    [else
     (cond
       [marker-start?
        (refused-at start
                    (format ".git marker present but not a usable git work tree: ~a"
                            (git-root-probe-stderr outer)))]
       [marker-q?
        (refused-at q-path
                    (format "q/.git marker present but not a usable git work tree: ~a"
                            (git-root-probe-stderr nested)))]
       [else absent-resolution])]))

;; The pure entry point: (resolve-delivery-root start #:branch-context ctx)
;; -> git-root-resolution.
(define (resolve-delivery-root start-dir #:branch-context [ctx #f])
  (define start
    (path->complete-path
     (cond
       [(path? start-dir) start-dir]
       [(string? start-dir) (string->path start-dir)]
       [else (raise-argument-error 'resolve-delivery-root "(or/c path? string?)" start-dir)])))
  (define ctx-root (and ctx (hash-ref ctx 'repo-root #f)))
  (if ctx-root
      ;; Explicit authenticated context wins; unverifiable → refused,
      ;; never fall through to layout sniffing.
      (explicit-root-resolution ctx-root)
      (layout-resolution start)))

;; Adapter for the classic (path-or-#f) resolver contracts: resolved → path,
;; refused → typed raise, absent → #f (the unchanged no-repo answer).
(define (delivery-root-or-raise start-dir #:branch-context [ctx #f])
  (define res (resolve-delivery-root start-dir #:branch-context ctx))
  (case (git-root-resolution-kind res)
    [(resolved) (git-root-resolution-root res)]
    [(refused)
     (raise (git-root-refusal (git-root-resolution-root res)
                              (git-root-resolution-reason res)))]
    [else #f]))

;; Public explicit-root validation (used by the verifier's evidence root and
;; the publication selector): returns the validated root path or raises the
;; typed refusal.
(define (resolve-explicit-delivery-root! ctx-root)
  (define res (explicit-root-resolution ctx-root))
  (case (git-root-resolution-kind res)
    [(resolved) (git-root-resolution-root res)]
    [else
     (raise (git-root-refusal (git-root-resolution-root res)
                              (git-root-resolution-reason res)))]))

;; ============================================================
;; Target containment
;; ============================================================

;; Every declared target must resolve INSIDE the selected delivery root.
;; A declared path is accepted when either anchor lands inside the root:
;;   - root-relative ("docs/x.md" with root <base>/q), or
;;   - base-relative ("q/docs/x.md" authored against the planning root),
;; mirroring wave-file->git-relative's dual convention. Escapes ("../"),
;; absolute outside paths and symlinked targets that leave the root refuse
;; with a diagnostic naming the root — never an implicit root switch.
(define (require-delivery-target-in-root! root target
                                          #:base [base #f]
                                          #:what [what "delivery target"])
  (define root-s (dir-string root))
  (define raw
    (cond
      [(path? target) target]
      [(string? target) (string->path (string-trim target))]
      [else (raise-argument-error 'require-delivery-target-in-root! "(or/c path? string?)" target)]))
  ;; An absolute declared target is used verbatim; a relative one is anchored
  ;; at the root (or, for the base-relative q/… convention, at the base).
  (define (anchored anchor)
    (normalized-path-string
     (if (absolute-path? raw) raw (build-path anchor raw))))
  (if (or (contained-in? (anchored root) root-s)
          (and base (contained-in? (anchored base) root-s)))
      (void)
      (raise (git-root-refusal
              root
              (format "~a ~a resolves outside the delivery root ~a; refusing without switching roots"
                      what
                      (if (path? raw) (path->string raw) raw)
                      root-s)))))
