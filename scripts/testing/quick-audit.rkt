#lang racket/base

;; scripts/testing/quick-audit.rkt — pure-unit admission for `./qt quick`
;; (final plan §3, rev 3 "capability trigger").
;;
;; Admission has three pieces:
;;
;; 1. THE REVIEWED OWNER TABLE (`admitted-owners` below). A deliberate,
;;    human-reviewed list of test files whose closures are pure decision code.
;;    This is the review surface: adding or removing an owner is a PR diff.
;;    It stays small enough that `quick` remains the fast loop (§9: the
;;    complete pure-unit loop targets warm p90 ≤ 90 s).
;;
;; 2. THE CAPABILITY DENYLIST SCAN (`denylist-version`, `scan-file`): a fixed,
;;    versioned vocabulary — filesystem write/delete/rename, process spawn,
;;    network, eval / dynamic-require of non-reviewed modules, home/temp
;;    mutation — scanned over each owner AND its reviewed closure. A hit is
;;    NEVER an approval decision: it sets review-required with the matched
;;    capability and location, and `quick` refuses to execute a
;;    review-required owner.
;;
;; 3. THE GENERATED ADMISSION RECORD (`tests/.quick-admission.rktd`): per
;;    owner, the dependency closure (repo-relative), the SHA-1 (distribution-available, staleness marker only) of every
;;    closure file at generation time, and the scan verdict. The record is
;;    committed, so a refresh is a reviewed diff. Ordinary content changes are
;;    cheap: `refresh` re-scans and re-admits automatically when the structure
;;    is unchanged and the scan is clean. A stale record (source or closure
;;    changed since generation), a structure change, or a denylist hit is
;;    review-required in both the audit report and the execution path — never
;;    silently accepted (§9).
;;
;; Dependency discovery reuses the repository's existing syntax-based
;; extractor (`extract-requires` from scripts/testing/changed-selection.rkt),
;; loaded lazily so `./qt quick`'s plain path gains no `json` dependency
;; (same pattern as full-plan.rkt's impact machinery). Reviewed code is
;; trusted, not sandboxed; the audit is a review-discipline gate, not a
;; defense against malicious code that forges result evidence.

(require file/sha1
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string)

(provide current-admitted-owners
         admitted-owners
         denylist-version
         (struct-out admission-state)
         owner-admission-state
         audit-ok?
         audit-report
         audit-report-lines
         refresh-record!
         record-path
         scan-file
         closure-of
         module-symbols-and-requires)

;; ============================================================
;; The reviewed owner table (human review surface)
;; ============================================================

;; Repo-relative test file paths. Every entry was reviewed as a pure decision
;; test at admission: no process spawn, no filesystem/network access in the
;; file or its reviewed closure, no eval/dynamic-require of non-reviewed
;; modules, and repository metadata that agrees (@boundary unit, @speed fast,
;; no @requires beyond gateable tokens, no @mutates/@isolation).
(define current-admitted-owners
  (make-parameter ;; The four original audited owners (their closures were re-reviewed as
   ;; part of the §3 expansion; fingerprints became the generated record).
   (list "tests/test-exn-recovery.rkt"
         "tests/test-vdom-html-render.rkt"
         "tests/test-gsd-campaign-decision-pure.rkt"
         "tests/test-gsd-delivery-verifier-decision.rkt"
         ;; §3 expansion batch 1: the closure-clean, metadata-clean PREFIX of the
         ;; unit-fast candidate list (deterministic; expanding the table is a PR
         ;; diff). Every entry and its transitive closure was denylist-scanned
         ;; clean at admission; the generated record carries the exact per-owner
         ;; closure and hashes.
         "tests/test-adr-0023-phase2.rkt"
         "tests/test-agent-loop-fsm.rkt"
         "tests/test-agent-memory-decoupling.rkt"
         "tests/test-agent-queue.rkt"
         "tests/test-agent-session-config.rkt"
         "tests/test-agent-state.rkt"
         "tests/test-ansi.rkt"
         "tests/test-anthropic-helpers.rkt"
         "tests/test-approval-broker.rkt"
         "tests/test-approval-channel.rkt"
         "tests/test-arch-boundaries.rkt"
         "tests/test-arch-composition-root.rkt"
         "tests/test-arch-parameters.rkt"
         "tests/test-arch-tool-constructor.rkt"
         "tests/test-audit-v09945-final-report.rkt"
         "tests/test-audit-v09945-w11-final.rkt"
         "tests/test-audit-v09945-w3-gsd.rkt"
         "tests/test-blackboard-reducer-w08.rkt"
         "tests/test-blackboard-reducer.rkt"
         "tests/test-blackboard.rkt"
         "tests/test-browser-adapter-contracts.rkt"
         "tests/test-browser-adapter.rkt"
         "tests/test-browser-close-contract.rkt"
         "tests/test-browser-errors.rkt"
         ;; W5 (#9953) bounded pure pilot, candidate 2: the quick child-run
         ;; trust classifier. classify-quick-child-run decides over
         ;; already-captured evidence values; its closure carries two
         ;; reviewed capability hits (see reviewed-hits), both owned by the
         ;; production runner modules and off the decision paths this
         ;; owner executes.
         "tests/test-qt-quick-run-classification.rkt"
         ;; R3c (#9948): the W3 pure-production pilot owner moves to its
         ;; extracted pure module (delivery-outcome.rkt); its closure drops
         ;; from the delivery-coordinator graph to one near-zero-hit module.
         "tests/test-gsd-delivery-stop-decision.rkt")))

;; The owner set as a function (parameterized for fixture tests; the
;; production value is the reviewed table).
(define (admitted-owners)
  (current-admitted-owners))

;; Human review dispositions for capability hits in reviewed closures (§3.2:
;; "require renewed review when prohibited capabilities appear"). The scan can
;; only FLAG — it never approves; approval requires exactly one entry here,
;; per (owner capability closure-file), with the review reason. This table is
;; part of the PR review surface, in the same diff that reports the hit.
;; Tightening the denylist invalidates every disposition (denylist-version).
(define reviewed-hits
  (hash
   "tests/test-vdom-html-render.rkt"
   (list
    (list
     "dynamic-require"
     "tui/char-width.rkt"
     "stdlib-only grapheme-span probe with a safe fallback; racket/string is in the reviewed distribution, no repo code loads dynamically")
    (list
     "subprocess"
     "tui/clipboard.rkt"
     "clipboard helper not on the render-decision paths this test executes; subprocess capability is owned by the production module")
    (list
     "system"
     "tui/render/message-layout.rkt"
     "system symbols in this closure are contract/message-kind names and terminal capability modules not exercised by the render-decision tests")
    (list
     "system"
     "tui/terminal-native.rkt"
     "terminal-control helper present in the closure but not on the render-decision paths this test executes")
    (list
     "system"
     "ui-core/conversation-artifact.rkt"
     "message-kind names and terminal capability modules not exercised by the render-decision tests"))
   "tests/test-gsd-campaign-decision-pure.rkt"
   (list
    (list
     "delete-directory/files"
     "extensions/gsd/plan-snapshot.rkt"
     "planning-artifact housekeeping helper present in the closure; the decision functions under test never invoke it")
    (list
     "make-directory*"
     "extensions/gsd/wave-docs.rkt"
     "wave-docs writer helper present in the closure; the decision functions under test never invoke it"))
   "tests/test-gsd-delivery-verifier-decision.rkt"
   (list
    (list
     "subprocess"
     "tests/test-gsd-delivery-verifier-decision.rkt"
     "binds a define-runtime-path constant for closure authentication; no subprocess is spawned by the decision functions under test"))
   "tests/test-qt-quick-run-classification.rkt"
   (list
    (list
     "subprocess"
     "sandbox/subprocess.rkt"
     "subprocess bindings/constants in the production runner module; the classifier decides over captured evidence values and never spawns")
    (list
     "make-temporary-file"
     "scripts/testing/quick-runner.rkt"
     "evidence-directory helper inside run-one, the effectful child runner; the classifier consumes evidence values, never the filesystem"))))

(define (reviewed-hit? owner capability hit-file)
  (and capability
       hit-file
       (for/or ([entry (in-list (hash-ref reviewed-hits owner '()))])
         (and (equal? (first entry) capability) (equal? (second entry) hit-file)))))

(define denylist-version 1)

(define filesystem-write-symbols
  '(call-with-output-file open-output-file
                          with-output-to-file
                          call-with-atomic-output-file
                          display-to-file
                          print-to-file
                          println-to-file
                          write-to-file
                          copy-file
                          delete-file
                          delete-directory
                          delete-directory/files
                          rename-file-or-directory
                          make-directory
                          make-directory*
                          make-parent-directory*
                          make-temporary-file
                          make-temp-file
                          make-lock-file-name
                          find-share-path
                          file-or-directory-permissions
                          copy-directory/files
                          put-preferences
                          set-pref!
                          preferences-port))

(define process-symbols
  '(system system*
           process
           process*
           subprocess
           subprocess-worker
           process/ports
           process*/ports
           process*-ports
           run-subprocess
           subprocess-group-enabled
           current-subprocess-custodian-mode
           shell-command
           split-command-line))

(define network-symbols
  '(tcp-connect tcp-connect/enable-break
                tcp-listen
                tcp-accept
                udp-open-socket
                udp-connect!
                udp-send-to
                http-send-soak
                make-http-connection
                http-conn-send!))

(define eval-dynamic-symbols
  '(eval eval-syntax
         dynamic-require
         dynamic-require-for-syntax
         dynamic-set!
         load
         load-extension
         load-relative-extension
         instantiate-module
         variable-reference->module-source
         module-compiled-imports))

;; Collection-level capability requires: requiring one of these modules is
;; itself the capability (network / process / eval), regardless of call sites.
(define capability-requires
  '(net/netrc net/ftp
              net/url
              net/head
              net/sendurl
              net/mime
              racket/system
              file/gunzip-no
              racket/tcp
              json-network
              mzlib/os))

(define (denylist-hit-symbol? sym)
  (or (memq sym filesystem-write-symbols)
      (memq sym process-symbols)
      (memq sym network-symbols)
      (memq sym eval-dynamic-symbols)))

;; ============================================================
;; Read-syntax symbol scan
;; ============================================================

;; Strip #lang/#reader/shebang LINES (plain `read` rejects #lang inside a
;; module context and enabling read-accept-lang would load reader plugins),
;; then walk the datums collecting symbols. Strings are skipped: a capability
;; name inside a string literal (a test's expected message) is not a
;; capability.
(define (module-symbols-and-requires source)
  (define text (file->string source))
  ;; Strip reader directives from the FILE HEAD only: blank and comment
  ;; lines may precede them, and a file may carry a shebang AND a #lang
  ;; line. Stop at the first line that is neither blank, comment, nor
  ;; directive. Later #lang lines are data inside string literals
  ;; (generated fixture sources), and stripping those corrupts the stream.
  ;; Dropped blank/comment lines are irrelevant to the reader.
  (define directive-line? (lambda (line) (regexp-match? #px"^[ \t]*(#lang|#reader|#!)" line)))
  (define skippable-line?
    (lambda (line)
      (or (zero? (string-length (string-trim line))) (string-prefix? (string-trim line) ";"))))
  (define (head-strip ls)
    (cond
      [(null? ls) ls]
      [(directive-line? (car ls)) (head-strip (cdr ls))]
      [(skippable-line? (car ls)) (head-strip (cdr ls))]
      [else ls]))
  (define port (open-input-string (string-join (head-strip (string-split text "\n")) "\n")))
  ;; Pure datum walk: collects symbols and require specs from ONE datum
  ;; without touching the shared port (a dotted pair used to recurse with
  ;; the reader continuation and corrupt the stream).
  (define (walk d syms requires depth)
    (cond
      [(> depth 400) (values syms requires)]
      ;; Quoted DATA is not code: a quoted 'system message-kind atom is not
      ;; a capability. The (require ...) check above still runs first, and
      ;; the head symbol of a dynamic-require form is flagged on descent.
      [(and (pair? d)
            (memq (car d) (list (quote quote) (quote quasiquote) (quote syntax) (quote quasisyntax))))
       (values syms requires)]
      [(and (pair? d) (list? (cdr d)) (eq? (car d) 'require))
       (define specs
         (for/list ([spec (in-list (cdr d))]
                    #:when (or (string? spec) (and (pair? spec) (symbol? (car spec)))))
           (if (string? spec)
               spec
               (car spec))))
       (values syms (append specs requires))]
      [(and (pair? d) (not (list? d)))
       (define-values (s1 r1) (walk (car d) syms requires (add1 depth)))
       (walk (cdr d) s1 r1 (add1 depth))]
      [(pair? d)
       ;; Proper lists iterate FLAT so a long provide form cannot exhaust
       ;; any budget (same shape discipline as changed-selection.rkt).
       (for/fold ([s syms]
                  [r requires])
                 ([x (in-list d)])
         (walk x s r (add1 depth)))]
      [(vector? d)
       (for/fold ([s syms]
                  [r requires])
                 ([x (in-vector d)])
         (walk x s r (add1 depth)))]
      [(box? d) (walk (unbox d) syms requires (add1 depth))]
      [(hash? d)
       (for/fold ([s syms]
                  [r requires])
                 ([(k v) (in-hash d)])
         (define-values (s1 r1) (walk k s r (add1 depth)))
         (walk v s1 r1 (add1 depth)))]
      [(symbol? d) (values (cons d syms) requires)]
      [else (values syms requires)]))
  (let read-loop ([syms (list)]
                  [requires (list)])
    (define d (read port))
    (cond
      [(eof-object? d) (values (reverse syms) (reverse requires))]
      [else
       (define-values (s r) (walk d syms requires 0))
       (read-loop s r)])))
;; One scan result: (values hit? capability line) — line numbers are the
;; best-effort match position; the record stores the capability + the file
;; where the hit occurred.
(define (scan-file path)
  ;; NOTE: for/first bodies return the matched datum only — the triple is
  ;; assembled here (a `(values ...)` body inside for/first raises an arity
  ;; error that the fail-closed handler would report as unreadable-source).
  (with-handlers ([exn:fail? (lambda (e) (values #t 'unreadable-source 0))])
    (define-values (syms requires) (module-symbols-and-requires path))
    (define sym-hit
      (for/first ([s (in-list syms)]
                  #:when (denylist-hit-symbol? s))
        s))
    (define req-symbol-hit
      (for/first ([r (in-list requires)]
                  #:when (and (symbol? r) (memq r capability-requires)))
        r))
    (define req-string-hit
      (for/first ([r (in-list requires)]
                  #:when (and (string? r)
                              (or (string-contains? r "net/") (string-prefix? r "racket/tcp"))))
        (string->symbol r)))
    (cond
      [sym-hit (values #t sym-hit 0)]
      [req-symbol-hit (values #t req-symbol-hit 0)]
      [req-string-hit (values #t req-string-hit 0)]
      [else (values #f #f 0)])))

;; ============================================================
;; Dependency closure (existing extractor, lazily loaded)
;; ============================================================

(define-runtime-path changed-selection-module "changed-selection.rkt")
(define-runtime-path classify-module "../run-tests/classify.rkt")

(define (extract-requires* root source)
  (define binding
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dynamic-require changed-selection-module 'extract-requires)))
  (unless (procedure? binding)
    (error 'quick-audit
           "dependency discovery unavailable: cannot load extract-requires from ~a (fail closed)"
           changed-selection-module))
  (define result (binding root source))
  (values (car result) (cdr result)))

;; Transitive repo-relative closure of one repo-relative test file, cycle-safe,
;; deterministic (sorted, deduplicated). A parse error anywhere in the closure
;; raises (fail closed — an unreadable closure cannot be reviewed).
(define (closure-of root owner-rel)
  (define visited (make-hash))
  (let walk ([rel owner-rel])
    (unless (hash-has-key? visited rel)
      (hash-set! visited rel #t)
      (define abs (build-path root rel))
      (unless (file-exists? abs)
        (error 'quick-audit "closure member does not exist: ~a" rel))
      ;; The reviewed closure is bound file-by-file: a symlink anywhere in it
      ;; escapes the review boundary and is refused (the old fingerprint
      ;; audit's link-free contract, kept verbatim).
      (when (or (link-exists? abs)
                (let ([p (simplify-path (path->complete-path abs) #f)])
                  (ormap link-exists? (explode-path p))))
        (error 'quick-audit "closure member is a symlink: ~a" rel))
      (define-values (targets status) (extract-requires* root abs))
      (unless (eq? status 'ok)
        (error 'quick-audit "closure member does not parse: ~a" rel))
      (for ([t (in-list targets)])
        (walk t))))
  (sort (hash-keys visited) string<?))

;; ============================================================
;; Admission record (generated; committed; reviewable)
;; ============================================================

(define-runtime-path default-record "tests/.quick-admission.rktd")

;; Root override for fixture tests; the production record lives at the repo
;; root's tests/.quick-admission.rktd.
(define (record-path [root #f])
  (if root
      (build-path root "tests" ".quick-admission.rktd")
      default-record))

(define (closure-hash path)
  (call-with-input-file path sha1))

(define (build-owner-entry root owner-rel)
  (define closure (closure-of root owner-rel))
  (define hashes
    (for/list ([rel (in-list closure)])
      (cons rel (closure-hash (build-path root rel)))))
  (define-values (hit? capability hit-file)
    (for/fold ([hit? #f]
               [capability #f]
               [hit-file #f])
              ([rel (in-list closure)])
      (if hit?
          (values hit? capability hit-file)
          (call-with-values (lambda () (scan-file (build-path root rel)))
                            (lambda (h? c _l) (values h? c rel))))))
  (cons owner-rel
        (list (cons 'status (if hit? 'review-required 'admitted))
              (cons 'capability (and capability (format "~a" capability)))
              (cons 'hit-file (and hit-file (format "~a" hit-file)))
              (cons 'closure closure)
              (cons 'hashes hashes))))

(define (fresh-record root)
  (list (cons 'record-version 1)
        (cons 'denylist-version denylist-version)
        (cons 'owners
              (for/list ([owner (in-list (admitted-owners))])
                (build-owner-entry root owner)))))

;; Regenerate the record over `root`. Content-only changes re-admit
;; automatically (scan clean + structure stable); anything else records
;; review-required and stays refused until a human resolves it. The refresh is
;; a committed diff — the reviewed-diff requirement of §9.
(define (refresh-record! [root #f])
  (define root-dir (or root (current-directory)))
  (define record (fresh-record root-dir))
  (define path (record-path root-dir))
  (make-parent-directory* path)
  (call-with-output-file path
                         #:exists 'truncate
                         (lambda (out)
                           (write record out)
                           (newline out)))
  path)

(define (load-record [root #f])
  (define path (record-path root))
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file path read)))

(define (record-owner-entry record owner-rel)
  (for/first ([entry (in-list (cdr (assoc 'owners record)))]
              #:when (equal? (car entry) owner-rel))
    (cdr entry)))

;; ============================================================
;; Admission states (pure; the report and the execution gate share this)
;; ============================================================
;;
;;   'admitted        — table member, record current, scan clean
;;   'review-required — table member whose record is stale, structurally
;;                      drifted, denylist-hit, or missing entirely
;;   'rejected        — not an owner: metadata or purity contract excludes it

(struct admission-state (owner status reason) #:transparent)

(define (hashes-match? entry root)
  (define hashes (cdr (assoc 'hashes entry)))
  (andmap (lambda (pair)
            (define rel (car pair))
            (define abs (build-path root rel))
            (and (file-exists? abs) (equal? (cdr pair) (closure-hash abs))))
          hashes))

(define (owner-admission-state root owner-rel [record (load-record root)])
  (cond
    [(not (member owner-rel (admitted-owners)))
     (admission-state owner-rel 'rejected "not an admitted owner (review the table to admit it)")]
    [(not record)
     (admission-state owner-rel 'review-required "admission record missing or unreadable")]
    [(not (equal? (cdr (assoc 'denylist-version record)) denylist-version))
     (admission-state owner-rel 'review-required "denylist version changed; renewed review required")]
    [else
     (define entry (record-owner-entry record owner-rel))
     (cond
       [(not entry)
        (admission-state owner-rel
                         'review-required
                         "owner missing from the record (refresh required)")]
       [else
        (define recorded-closure (cdr (assoc 'closure entry)))
        (cond
          ;; A closure that cannot be recomputed (missing/unparseable members,
          ;; a fixture root that is not the reviewed tree) is fail-closed
          ;; review-required, never an exception escaping the audit.
          [(not (with-handlers ([exn:fail? (lambda (_) #f)])
                  (equal? recorded-closure (closure-of root owner-rel))))
           (admission-state
            owner-rel
            'review-required
            "dependency structure changed or is not recomputable since the record was generated")]
          [(not (hashes-match? entry root))
           (admission-state owner-rel
                            'review-required
                            "source changed since the record was generated (refresh required)")]
          [(eq? (cdr (assoc 'status entry)) 'review-required)
           (define cap (cdr (assoc 'capability entry)))
           (define hit-file (cdr (assoc 'hit-file entry)))
           (if (reviewed-hit? owner-rel cap hit-file)
               (admission-state owner-rel 'admitted #f)
               (admission-state
                owner-rel
                'review-required
                (format "denylist hit: ~a in ~a (renewed review required)" cap hit-file)))]
          [else (admission-state owner-rel 'admitted #f)])])]))

;; Execution gate: exactly the admitted state is executable; everything else
;; refuses. `quick` calls this before and after every run.
(define (audit-ok? root owner-rel)
  (eq? (admission-state-status (owner-admission-state root owner-rel)) 'admitted))

;; ============================================================
;; Audit report (`./qt quick --audit`)
;; ============================================================

;; Rejected candidates get an honest reason: the same repository metadata the
;; fast/PR lane filter reads (one vocabulary, no second classification system)
;; plus the purity-capability scan. Reported for the audit view only; a
;; rejected owner is never executable.
(define (rejected-reason root rel)
  (cond
    [(not (member rel (admitted-owners)))
     (define cap
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define-values (hit? capability _line) (scan-file (build-path root rel)))
         (and hit? (format "~a" capability))))
     (if cap
         (format "capability scan hit: ~a" cap)
         "not in the reviewed owner table (see docs/TEST_CONVENTIONS.md)")]
    [else #f]))

(define (audit-report root)
  (define record (load-record root))
  (define states
    (append (for/list ([owner (in-list (admitted-owners))])
              (owner-admission-state root owner record))
            ;; Rejected side: every other discovered test file, with its reason.
            (let ()
              (define collect (dynamic-require classify-module 'collect-test-files))
              (define all
                (with-handlers ([exn:fail? (lambda (_) '())])
                  (collect 'all #:root root)))
              (for/list ([rel (in-list all)]
                         #:unless (member rel (admitted-owners)))
                (admission-state rel 'rejected (rejected-reason root rel))))))
  states)

(define (audit-report-lines root)
  (define states (audit-report root))
  (define admitted (filter (lambda (s) (eq? (admission-state-status s) 'admitted)) states))
  (define review (filter (lambda (s) (eq? (admission-state-status s) 'review-required)) states))
  (define rejected (filter (lambda (s) (eq? (admission-state-status s) 'rejected)) states))
  (append
   (list (format
          "quick admission audit (denylist version ~a): ~a admitted, ~a review-required, ~a rejected"
          denylist-version
          (length admitted)
          (length review)
          (length rejected)))
   (for/list ([s (in-list admitted)])
     (format "  admitted: ~a" (admission-state-owner s)))
   (for/list ([s (in-list review)])
     (format "  review-required: ~a (~a)" (admission-state-owner s) (admission-state-reason s)))
   ;; Rejected owners are the interesting rejection cases (they carry a
   ;; capability reason); unadmitted plain files are summarized.
   (for/list ([s (in-list rejected)]
              #:when (string-prefix? (admission-state-reason s) "capability"))
     (format "  rejected: ~a (~a)" (admission-state-owner s) (admission-state-reason s)))
   (list (format "  rejected: ~a other file(s) are not in the reviewed owner table"
                 (length (filter (lambda (s)
                                   (not (string-prefix? (admission-state-reason s) "capability")))
                                 rejected))))))
