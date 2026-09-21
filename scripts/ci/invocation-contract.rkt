#lang racket/base

;; q/scripts/ci/invocation-contract.rkt — v1.00.31 W1
;;
;; Declared-invocation extractor/validator for `.github/`.
;;
;; A workflow step or action that declares `racket <script> <flags...>`
;; must declare an invocation the target script can actually execute.
;; Register F1 was invisible to a substring assertion: the declaration
;; passed `--out`, the script never accepted `--out`, and the suite went
;; green because it only checked that the *string* "compiled-root.rkt
;; build" appeared in the file.
;;
;; Verification tiers, from strongest to weakest. Every declaration ends
;; in exactly one of these, and the matrix records which:
;;
;;   option-set          the target declares its flags via `command-line`
;;                       and every declared flag is in that set (exact)
;;   literal-vocabulary  the target parses arguments by hand, so the
;;                       declared flags are checked against the `--flag`
;;                       string literals it (or a local module it
;;                       requires) actually mentions (heuristic)
;;   external-provisioned the target lives under a directory a preceding
;;                       checkout step provisions (e.g. a trusted verifier
;;                       checkout into `tooling/`); it cannot exist in this
;;                       repository by construction
;;   not-a-script        `racket -e …` / `racket --version` / `raco pkg …`
;;   unverifiable        none of the above; an error unless explicitly
;;                       recorded in `unverifiable-targets` with a reason
;;
;; Fail closed: unparseable declarations, missing targets and
;; unverifiable flags are errors. The matrix states how each declaration
;; was resolved, so nobody has to take a green suite on faith.

(require racket/file
         racket/hash
         racket/list
         racket/path
         racket/string)

(provide github-declaration-files
         logical-lines
         shell-tokens
         extract-declared-invocations
         extract-all-declared-invocations
         script-option-set
         script-flag-vocabulary
         external-provisioned-roots
         resolve-variable-target
         check-invocation
         check-declared-invocations
         invocation-matrix
         unverifiable-targets
         option-flag?)

;; ------------------------------------------------------------ declaration files

;; Every YAML workflow/action under `.github/` (repo-relative strings,
;; deterministic order).
;; Entry-point normalization: `--root .` (or any relative root) must produce
;; byte-identical output to an absolute root. Without this, a `.` component
;; survives into `find-relative-path`, `source` fields come out as absolute
;; paths, and the emitted artifact stops being reproducible.
(define (normalize-root repo-root)
  (simplify-path (path->complete-path repo-root)))

(define (github-declaration-files repo-root)
  ;; `simplify-path` matters: a relative root such as `.` (the natural thing to
  ;; type by hand) otherwise leaves a `.` component in the base path that
  ;; breaks the downstream `find-relative-path`/`explode-path` walk. The
  ;; emitter test runs both a relative and an absolute root for that reason.
  (define abs (normalize-root repo-root))
  (define root (build-path abs ".github"))
  (unless (directory-exists? root)
    (error 'github-declaration-files "no .github directory under ~a" abs))
  (sort (for/list ([p (in-list (find-files (lambda (p)
                                             (and (file-exists? p)
                                                  (regexp-match? #rx"\\.(yml|yaml)$"
                                                                 (path->string p))))
                                           root))]
                   #:unless (member ".git" (map path->string (explode-path p))))
          (path->string (find-relative-path abs (path->complete-path p))))
        string<?))

;; ------------------------------------------------------------ shell line reading

(define (invocation-mention? s)
  (regexp-match? #rx"(^|[;&|(]|\\$\\() *(racket|raco) " s))

;; Logical lines of a YAML script body: backslash-continuations joined
;; into one line, carrying the line number of the FIRST fragment so a
;; finding points at the declaration a human would edit. Only logical
;; lines that mention `racket` or `raco` are returned.
(define (logical-lines text)
  (define out '())
  (define pending '())
  (define pending-line 0)
  (define (flush!)
    (when (pair? pending)
      (define joined (string-join (reverse pending) " "))
      (when (invocation-mention? joined)
        (set! out (cons (cons pending-line joined) out)))
      (set! pending '())))
  (for ([line (in-list (string-split text "\n"))]
        [i (in-naturals 1)])
    (define l (string-trim line))
    (define continuation? (regexp-match? #rx"\\\\$" l))
    (define fragment
      (if continuation?
          (string-trim (substring l 0 (sub1 (string-length l))))
          l))
    (cond
      [(and (null? pending) (not continuation?))
       (when (invocation-mention? l)
         (set! out (cons (cons i l) out)))]
      [else
       (when (null? pending)
         (set! pending-line i))
       (set! pending (cons fragment pending))
       (unless continuation?
         (flush!))]))
  (flush!)
  (reverse out))

;; Shell tokens with quotes stripped, so `--out "$stage/x"` yields the
;; value-bearing token without the quoting noise. These lines are flat
;; command invocations, not general shell programs, so this is enough.
;; The loop variable must NOT be called `quote`: Racket's reader reaches
;; `quote` for datum syntax, so shadowing it breaks `'()` in the same
;; scope (learned the hard way in this wave).
(define (shell-tokens s)
  (define toks '())
  (define cur (open-output-string))
  (define quote-char #f)
  (define (emit!)
    (define t (get-output-string cur))
    (unless (string=? t "")
      (set! toks (cons t toks)))
    (set! cur (open-output-string)))
  (for ([ch (in-string s)])
    (cond
      [(and (not quote-char) (or (char=? ch #\") (char=? ch #\'))) (set! quote-char ch)]
      [(and quote-char (char=? ch quote-char)) (set! quote-char #f)]
      [(and (not quote-char) (memv ch (list #\space #\tab))) (emit!)]
      [else (write-char ch cur)]))
  (emit!)
  (reverse toks))

;; A token that ends the invocation and starts a different command,
;; redirect or substitution.
(define (stop-token? t)
  (or (member t '("|" "||" "&&" ";" "&")) (regexp-match? #rx"[<>]" t)))

(define (option-flag? t)
  (and (string? t) (regexp-match? #rx"^--" t) (not (string=? t "--"))))

;; `raco pkg install`, `raco fmt --help`, … are built-in subcommands, not
;; declared scripts: only a target that looks like a file is a script.
(define (script-target-token? t)
  (and (string? t)
       (not (regexp-match? #rx"^-" t))
       (or (regexp-match? #rx"\\.rktl?$" t) (regexp-match? #rx"/" t))))

;; ------------------------------------------------------------ extraction

;; Extract every declared `racket`/`raco` invocation from one YAML file.
;; Returns a list of hashes: source line tool kind target argv flags raw.
(define (extract-declared-invocations path)
  (define text (file->string path))
  (define out '())
  (for ([entry (in-list (logical-lines text))])
    (define line (car entry))
    (define raw (cdr entry))
    (define toks (shell-tokens raw))
    (for ([t (in-list toks)]
          [idx (in-naturals)]
          #:when (or (string=? t "racket") (string=? t "raco")))
      (define after (drop toks (add1 idx)))
      (define argv
        (for/list ([a (in-list after)]
                   #:unless (stop-token? a))
          a))
      (define flags (filter option-flag? argv))
      ;; Never bind `first`/`rest` locally: the reader uses them too.
      (define head-arg (and (pair? argv) (first argv)))
      (define (kind-and-target)
        (cond
          [(not head-arg) (values "not-a-script" #f)]
          [(regexp-match? #rx"^-e" head-arg) (values "not-a-script" #f)]
          [(script-target-token? head-arg) (values "script" head-arg)]
          [else (values "not-a-script" #f)]))
      (define-values (kind target) (kind-and-target))
      (set! out
            (cons (hash 'source
                        path
                        'line
                        line
                        'tool
                        t
                        'kind
                        kind
                        'target
                        target
                        'argv
                        argv
                        'flags
                        flags
                        'raw
                        raw)
                  out))))
  (reverse out))

(define (extract-all-declared-invocations repo-root)
  (define abs (normalize-root repo-root))
  (append* (for/list ([rel (in-list (github-declaration-files repo-root))])
             (extract-declared-invocations (build-path abs rel)))))

;; ------------------------------------------------------------ option sets

;; The declared option set of a script, read from its own `command-line`
;; clauses. Returns (values (listof string) declared?).
;;
;; `[--flag value "help" (action)]` inside `(command-line …)` reads as a
;; list whose first element is the literal flag string, which is exactly
;; what command-line binds — so the contract cannot drift from the script
;; the way a parallel hand-maintained list would.
(define (script-option-set path)
  (define forms
    ;; `file->list` raises on a `#lang` line, which silently disabled the
    ;; strong tier for every script (caught in this wave by a test that
    ;; asserted `--out` was in the CLI's option set and saw an empty set).
    ;; Skip the optional shebang and the `#lang` line, then read the
    ;; remaining top-level forms explicitly.
    (let* ([text (with-handlers ([exn:fail? (lambda (_) "")])
                   (file->string path))]
           [no-shebang (regexp-replace #rx"^(#![^\n]*\n)?" text "")]
           [body (regexp-replace #rx"^(#lang [^\n]*\n)?" no-shebang "")])
      (with-handlers ([exn:fail? (lambda (_) '())])
        (let loop ([p (open-input-string body)]
                   [acc '()])
          (define v (read p))
          (if (eof-object? v)
              (reverse acc)
              (loop p (cons v acc)))))))
  (define switches '())
  (define declared? #f)
  (define (walk v)
    (cond
      [(list? v)
       (cond
         [(and (pair? v) (eq? (first v) 'command-line))
          (set! declared? #t)
          (for ([clause (in-list (rest v))])
            ;; `command-line` accepts both `["--flag" …]` and `[("--flag" "--alias") …]`,
            ;; and the second form is used in this repository — reading only the
            ;; bare-string shape silently reported those flags as unknown.
            (define flag-forms
              (cond
                [(and (pair? clause) (string? (first clause))) (list (first clause))]
                [(and (pair? clause) (list? (first clause))) (first clause)]
                [else '()]))
            (for ([f (in-list flag-forms)]
                  #:when (and (string? f) (regexp-match? #rx"^--" f)))
              (set! switches (cons f switches))))]
         [else (for-each walk v)])]
      [else (void)]))
  (for-each walk forms)
  (values (sort (remove-duplicates switches) string<?) declared?))

;; ------------------------------------------------------------ flag vocabulary

;; Every `--flag` string literal a file mentions. Used only when a target
;; parses arguments by hand and therefore declares no option set; it
;; catches the actual F1 failure mode (a flag the script never mentions at
;; all) without pretending to be an exact parser.
(define (file-flag-literals path)
  (define text
    (with-handlers ([exn:fail? (lambda (_) "")])
      (file->string path)))
  (remove-duplicates (for/list ([m (in-list (regexp-match* #rx"\"(--[A-Za-z0-9][A-Za-z0-9-]*)\""
                                                           text
                                                           #:match-select cadr))])
                       m)))

;; Local `(require "…")` strings of a script, resolved against its own
;; directory — one level, which is enough for the runner CLIs that keep
;; their option table in a sibling module.
(define (local-required-files path)
  (define text
    (with-handlers ([exn:fail? (lambda (_) "")])
      (file->string path)))
  (define dir (path-only (path->complete-path path)))
  ;; Every relative `.rkt` literal in the file, not only the first entry of a
  ;; require list: real require lists put one path per line, so anchoring on
  ;; `(require "` would see just the first module and miss the sibling that
  ;; actually declares the option table.
  (for/list ([rel (in-list (regexp-match* #rx"\"([A-Za-z0-9_][A-Za-z0-9_./-]*\\.rktl?)\""
                                          text
                                          #:match-select cadr))]
             #:unless (regexp-match? #rx"^/" rel))
    (build-path dir rel)))

(define (script-flag-vocabulary path)
  (remove-duplicates (append (file-flag-literals path)
                             (append* (for/list ([p (in-list (local-required-files path))])
                                        (file-flag-literals p))))))

;; ------------------------------------------------------------ external roots

;; Directories a preceding checkout step provisions in the same file
;; (`uses: actions/checkout@…` followed by `path: <dir>`). Declarations
;; under such a directory cannot resolve in this repository by
;; construction — they resolve against the trusted external checkout.
(define (external-provisioned-roots declaration-file)
  (define text
    (with-handlers ([exn:fail? (lambda (_) "")])
      (file->string declaration-file)))
  (define lines (string-split text "\n"))
  (remove-duplicates (for/fold ([roots '()])
                               ([line (in-list lines)]
                                [i (in-naturals)])
                       (if (regexp-match? #rx"uses: *actions/checkout@" line)
                           (let scan ([j (add1 i)]
                                      [n 0])
                             (cond
                               [(or (>= j (length lines)) (>= n 16)) roots]
                               [else
                                ;; Repo-relative roots only: an absolute `path:` (a scratch dir the
                                ;; step writes to) is not a checkout into the workspace.
                                (define m
                                  (regexp-match #rx"^ *path: *([A-Za-z0-9_][A-Za-z0-9_./-]*)"
                                                (list-ref lines j)))
                                (if m
                                    (cons (list-ref m 1) roots)
                                    (scan (add1 j) (add1 n)))]))
                           roots))))

(define (external-target? roots target)
  (for/or ([r (in-list roots)])
    (or (string=? r target) (string-prefix? target (string-append r "/")))))

;; ------------------------------------------------------------ variable targets

;; Resolve a `"$name"` target from a same-file shell assignment
;; (`name="scripts/ci/foo.rkt"`), so declarations routed through a shell
;; variable are still validated instead of being waved through. Returns a
;; path string, or #f when the value is itself dynamic (`$(…)`, `$OTHER`,
;; …) or no literal assignment exists.
(define (resolve-variable-target source-file target)
  (define name-match (regexp-match #rx"^\\$([A-Za-z_][A-Za-z0-9_]*) *$" target))
  (define name (and name-match (list-ref name-match 1)))
  (and name
       (let* ([text (with-handlers ([exn:fail? (lambda (_) "")])
                      (file->string source-file))]
              [assign (regexp (format "~a=\"?([^\"]+)\"?" (regexp-quote name)))]
              [matches (regexp-match* assign text #:match-select cadr)])
         (and (pair? matches)
              (let ([value (string-trim (last matches))])
                (and (not (regexp-match? #rx"[$`]" value)) value))))))

;; ------------------------------------------------------------ validation

;; Empty by construction: every declaration under `.github/` resolves to a
;; script with a declared option set, a script whose hand-rolled parser
;; mentions the flags, a `racket -e`/subcommand form, or a trusted external
;; checkout. Keep it empty unless a future declaration is genuinely
;; undecidable statically — an entry here is an admission that a
;; declaration is not machine-checked, so it must carry a reason and stay
;; visible in the emitted matrix.
(define unverifiable-targets '())

(define (exempt? result)
  (for/or ([e (in-list unverifiable-targets)])
    (and (equal? (hash-ref e 'source) (hash-ref result 'source))
         (equal? (hash-ref e 'target) (hash-ref result 'target)))))

(define (check-invocation inv repo-root)
  (define abs (normalize-root repo-root))
  (define source (hash-ref inv 'source))
  ;; Repo-relative reporting: an absolute path would make the emitted
  ;; artifact depend on where it was produced.
  (define source-path
    (if (path? source)
        source
        (string->path source)))
  (define source-rel
    (path->string (with-handlers ([exn:fail? (lambda (_) source-path)])
                    (find-relative-path abs (path->complete-path source-path)))))
  (define target (hash-ref inv 'target #f))
  (define flags (hash-ref inv 'flags '()))
  (define base
    (hash 'source
          source-rel
          'line
          (hash-ref inv 'line #f)
          'tool
          (hash-ref inv 'tool #f)
          'kind
          (hash-ref inv 'kind #f)
          'target
          target
          'flags
          flags))
  (define resolved
    (cond
      [(not (string? target)) #f]
      [(regexp-match? #rx"\\$" target) (resolve-variable-target source target)]
      [else target]))
  (define (merge2 a b)
    ;; Later values win: the tier/status keys override what `base` recorded.
    (hash-union a b #:combine (lambda (_x y) y)))
  (define (finish tier status extra)
    (merge2 base (merge2 (hash 'tier tier 'status status 'resolved resolved) extra)))
  (cond
    [(not (string? target)) (finish "not-a-script" "ok" (hash))]
    [(not resolved)
     (finish "unverifiable" "unverifiable-target" (hash 'detail (format "cannot resolve ~a" target)))]
    [(external-target? (external-provisioned-roots source-path) resolved)
     (finish "external-provisioned" "ok" (hash))]
    [else
     (define script (build-path abs resolved))
     (cond
       [(not (file-exists? script))
        (finish "none" "missing-target" (hash 'detail (format "no such file: ~a" resolved)))]
       [else
        (define-values (switches declared?) (script-option-set script))
        (cond
          [declared?
           (define unknown
             (for/list ([f (in-list flags)]
                        #:unless (member f switches))
               f))
           (finish "option-set"
                   (if (null? unknown) "ok" "unknown-flag")
                   (hash 'unknown-flags unknown 'option-set switches))]
          [else
           (define vocab (script-flag-vocabulary script))
           (define unknown
             (for/list ([f (in-list flags)]
                        #:unless (member f vocab))
               f))
           (cond
             [(null? unknown) (finish "literal-vocabulary" "ok" (hash))]
             [else
              (finish "literal-vocabulary"
                      "unverifiable-target"
                      (hash 'unknown-flags
                            unknown
                            'detail
                            (format "no declared option set and ~a absent from the flag vocabulary"
                                    (string-join unknown " "))))])])])]))

(define (failing-status? status)
  (member status '("unknown-flag" "missing-target" "unverifiable-target" "unparseable")))

;; Validate every declaration under `.github/`.
;; Returns (hash results errors): errors is empty when every declared
;; invocation resolved.
(define (check-declared-invocations repo-root)
  (define results
    (for/list ([inv (in-list (extract-all-declared-invocations repo-root))])
      (check-invocation inv repo-root)))
  (define errors
    (for/list ([r (in-list results)]
               #:when (and (failing-status? (hash-ref r 'status)) (not (exempt? r))))
      (format "~a:~a declared ~a but ~a"
              (hash-ref r 'source)
              (hash-ref r 'line)
              (or (hash-ref r 'target) "(no target)")
              (case (hash-ref r 'status)
                [("unknown-flag")
                 (format "the script does not accept: ~a"
                         (string-join (hash-ref r 'unknown-flags) " "))]
                [else (hash-ref r 'detail "unparseable declaration")]))))
  (hash 'results results 'errors errors))

;; ------------------------------------------------------------ matrix artifact

(define (matrix-key r k)
  (hash-ref r k))

;; JSON-ready, deterministic matrix: every declared invocation under
;; `.github/`, how it was resolved, and the exemptions in force.
(define (invocation-matrix repo-root)
  (define files (github-declaration-files repo-root))
  (define checked (check-declared-invocations repo-root))
  (define results
    (sort (for/list ([r (in-list (hash-ref checked 'results))])
            (hash 'source
                  (matrix-key r 'source)
                  'line
                  (matrix-key r 'line)
                  'tool
                  (matrix-key r 'tool)
                  'kind
                  (matrix-key r 'kind)
                  'target
                  (or (matrix-key r 'target) "")
                  'resolved
                  (or (matrix-key r 'resolved) "")
                  'flags
                  (matrix-key r 'flags)
                  'tier
                  (matrix-key r 'tier)
                  'status
                  (matrix-key r 'status)
                  'unknown-flags
                  (hash-ref r 'unknown-flags '())))
          (lambda (a b)
            (or (string<? (hash-ref a 'source) (hash-ref b 'source))
                (and (string=? (hash-ref a 'source) (hash-ref b 'source))
                     (< (hash-ref a 'line) (hash-ref b 'line)))))))
  (hash 'schema
        "invocation-matrix-1"
        'generated-by
        "scripts/ci/invocation-contract.rkt"
        'declaration-files
        files
        'declaration-file-count
        (length files)
        'invocation-count
        (length results)
        'tier-counts
        (for/fold ([h (hash)]) ([r (in-list results)])
          (define k (hash-ref r 'tier))
          (hash-set h k (add1 (hash-ref h k 0))))
        'status-counts
        (for/fold ([h (hash)]) ([r (in-list results)])
          (define k (hash-ref r 'status))
          (hash-set h k (add1 (hash-ref h k 0))))
        'unverifiable-targets
        (for/list ([e (in-list unverifiable-targets)])
          (hash 'source
                (hash-ref e 'source)
                'target
                (hash-ref e 'target)
                'reason
                (hash-ref e 'reason "unspecified")))
        'invocations
        results
        'errors
        (hash-ref checked 'errors)))

;; ------------------------------------------------------------ emitter
;;
;; The wave evidence artifact (`invocation-matrix.json`) is GENERATED from
;; this module rather than hand-written: a hand-maintained copy of a
;; verification report is exactly the drift the report exists to detect, and
;; a hand-written copy would also let the artifact say `ok` while the tool
;; says otherwise.
(define (jsexpr-value v)
  (cond
    ;; Hash KEYS become symbols: this Racket's `write-json` accepts symbol keys
    ;; only (it rejects string keys outright), and the extractor produces a mix
    ;; (structural keys are symbols; status/tier keys come from row values and
    ;; are strings). Symbols serialize as JSON strings either way.
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (cond
                 [(symbol? k) k]
                 [(string? k) (string->symbol k)]
                 [else k])
               (jsexpr-value val)))]
    [(list? v) (map jsexpr-value v)]
    [(symbol? v) (symbol->string v)]
    [else v]))

(module+ main
  (require json
           racket/cmdline)
  (define root (make-parameter (current-directory)))
  (define out (make-parameter #f))
  (command-line #:program "invocation-contract"
                #:once-each ["--root" r "checkout root to scan" (root r)]
                ["--out" p "write the matrix JSON to p (default: stdout)" (out p)]
                #:args ()
                (void))
  (define matrix-json (jsexpr-value (invocation-matrix (root))))
  (cond
    [(out)
     (call-with-output-file (out)
                            #:exists 'replace
                            (lambda (port)
                              (write-json matrix-json port)
                              (newline port)))]
    [else
     (write-json matrix-json (current-output-port))
     (newline)]))
