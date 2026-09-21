#lang racket/base

;; scripts/gsd-evidence-bind.rkt — The sanctioned author and verifier of the
;; schema-2 changed-content digest (register F2) and the record-commit purity
;; check (register F4).
;;
;; The digest is never authored by hand: `bind` recomputes the excluded-evidence
;; digest at an exact head and writes that computed value into the named
;; records; `verify` recomputes and refuses any authored, missing, or malformed
;; recorded value. `record-commit` refuses a wave whose evidence record was
;; authored in a commit that also touched non-record paths.
;;
;; Verdict contract: every decided verdict is printed on stdout with exit 0
;; (`digest-ok …`, `digest-mismatch: …`, `malformed-digest: …`, `pure`,
;; `impure-record-commit: …`, `bound …`, or a plain digest for `digest`). A
;; nonzero exit means the tool itself failed (bad arguments, git failure,
;; unparsable record) — callers fail closed on it.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         (only-in file/sha1 bytes->hex-string)
         openssl) ; sha256-bytes — same import contract as util/json/checksum.rkt

(provide exclusion-specs
         exclusion-prefixes
         empty-digest
         evidence-dir-prefix
         excluded-evidence-digest
         read-record-digest
         verify-records
         bind-records!
         record-commit-purity
         main)

(define empty-digest (bytes->hex-string (sha256-bytes (open-input-bytes #""))))

(define evidence-dir-prefix "docs/reports/gsd-wave-evidence/")
(define exclusion-prefixes
  (list "docs/reports/gsd-wave-evidence/"
        "docs/reports/gsd-wave-reviews/"
        "docs/reports/gsd-wave-validation/"))
;; Same contract as scripts/gsd-delivery.py digest(): the changed-content
;; digest of base...head excluding the three record directories.
(define exclusion-specs
  (list ":(exclude)docs/reports/gsd-wave-evidence/**"
        ":(exclude)docs/reports/gsd-wave-reviews/**"
        ":(exclude)docs/reports/gsd-wave-validation/**"))

(define sha-pattern #px"^[0-9a-f]{40}$")
(define digest-pattern #px"^[0-9a-f]{64}$")

(define (require-full-sha who kind value)
  (unless (and (string? value) (regexp-match? sha-pattern value))
    (error who "~a must be a full lowercase Git SHA, got: ~a" kind value)))

;; Byte-exact git runner: the digest hashes raw git stdout bytes, never a
;; decoded string, so binary diffs digest identically to the Python contract.
;; Git subprocesses run with a GIT_*-scrubbed environment: an ambient
;; GIT_DIR/GIT_INDEX_FILE/GIT_OBJECT_DIRECTORY (set by any outer git process,
;; e.g. a pre-commit hook in a linked worktree) would otherwise re-target
;; these subprocesses at a foreign repository and corrupt the verdict.
(define (hermetic-git-env)
  (define clean (make-environment-variables))
  (for ([kv (in-list (environment-variables-names (current-environment-variables)))]
        #:unless (regexp-match? #px"(?i:^git_)" kv))
    (environment-variables-set! clean
                                kv
                                (environment-variables-ref (current-environment-variables) kv)))
  clean)

(define (run/git/bytes repo . args)
  (define git-exe
    (or (find-executable-path "git") (error 'gsd-evidence-bind "git executable not found on PATH")))
  (define-values (proc stdout stdin stderr)
    (parameterize ([current-environment-variables (hermetic-git-env)])
      (apply subprocess #f #f #f git-exe "-C" (path->string (path->complete-path repo)) args)))
  (close-output-port stdin)
  (define out-bytes (port->bytes stdout))
  (define err-bytes (port->bytes stderr))
  (subprocess-wait proc)
  (close-input-port stdout)
  (close-input-port stderr)
  (values (subprocess-status proc) out-bytes err-bytes))

(define (excluded-evidence-digest repo base head)
  (require-full-sha 'gsd-evidence-bind "base" base)
  (require-full-sha 'gsd-evidence-bind "head" head)
  (define-values (code stdout stderr)
    (run/git/bytes repo
                   "diff"
                   "--binary"
                   (string-append base "..." head)
                   "--"
                   "."
                   (first exclusion-specs)
                   (second exclusion-specs)
                   (third exclusion-specs)))
  (unless (zero? code)
    (error 'gsd-evidence-bind "git diff failed: ~a" (string-trim (bytes->string/utf-8 stderr))))
  (bytes->hex-string (sha256-bytes (open-input-bytes stdout))))

(define (read-single-datum path)
  (call-with-input-file path
                        (lambda (in)
                          (parameterize ([read-accept-reader #f]
                                         [read-accept-lang #f]
                                         [read-accept-graph #f])
                            (define datum (read in))
                            (when (eof-object? datum)
                              (error 'gsd-evidence-bind "file is empty: ~a" path))
                            (unless (eof-object? (read in))
                              (error 'gsd-evidence-bind "file contains multiple datums: ~a" path))
                            datum))))

;; -> (values status value) with status in 'ok 'missing 'malformed 'unreadable.
;; Fail-closed: a record without a well-formed lowercase SHA-256 content-digest
;; can never verify.
(define (read-record-digest path)
  (cond
    [(not (file-exists? path)) (values 'unreadable #f)]
    [else
     (with-handlers ([exn:fail? (lambda (_) (values 'malformed #f))])
       (define datum (read-single-datum path))
       (unless (hash? datum)
         (error 'gsd-evidence-bind "record is not a hash: ~a" path))
       (define value (hash-ref datum 'content-digest #f))
       (cond
         [(not (string? value)) (values 'missing #f)]
         [(not (regexp-match? digest-pattern value)) (values 'malformed #f)]
         [else (values 'ok value)]))]))

(define (record-label path)
  ;; CLI callers pass strings; in-process callers may pass paths.
  (define text
    (if (path? path)
        (path->string path)
        path))
  (cond
    [(regexp-match? #rx"gsd-wave-evidence" text) "evidence"]
    [(regexp-match? #rx"gsd-wave-reviews" text) "review"]
    [(regexp-match? #rx"gsd-wave-validation" text) "validation"]
    [else text]))

;; The F2 verifier: recomputes at the exact head and refuses any recorded
;; value that differs, is missing, or is malformed. Returns the decided
;; verdict line (never raises for a refused digest).
(define (verify-records repo base head record-paths)
  (define computed (excluded-evidence-digest repo base head))
  (or
   (for/or ([path (in-list record-paths)])
     (define-values (status recorded) (read-record-digest path))
     (define label (record-label path))
     (match status
       ['ok
        (and
         (not (equal? recorded computed))
         (format
          "digest-mismatch: ~a records ~a but the computed excluded-evidence digest at head ~a is ~a"
          label
          recorded
          (substring head 0 12)
          computed))]
       ['missing (format "malformed-digest: ~a carries no content-digest field" label)]
       ['malformed (format "malformed-digest: ~a content-digest is not a lowercase SHA-256" label)]
       [_ (format "unreadable-record: ~a" path)]))
   (format "digest-ok ~a" computed)))

;; The F2 author: recompute and WRITE the computed digest into every named
;; record. The only sanctioned way a content-digest comes into existence.
(define (bind-records! repo base head record-paths)
  (define computed (excluded-evidence-digest repo base head))
  (for ([path (in-list record-paths)])
    (define datum (read-single-datum path))
    (unless (hash? datum)
      (error 'gsd-evidence-bind "record is not a hash: ~a" path))
    (write-record! path (hash-set datum 'content-digest computed))
    (displayln (format "bound ~a ~a" (record-label path) computed)))
  computed)

(define (datum->string value)
  (cond
    [(hash? value)
     (format "#hasheq(~a)"
             (string-join (for/list ([key (in-list (sort (hash-keys value) symbol<?))])
                            (format "(~a . ~a)" key (datum->string (hash-ref value key))))
                          " "))]
    [(pair? value) (format "(~a)" (string-join (map datum->string value) " "))]
    [(null? value) "()"]
    [(string? value) (format "~s" value)]
    [(symbol? value) (format "~a" value)]
    [(boolean? value) (if value "#t" "#f")]
    [(exact-integer? value) (number->string value)]
    [(real? value) (format "~a" value)]
    [else (error 'gsd-evidence-bind "cannot serialize record value: ~a" value)]))

(define (write-record! path datum)
  (define text (string-append (datum->string datum) "\n"))
  (define temp (make-temporary-file "q-evidence-bind-~a.rktd" #f (path-only (simple-form-path path))))
  (with-output-to-file temp (lambda () (display text)) #:exists 'replace)
  (rename-file-or-directory temp path #t))

;; The F4 purity check: every base..head commit that touches the evidence
;; directory must have touched ONLY record paths. Returns the decided verdict
;; line (`pure`, or `impure-record-commit: <paths> (commit <sha>)` naming the
;; offending paths).
(define (record-commit-purity repo base head)
  (require-full-sha 'gsd-evidence-bind "base" base)
  (require-full-sha 'gsd-evidence-bind "head" head)
  (define-values (code revlist stderr)
    (run/git/bytes repo "rev-list" "--reverse" (string-append base ".." head)))
  (unless (zero? code)
    (error 'gsd-evidence-bind "git rev-list failed: ~a" (string-trim (bytes->string/utf-8 stderr))))
  (define commits
    (for/list ([line (in-lines (open-input-bytes revlist))]
               #:unless (string=? (string-trim line) ""))
      (string-trim line)))
  (or (for/or ([commit (in-list commits)])
        (define-values (dcode dstdout dstderr)
          (run/git/bytes repo "diff-tree" "--no-commit-id" "--name-only" "--no-renames" "-r" commit))
        (unless (zero? dcode)
          (error 'gsd-evidence-bind
                 "git diff-tree failed: ~a"
                 (string-trim (bytes->string/utf-8 dstderr))))
        (define paths
          (for/list ([line (in-lines (open-input-bytes dstdout))]
                     #:unless (string=? (string-trim line) ""))
            (string-trim line)))
        ;; Merge commits list no paths with plain diff-tree; they are skipped.
        (and (pair? paths)
             (ormap (lambda (p) (string-prefix? p evidence-dir-prefix)) paths)
             (let ([foreign (for/list ([p (in-list paths)]
                                       #:unless (ormap (lambda (prefix) (string-prefix? p prefix))
                                                       exclusion-prefixes))
                              p)])
               (and (pair? foreign)
                    (format "impure-record-commit: ~a (commit ~a)"
                            (string-join foreign ", ")
                            (substring commit 0 12))))))
      "pure"))

(define (print-usage)
  (displayln
   (string-append
    "Usage:\n"
    "  racket scripts/gsd-evidence-bind.rkt digest --repo <dir> --base <sha> --head <sha>\n"
    "  racket scripts/gsd-evidence-bind.rkt bind   --repo <dir> --base <sha> --head <sha>\n"
    "       [--evidence <path>] [--reviews <path>] [--validation <path>]\n"
    "  racket scripts/gsd-evidence-bind.rkt verify --repo <dir> --base <sha> --head <sha>\n"
    "       [--evidence <path>] [--reviews <path>] [--validation <path>]\n"
    "  racket scripts/gsd-evidence-bind.rkt check  --repo <dir> --base <sha> --head <sha>\n"
    "       --evidence <path>  ; alias of verify\n"
    "  racket scripts/gsd-evidence-bind.rkt record-commit --repo <dir> --base <sha> --head <sha>")))

(define (parse-options args required)
  (define table
    (let loop ([args args]
               [acc (hash)])
      (match args
        ['() acc]
        [(list "--repo" value rest ...) (loop rest (hash-set acc '--repo value))]
        [(list "--base" value rest ...) (loop rest (hash-set acc '--base value))]
        [(list "--head" value rest ...) (loop rest (hash-set acc '--head value))]
        [(list "--evidence" value rest ...) (loop rest (hash-set acc '--evidence value))]
        [(list "--reviews" value rest ...) (loop rest (hash-set acc '--reviews value))]
        [(list "--validation" value rest ...) (loop rest (hash-set acc '--validation value))]
        [(list "--check" rest ...)
         (loop rest (hash-set acc '--check #t))] ; accepted for tolerance, unused
        [(list flag rest ...)
         #:when (string-prefix? flag "--")
         (print-usage)
         (exit 2)]
        [_
         (print-usage)
         (exit 2)])))
  (for ([option (in-list required)])
    (unless (hash-ref table option #f)
      (print-usage)
      (exit 2)))
  table)

(define (record-paths options)
  (for/list ([opt (in-list (list (hash-ref options '--evidence #f)
                                 (hash-ref options '--reviews #f)
                                 (hash-ref options '--validation #f)))]
             #:when opt)
    opt))

(define (main args)
  (match args
    [(list "digest" rest ...)
     (define options (parse-options rest '(--repo --base --head)))
     (displayln (excluded-evidence-digest (hash-ref options '--repo)
                                          (hash-ref options '--base)
                                          (hash-ref options '--head)))
     0]
    [(list "verify" rest ...)
     (define options (parse-options rest '(--repo --base --head --evidence)))
     (displayln (verify-records (hash-ref options '--repo)
                                (hash-ref options '--base)
                                (hash-ref options '--head)
                                (record-paths options)))
     0]
    ;; The wave contract's `--check` mode: same fail-closed verification as
    ;; `verify` (typed digest-mismatch/malformed-digest refusals on stdout),
    ;; under the contract's literal name.
    [(list "check" rest ...) (main (list* "verify" rest))]
    [(list "bind" rest ...)
     (define options (parse-options rest '(--repo --base --head --evidence)))
     (bind-records! (hash-ref options '--repo)
                    (hash-ref options '--base)
                    (hash-ref options '--head)
                    (record-paths options))
     0]
    [(list "record-commit" rest ...)
     (define options (parse-options rest '(--repo --base --head)))
     (displayln (record-commit-purity (hash-ref options '--repo)
                                      (hash-ref options '--base)
                                      (hash-ref options '--head)))
     0]
    [_
     (print-usage)
     1]))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
