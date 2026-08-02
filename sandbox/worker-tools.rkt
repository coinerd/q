#lang racket/base

;; sandbox/worker-tools.rkt — Tool execution dispatch for the worker process
;;
;; The worker has a minimal tool registry. It does NOT import the full
;; 27-tool registry (too heavy, defeats isolation). Instead, each tool
;; is a function: (-> hash? ipc-response?).
;;
;; Security constraints:
;;   - Working directory locked to configured workspace
;;   - Path traversal: all file paths resolved and checked
;;   - Output size capped at IPC-MAX-RESPONSE-BYTES
;;   - Shell execution reuses sandbox/subprocess.rkt (custodian-based)

(require racket/contract
         racket/file
         racket/match
         racket/port
         racket/string
         (only-in racket/list take drop last)
         json
         "ipc-protocol.rkt"
         "subprocess.rkt"
         "limits.rkt"
         ;; SEC-7 (v0.99.76 W2): shared config-dir resolution for edit backups.
         (only-in "../util/config-paths.rkt" global-config-dir)
         (only-in "../util/racket-source-validation.rkt"
                  validate-proposed-racket-source
                  racket-edit-balance-warning)
         (only-in "../tools/builtins/edit-contract.rkt"
                  DEFAULT-MAX-OLD-TEXT-LEN
                  SAFE-MAX-OLD-TEXT-LEN
                  apply-edit-contract
                  edit-contract-result-status
                  edit-contract-result-content
                  edit-contract-result-occurrences
                  edit-contract-result-replacements)
         ;; SEC-1 (v0.99.76 W1): shared shell-safety predicates — same source of
         ;; truth as the main tool-bash path (STATE D1: import, don't duplicate).
         (only-in "../tools/builtins/bash-safety.rkt"
                  destructive-command?
                  high-risk-command?
                  structured-destructive-command?
                  structured-critical-command?))

;; ── Path Safety ─────────────────────────────────────────────────

;; Allowed root directories for file operations
(define current-allowed-roots (make-parameter (list (current-directory))))

;; LF3 (v0.99.4): Walk path components to resolve symlinks on the longest
;; existing prefix. Prevents symlink-based escapes when non-existent
;; directories exist after a symlink in the path chain. For example,
;; /allowed-root/symlink→/etc/a/b/file.txt would previously pass the check
;; because the else branch only called simplify-path (no symlink resolution).
(define (resolve-longest-prefix p)
  (define parts (explode-path p))
  (let loop ([remaining parts]
             [resolved-prefix #f])
    (cond
      [(null? remaining) (or resolved-prefix (simplify-path p #f))]
      [else
       (define candidate
         (if resolved-prefix
             (build-path resolved-prefix (car remaining))
             (car remaining)))
       (cond
         [(or (file-exists? candidate) (directory-exists? candidate))
          ;; Component exists — resolve symlinks and continue
          (loop (cdr remaining) (simplify-path (resolve-path candidate) #f))]
         [(link-exists? candidate)
          ;; Broken symlink — reject (resolve-path would raise, caught by outer handler → #f)
          (raise (exn:fail (format "broken symlink: ~a" candidate) (current-continuation-marks)))]
         [else
          ;; Non-existent component — append remaining to resolved prefix
          (if resolved-prefix
              (for/fold ([acc resolved-prefix]) ([part (in-list remaining)])
                (build-path acc part))
              (simplify-path p #f))])])))

;; H3: Resolve symlinks before checking — simplify-path alone does NOT resolve symlinks.
;; A symlink inside the allowed root pointing to /etc would pass the old check.
;; resolve-path follows symlinks and raises exn on broken links → we reject those.
;; For non-existent files (new writes), resolve the parent directory instead.
;; LF3 (v0.99.4): When neither file nor parent dir exists, walk path components
;; to resolve symlinks on the longest existing prefix.
(define (resolve-path-safely p)
  (define complete (path->complete-path (expand-user-path p) (current-directory)))
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (cond
      [(file-exists? complete) (simplify-path (resolve-path complete) #f)]
      ;; File doesn't exist yet (e.g. new write) — resolve parent dir, then append filename
      [(let-values ([(base name must-be-dir?) (split-path complete)])
         (and base (directory-exists? base)))
       (let-values ([(base name must-be-dir?) (split-path complete)])
         (define parent-resolved (simplify-path (resolve-path base) #f))
         (if name
             (build-path parent-resolved name)
             parent-resolved))]
      ;; LF3 (v0.99.4): Neither file nor parent dir exists — walk path
      ;; components to resolve symlinks on the longest existing prefix.
      [else (resolve-longest-prefix complete)])))

;; H3: Normalize a root directory at comparison time (resolve symlinks)
(define (normalize-root r)
  (with-handlers ([exn:fail? (lambda (_) r)])
    (simplify-path (resolve-path (path->complete-path r (current-directory))) #f)))

;; Check if a path is within allowed roots
(define (path-allowed? p)
  (define resolved (resolve-path-safely p))
  ;; If resolve-path failed (broken symlink or missing file), reject
  (and resolved
       (let ([resolved-str (path->string resolved)])
         (for/or ([root (in-list (current-allowed-roots))])
           (define normalized-root (normalize-root root))
           (define root-str (path->string (path->complete-path normalized-root (current-directory))))
           ;; Ensure root ends with a single slash for prefix comparison
           (define root-dir
             (if (string-suffix? root-str "/")
                 root-str
                 (string-append root-str "/")))
           (or (string=? resolved-str root-str)
               (string=? resolved-str root-dir)
               (string-prefix? resolved-str root-dir))))))

;; ── SEC-7 (v0.99.76 W2): Worker file-op safety parity ───────────
;; Mirrors main tool-write/tool-edit guards: per-write size limit,
;; cumulative write budget, inode (identity) TOCTOU check, backups.

(define current-worker-write-limit
  ;; Per-write byte limit (default 1 MB, matches main tool-write)
  (make-parameter (* 1024 1024)))

(define current-worker-cumulative-limit
  ;; Session cumulative write budget (default 50 MB, matches main tool-write)
  (make-parameter (* 50 1024 1024)))

;; Module-level cumulative bytes written this worker session.
(define worker-cumulative-bytes (box 0))

;; Hook invoked immediately before the atomic write in execute-edit /
;; execute-delete-lines. Lets tests inject a concurrent modification
;; (TOCTOU simulation) between read and write.
(define current-worker-edit-before-write-hook (make-parameter (lambda (path new-content) (void))))

(define (worker-write-limit-check content-str)
  ;; Returns #f when the write is allowed, or an error message string.
  (define bytes-written (string-length content-str))
  (define per-write (current-worker-write-limit))
  (define cumulative (current-worker-cumulative-limit))
  (cond
    [(> bytes-written per-write)
     (format "write: content exceeds per-write limit (~a bytes > ~a bytes)" bytes-written per-write)]
    [(> (+ bytes-written (unbox worker-cumulative-bytes)) cumulative)
     (format "write: exceeds cumulative write budget (~a + ~a > ~a bytes)"
             bytes-written
             (unbox worker-cumulative-bytes)
             cumulative)]
    [else
     (set-box! worker-cumulative-bytes (+ bytes-written (unbox worker-cumulative-bytes)))
     #f]))

;; ── Backup helpers (shared pattern with edit.rkt / delete-lines.rkt)
(define MAX-BACKUPS-PER-FILE 10)

(define (ensure-backup-dir)
  (define dir (build-path (global-config-dir) "edit-backups"))
  (unless (directory-exists? dir)
    (make-directory* dir)
    (file-or-directory-permissions dir #o700))
  dir)

(define (save-backup path-str content)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "worker/backup: ~a" (exn-message e)))
                               #f)])
    (define dir (ensure-backup-dir))
    (define basename (file-name-from-path path-str))
    (define source-key (number->string (equal-hash-code path-str) 16))
    (define timestamp (number->string (abs (current-milliseconds))))
    ;; Exclusive creation prevents concurrent edits from overwriting backups.
    (define backup-path
      (make-temporary-file (format "~a_~a_~a_~a" timestamp source-key "~a" basename) #:base-dir dir))
    (display-to-file content backup-path #:exists 'truncate)
    (prune-old-backups dir source-key basename)
    (path->string backup-path)))

(define (file-name-from-path p)
  (define fname
    (if (string? p)
        p
        (path->string p)))
  (define parts (regexp-split #rx"/" fname))
  (if (null? parts)
      "unknown"
      (last parts)))

(define (prune-old-backups dir source-key basename)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "worker/prune: ~a" (exn-message e)))
                               (void))])
    (define marker (format "_~a_" source-key))
    (define all (directory-list dir))
    (define matching
      (filter (lambda (f) (string-contains? (path->string f) marker))
              (sort (map path->string all) string>?)))
    (when (> (length matching) MAX-BACKUPS-PER-FILE)
      (for ([f (in-list (drop matching MAX-BACKUPS-PER-FILE))])
        (delete-file (build-path dir f))))))

;; SEC-7 (D5): best-effort inode/identity snapshot — network filesystems may
;; not support identity; never fail the operation on snapshot errors.
(define (worker-file-identity p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (file-or-directory-identity p)))

(define (worker-identity-unchanged? before after)
  (or (not before) (not after) (equal? before after)))

(define (worker-check-then-write resolved new-content [identity-before #f])
  ;; Returns #f on success, or an error message string (no write performed).
  (define hook (current-worker-edit-before-write-hook))
  (when hook
    (hook resolved new-content))
  ;; SEC-7 (D5): TOCTOU re-check AFTER the hook runs — the hook (or any
  ;; concurrent writer) may have replaced the file since we read it.
  (define identity-after (worker-file-identity resolved))
  (if (worker-identity-unchanged? identity-before identity-after)
      (begin
        (call-with-atomic-output-file resolved (lambda (port) (display new-content port)))
        #f)
      "edit: file was modified concurrently"))

;; ── Tool Execution Functions ────────────────────────────────────

;; Each function accepts a hash and returns an ipc-response. execute-edit also
;; accepts optional internal policy keywords while preserving its one-argument API.
;; Returns ipc-response with content and details populated.

(define (execute-bash args)
  (define command (hash-ref args 'command #f))
  (define timeout-secs (hash-ref args 'timeout 30))
  (define cwd (hash-ref args 'cwd #f))
  (cond
    [(not (and command (string? command)))
     (make-error-response #f "bash: missing 'command' argument")]
    [(and cwd (not (path-allowed? cwd)))
     (make-error-response #f (format "bash: cwd not allowed: ~a" cwd))]
    ;; SEC-1 (v0.99.76 W1): worker safety chain — regex blocklist first, then
    ;; fail-closed structured classifier for obfuscated commands. Worker policy
    ;; is BLOCK (stricter than main's warn): the worker has no approval channel.
    [(destructive-command? command)
     (make-error-response #f (format "bash: blocked destructive command: ~a" command))]
    [(structured-critical-command? command)
     (make-error-response #f (format "bash: blocked by structured risk classifier: ~a" command))]
    [else
     (define safety-warning
       (cond
         [(or (high-risk-command? command) (structured-destructive-command? command))
          (format "WARNING: High-risk command detected: ~a" command)]
         [else #f]))
     (define result
       (run-subprocess "/bin/bash"
                       #:args (list "-c" command)
                       ;; W1 v0.99.77: process-group launch so a timed-out
                       ;; worker bash call can SIGKILL the whole group.
                       #:process-group? #t
                       #:timeout timeout-secs
                       #:directory (or cwd (current-directory))))
     (define exit-code (subprocess-result-exit-code result))
     (define timed-out? (subprocess-result-timed-out? result))
     (cond
       [timed-out?
        (ipc-response #f
                      'timeout
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result)
                              'warning
                              safety-warning)
                      "command timed out"
                      IPC-SCHEMA-VERSION)]
       [(eqv? exit-code 0)
        (ipc-response #f
                      'ok
                      (string-trim (subprocess-result-stdout result))
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result)
                              'warning
                              safety-warning)
                      #f
                      IPC-SCHEMA-VERSION)]
       [else
        (ipc-response #f
                      'error
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result)
                              'warning
                              safety-warning)
                      (format "command exited with code ~a" exit-code)
                      IPC-SCHEMA-VERSION)])]))

(define (execute-write args)
  (define path (hash-ref args 'path #f))
  (define content (hash-ref args 'content ""))
  (cond
    [(not path) (make-error-response #f "write: missing 'path' argument")]
    [(not (path-allowed? path)) (make-error-response #f (format "write: path not allowed: ~a" path))]
    [else
     (define content-str
       (cond
         [(string? content) content]
         [(bytes? content) (bytes->string/utf-8 content)]
         [else (format "~a" content)]))
     (define resolved (path->complete-path (expand-user-path path) (current-directory)))
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-response #f (format "write: ~a" (exn-message e))))])
       (define bytes-written (string-length content-str))
       ;; SEC-7 (v0.99.76 W2): per-write + cumulative size limits (main parity)
       (define limit-error (worker-write-limit-check content-str))
       (if limit-error
           (make-error-response #f limit-error)
           (begin
             (call-with-atomic-output-file resolved (lambda (port) (display content-str port)))
             (ipc-response #f
                           'ok
                           (format "wrote ~a bytes to ~a" bytes-written (path->string resolved))
                           (hasheq 'path (path->string resolved) 'bytes-written bytes-written)
                           #f
                           IPC-SCHEMA-VERSION))))]))

(define (call-with-atomic-output-file path proc)
  ;; Write to temp then rename for atomicity
  (define tmp (format "~a.tmp.~a" path (current-milliseconds)))
  (call-with-output-file tmp #:exists 'replace proc)
  (rename-file-or-directory tmp path #t))

(define (execute-edit args
                      #:max-old-text-len [max-old-text-len DEFAULT-MAX-OLD-TEXT-LEN]
                      #:fuzzy-edit-enabled? [global-fuzzy-enabled? #f])
  (define path (hash-ref args 'path #f))
  (define old-text (hash-ref args 'old-text #f))
  (define new-text (hash-ref args 'new-text ""))
  (define max-old-text-len-provided? (hash-has-key? args 'max-old-text-len))
  (define provided-max-old-text-len (hash-ref args 'max-old-text-len #f))
  (define effective-max-old-text-len
    (if max-old-text-len-provided? provided-max-old-text-len max-old-text-len))
  (cond
    [(not path) (make-error-response #f "edit: missing 'path' argument")]
    [(not old-text) (make-error-response #f "edit: missing 'old-text' argument")]
    [(or (not (exact-positive-integer? effective-max-old-text-len))
         (> effective-max-old-text-len SAFE-MAX-OLD-TEXT-LEN))
     (make-error-response
      #f
      (format "edit: max-old-text-len must be an exact positive integer at most ~a"
              SAFE-MAX-OLD-TEXT-LEN))]
    [(and (hash-has-key? args 'fuzzy?) (not (boolean? (hash-ref args 'fuzzy?))))
     (make-error-response #f "edit: fuzzy? must be a boolean (#t or #f)")]
    [(not (boolean? global-fuzzy-enabled?))
     (make-error-response #f "edit: fuzzy edit policy must be boolean")]
    [(not (path-allowed? path)) (make-error-response #f (format "edit: path not allowed: ~a" path))]
    [else
     (define resolved (path->complete-path (expand-user-path path) (current-directory)))
     (cond
       [(not (file-exists? resolved))
        (make-error-response #f (format "edit: file not found: ~a" path))]
       [else
        (define content (file->string resolved))
        ;; SEC-7 (v0.99.76 W2): record inode before read for TOCTOU check
        (define identity-before (worker-file-identity resolved))
        (define fuzzy-allowed? (or (hash-ref args 'fuzzy? #f) global-fuzzy-enabled?))
        (define edit-result
          (apply-edit-contract content
                               old-text
                               new-text
                               #:fuzzy? fuzzy-allowed?
                               #:max-old-text-len effective-max-old-text-len))
        (case (edit-contract-result-status edit-result)
          [(empty-old-text) (make-error-response #f "edit: old-text must not be empty")]
          [(too-long)
           (make-error-response
            #f
            (format (string-append "edit: old-text is too long (~a chars, max ~a). "
                                   "For a whole-form replacement, pass max-old-text-len explicitly "
                                   "(up to ~a), or use the structural edit tool; "
                                   "do not split a nested form into partial edits.")
                    (string-length old-text)
                    effective-max-old-text-len
                    SAFE-MAX-OLD-TEXT-LEN))]
          [(not-found)
           (define detail (build-not-found-detail content old-text))
           (make-error-response #f (format "edit: old-text not found in file\n~a" detail))]
          [(duplicate)
           (make-error-response
            #f
            (format "edit: old-text appears ~a times; provide one unique exact snippet"
                    (edit-contract-result-occurrences edit-result)))]
          [(ambiguous)
           (make-error-response
            #f
            (format "edit: fuzzy matching found ~a possible matches; provide exact text"
                    (edit-contract-result-occurrences edit-result)))]
          [(line-count-mismatch) (make-error-response #f "edit: line count changed unexpectedly")]
          [(ok)
           (define new-content (edit-contract-result-content edit-result))
           ;; SEC-7 (v0.99.76 W2): backup original + TOCTOU identity re-check
           (define identity-after (worker-file-identity resolved))
           ;; Parse and size validation both fail closed before backup/write.
           ;; The shared parser and balance guidance keep worker and in-process
           ;; edit semantics equal.
           (define balance-warning (racket-edit-balance-warning resolved old-text new-text))
           (define validation-error (validate-proposed-racket-source resolved new-content))
           (define validation-message
             (and validation-error
                  (if balance-warning
                      (string-append validation-error "\n" balance-warning)
                      validation-error)))
           (define limit-error (worker-write-limit-check new-content))
           (if (or validation-message limit-error)
               (make-error-response #f (or validation-message limit-error))
               (if (worker-identity-unchanged? identity-before identity-after)
                   (let ([write-error
                          (begin
                            (save-backup resolved content)
                            (worker-check-then-write resolved new-content identity-before))])
                     (if write-error
                         (make-error-response #f write-error)
                         (ipc-response #f
                                       'ok
                                       (if balance-warning
                                           (string-append "edit applied\n" balance-warning)
                                           "edit applied")
                                       (hasheq 'path
                                               (path->string resolved)
                                               'replacements
                                               (edit-contract-result-replacements edit-result))
                                       #f
                                       IPC-SCHEMA-VERSION)))
                   (make-error-response #f "edit: file was modified concurrently")))])])]))

(define (execute-git args)
  (define command (hash-ref args 'command #f))
  (define git-args (hash-ref args 'args '()))
  (define cwd (hash-ref args 'cwd #f))
  (cond
    [(not command) (make-error-response #f "git: missing 'command' argument")]
    ;; SEC-4 (v0.99.76 W0): cwd confinement — fail closed before safety eval.
    [(and cwd (not (path-allowed? cwd)))
     (make-error-response #f (format "git: cwd not allowed: ~a" cwd))]
    ;; SEC-1 (v0.99.76 W1): block destructive git compositions — force push to
    ;; shared branches, clean -fdx, reset --hard (data loss). Fail closed.
    [(and (string=? command "push")
          (member "--force" git-args)
          (ormap (lambda (a) (and (string? a) (member a '("origin" "upstream")))) git-args))
     (make-error-response #f "git: blocked: force push to shared branch")]
    [(and (string=? command "clean")
          (ormap (lambda (a) (and (string? a) (regexp-match? #rx"^-f" a))) git-args))
     (make-error-response #f "git: blocked: destructive clean")]
    [(and (string=? command "reset") (member "--hard" git-args))
     (make-error-response #f "git: blocked: reset --hard")]
    [else
     (define args-list
       (cond
         [(list? git-args) git-args]
         [(string? git-args) (list git-args)]
         [else '()]))
     (define result
       (run-subprocess "git"
                       #:args (cons command args-list)
                       #:timeout 30
                       #:directory (or cwd (current-directory))))
     (define exit-code (subprocess-result-exit-code result))
     (define timed-out? (subprocess-result-timed-out? result))
     (cond
       [timed-out?
        (ipc-response #f
                      'timeout
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      "git command timed out"
                      IPC-SCHEMA-VERSION)]
       [(eqv? exit-code 0)
        (ipc-response #f
                      'ok
                      (string-trim (subprocess-result-stdout result))
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      #f
                      IPC-SCHEMA-VERSION)]
       [else
        (ipc-response #f
                      'error
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      (format "git ~a exited with code ~a" command exit-code)
                      IPC-SCHEMA-VERSION)])]))

;; v0.99.20 W2 (§3.2): delete-lines — pure file-edit operation.
;; Reads file, removes lines [start-line, end-line] (inclusive, 1-based),
;; writes result back atomically.
(define (execute-delete-lines args)
  (define path (hash-ref args 'path #f))
  (define start-line (hash-ref args 'start-line #f))
  (define end-line (hash-ref args 'end-line #f))
  (cond
    [(not path) (make-error-response #f "delete-lines: missing 'path' argument")]
    [(not start-line) (make-error-response #f "delete-lines: missing 'start-line' argument")]
    [(not end-line) (make-error-response #f "delete-lines: missing 'end-line' argument")]
    [(not (exact-integer? start-line))
     (make-error-response #f (format "delete-lines: start-line must be integer, got: ~v" start-line))]
    [(not (exact-integer? end-line))
     (make-error-response #f (format "delete-lines: end-line must be integer, got: ~v" end-line))]
    [(not (path-allowed? path))
     (make-error-response #f (format "delete-lines: path not allowed: ~a" path))]
    [else
     (define resolved (path->complete-path (expand-user-path path) (current-directory)))
     (cond
       [(not (file-exists? resolved))
        (make-error-response #f (format "delete-lines: file not found: ~a" path))]
       [else
        (define content (file->string resolved))
        ;; SEC-7 (v0.99.76 W2): record inode before read for TOCTOU check
        (define identity-before (worker-file-identity resolved))
        (define lines (string-split content "\n" #:trim? #f))
        (define total-lines (length lines))
        (cond
          [(< start-line 1)
           (make-error-response
            #f
            (format "delete-lines: start-line ~a is out of range (file has ~a lines)"
                    start-line
                    total-lines))]
          [(> end-line total-lines)
           (make-error-response
            #f
            (format "delete-lines: end-line ~a exceeds file length (file has ~a lines)"
                    end-line
                    total-lines))]
          [(> start-line end-line)
           (make-error-response
            #f
            (format "delete-lines: start-line (~a) must be ≤ end-line (~a)" start-line end-line))]
          [else
           (define before (take lines (sub1 start-line)))
           (define after (drop lines end-line))
           (define new-lines (append before after))
           (define new-content (string-join new-lines "\n"))
           (define deleted-count (- end-line start-line -1))
           ;; SEC-7 (v0.99.76 W2): size limit + backup + TOCTOU re-check (parity)
           (define limit-error (worker-write-limit-check new-content))
           (if limit-error
               (make-error-response #f limit-error)
               (let ([write-error (begin
                                    (save-backup resolved content)
                                    (worker-check-then-write resolved new-content identity-before))])
                 (if write-error
                     (make-error-response #f write-error)
                     (ipc-response #f
                                   'ok
                                   (format "Deleted lines ~a-~a from ~a (~a lines removed)"
                                           start-line
                                           end-line
                                           (path->string resolved)
                                           deleted-count)
                                   (hasheq 'path
                                           (path->string resolved)
                                           'lines-deleted
                                           deleted-count
                                           'remaining-lines
                                           (length new-lines))
                                   #f
                                   IPC-SCHEMA-VERSION))))])])]))

;; ── Tool Registry ───────────────────────────────────────────────

(define worker-tool-registry
  (hash "bash"
        execute-bash
        "write"
        execute-write
        "edit"
        execute-edit
        "git"
        execute-git
        "delete-lines"
        execute-delete-lines))

;; --------------------------------------------------
;; Enhanced diagnostics: first-difference offset, escaped code points, whitespace count
;; (W3 parity with tools/builtins/edit.rkt)
;; --------------------------------------------------

(define (first-differing-offset a b)
  (define len-a (string-length a))
  (define len-b (string-length b))
  (define min-len (min len-a len-b))
  (let loop ([i 0])
    (cond
      [(= i min-len)
       (if (= len-a len-b)
           (values #f #f #f)
           (values i (and (< i len-a) (string-ref a i)) (and (< i len-b) (string-ref b i))))]
      [(not (char=? (string-ref a i) (string-ref b i))) (values i (string-ref a i) (string-ref b i))]
      [else (loop (+ i 1))])))

(define (escape-char c)
  (format "U+~X" (char->integer c)))

(define (escaped-context s offset [context-radius 6])
  (define len (string-length s))
  (define start (max 0 (- offset context-radius)))
  (define end (min len (+ offset context-radius 1)))
  (define parts
    (for/list ([i (in-range start end)])
      (escape-char (string-ref s i))))
  (format "[~a]" (string-join parts " ")))

(define (count-leading-spaces s)
  (for/fold ([count 0])
            ([ch (in-string s)]
             #:break (not (char=? ch #\space)))
    (add1 count)))

(define (build-not-found-detail content old-text)
  (define-values (diff-offset content-char old-char) (first-differing-offset content old-text))
  (define diff-detail
    (cond
      [(and diff-offset content-char old-char)
       (format
        "First differing offset: ~a (~a vs ~a)\nContext around mismatch in file:  ~a\nContext around mismatch in old-text: ~a"
        diff-offset
        (escape-char content-char)
        (escape-char old-char)
        (escaped-context content diff-offset)
        (escaped-context old-text diff-offset))]
      [diff-offset
       (format "First differing offset: ~a (file has ~a chars, old-text has ~a chars)"
               diff-offset
               (string-length content)
               (string-length old-text))]
      [else ""]))
  (define ws-info
    (let* ([c-lines (string-split content "\n" #:trim? #f)]
           [o-lines (string-split old-text "\n" #:trim? #f)]
           [diff-lines (for/list ([c (in-list c-lines)]
                                  [o (in-list o-lines)]
                                  #:when (and c o (not (equal? c o))))
                         (define c-spaces (count-leading-spaces c))
                         (define o-spaces (count-leading-spaces o))
                         (format "  file has ~a leading spaces, old-text has ~a" c-spaces o-spaces))])
      (if (pair? diff-lines)
          (string-append "Whitespace differences:\n" (string-join diff-lines "\n"))
          "")))
  (string-append diff-detail (if (equal? diff-detail "") "" "\n") ws-info))

(define (dispatch-tool tool-name arguments)
  (define executor (hash-ref worker-tool-registry tool-name #f))
  (cond
    [executor
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-response
                                   #f
                                   (format "tool '~a' crashed: ~a" tool-name (exn-message e))))])
       (executor arguments))]
    [else (make-error-response #f (format "unknown tool: ~a" tool-name))]))

;; ── Provides ────────────────────────────────────────────────────

(provide worker-tool-registry
         dispatch-tool
         current-allowed-roots
         path-allowed?
         current-worker-write-limit
         current-worker-cumulative-limit
         current-worker-edit-before-write-hook
         execute-bash
         execute-write
         execute-edit
         execute-git
         execute-delete-lines)

;; dispatch-tool is provided above without contract
