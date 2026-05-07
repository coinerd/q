#lang racket/base

;; tools/builtins/write.rkt — File write tool with security guards
;;
;; Layer: tools (interface layer consumer)
;; Purpose: Write content to files on disk with path canonicalization,
;; safe-mode checks, size limits (1MB per write, 50MB cumulative),
;; and destructive-command detection. Also manages the session write
;; budget parameter.
;;
;; Security: All writes are validated against safe-mode predicates,
;; allowed-path checks, and the destructive-command blocklist before
;; any filesystem modification.

(require racket/file
         racket/string
         (only-in "../tool.rkt"
                  make-success-result
                  make-error-result
                  exec-context?
                  exec-context-bytes-written)
         (only-in "../../util/path-helpers.rkt" path-only expand-home-path)
         (only-in "../../util/errors.rkt" raise-tool-error tool-error?)
         (only-in "../../util/error-sanitizer.rkt" sanitize-error-message)
         ;; v0.21.10: planning path resolution hardening (F7)
         (only-in "../../extensions/gsd-planning-state.rkt" pinned-planning-dir)
         (only-in "builtin-helpers.rkt" require-safe-path!))

(provide tool-write
         current-max-write-bytes
         cumulative-write-budget
         reset-cumulative-writes!
         init-session-writes!)

;; Maximum write size in bytes (default 1MB) — SEC-03
(define current-max-write-bytes (make-parameter 1048576))

;; Cumulative write budget per session (default 50MB) — SEC-14
(define cumulative-write-budget (make-parameter 52428800))

;; Track cumulative bytes written in current session.
;; v0.29.9: Migrated from make-parameter+box to exec-context field.
;; v0.29.11: Parameter retained for backward compat (tests, direct calls without exec-ctx).
;; DEPRECATED (v0.29.12): session-bytes-written will be removed in v0.30.x.
;; Retained for backward-compat tests. Has 0 production callers outside test mocks.
(define session-bytes-written (make-parameter (box 0)))

(define (init-session-writes!)
  (session-bytes-written (box 0)))

(define (reset-cumulative-writes!)
  (set-box! (session-bytes-written) 0))

;; Get the bytes-written box from exec-ctx if available, else fall back to parameter.
(define (get-bytes-box exec-ctx)
  (if (and exec-ctx (exec-context-bytes-written exec-ctx))
      (exec-context-bytes-written exec-ctx)
      (session-bytes-written)))

;; Main tool function
(define (tool-write args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define expanded (and raw-path (expand-home-path raw-path)))
  ;; v0.21.10 (F7): Rewrite .planning/ paths to pinned project root FIRST
  ;; (before canonicalization, so prefix detection works on original path)
  (define resolved (and expanded (resolve-planning-path expanded)))
  ;; SEC-02 (v0.22.0): Canonicalize path to prevent traversal attacks
  (define path-str
    (and resolved
         (let ([p (if (string? resolved)
                      resolved
                      (path->string resolved))])
           (with-handlers ([exn:fail? (lambda (_) p)])
             (path->string (simplify-path (resolve-path p)))))))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [(require-safe-path! path-str "write")
     =>
     (lambda (err) (make-error-result err))]
    [else
     (define content-str (hash-ref args 'content ""))
     (with-handlers ([exn:fail:filesystem? (lambda (e)
                                             (make-error-result (sanitize-error-message
                                                                 (format "Write error: ~a"
                                                                         (exn-message e)))))]
                     [tool-error? (lambda (e) (make-error-result (exn-message e)))])
       ;; SEC-03: Enforce per-write max-write-bytes limit
       (define content-bytes (string->bytes/utf-8 content-str))
       (define bytes-count (bytes-length content-bytes))
       (when (> bytes-count (current-max-write-bytes))
         (raise-tool-error (format "write rejected: ~a bytes exceeds limit of ~a"
                                   bytes-count
                                   (current-max-write-bytes))
                           'write))

       ;; SEC-14: Enforce cumulative write budget
       (define bytes-box (get-bytes-box exec-ctx))
       (define new-total (+ (unbox bytes-box) bytes-count))
       (when (> new-total (cumulative-write-budget))
         (raise-tool-error (format "write rejected: cumulative ~a bytes exceeds session budget of ~a"
                                   new-total
                                   (cumulative-write-budget))
                           'write))

       ;; Create parent directories if needed
       (define dir (path-only path-str))
       (when (and dir (not (directory-exists? dir)))
         (make-directory* dir))

       ;; Write content
       (call-with-output-file path-str (lambda (out) (display content-str out)) #:exists 'replace)

       ;; Track cumulative bytes
       (set-box! bytes-box new-total)

       (make-success-result
        (list (format "Wrote ~a bytes to ~a" bytes-count path-str))
        (hasheq 'path path-str 'bytes-written bytes-count 'session-total new-total)))]))

;; v0.21.10 (F7): If path contains .planning/ as a leading component
;; and pinned-planning-dir is set, rewrite to use the pinned dir.
;; This prevents writes to ~/.planning/ when TUI launched from ~/.
(define (resolve-planning-path path-str)
  (cond
    [(not path-str) #f]
    [(path? path-str) (resolve-planning-path (path->string path-str))]
    [else
     (define pinned (pinned-planning-dir))
     (cond
       [(not pinned) path-str]
       ;; Only rewrite simple .planning/ prefix paths (not ../other/.planning/)
       [(and (or (string-prefix? path-str ".planning/") (string-prefix? path-str "./.planning/"))
             (not (string-contains? (substring path-str 0 (min (string-length path-str) 20)) "..")))
        (define suffix
          (if (string-prefix? path-str "./.planning/")
              (substring path-str (string-length "./.planning/"))
              (substring path-str (string-length ".planning/"))))
        (build-path pinned ".planning" suffix)]
       [else path-str])]))

;; path-only imported from util/path-helpers.rkt
