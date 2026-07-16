#lang racket/base

;; Canonical containment boundary for every session-ID-derived path.
;; Legacy one-component IDs remain valid; path syntax and escaping links do not.

(require racket/contract
         racket/path
         racket/string)

(provide (contract-out
          [valid-session-id? (-> any/c boolean?)]
          [resolve-session-path
           (->* (path-string? string?)
                ((or/c #f "session.jsonl" "session.jsonl.pending" "session.index" "trace.jsonl"))
                path?)]
          [session-path-contained? (-> path-string? path-string? boolean?)]))

(define windows-drive-rx #px"^[A-Za-z]:")
(define windows-unc-rx #px"^(?:\\\\\\\\|//)")

(define (valid-session-id? value)
  (and (string? value)
       (positive? (string-length value))
       (not (member value '("." "..")))
       (not (string-contains? value "/"))
       (not (string-contains? value "\\"))
       (not (string-contains? value (string #\nul)))
       (not (regexp-match? windows-drive-rx value))
       (not (regexp-match? windows-unc-rx value))
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (not (absolute-path? (string->path value))))))
(define (resolve-link-components path)
  (for/fold ([resolved #f]) ([piece (in-list (explode-path path))])
    (define candidate
      (cond
        [(not resolved) piece]
        [(path? piece) (build-path resolved piece)]
        [else
         (raise-arguments-error 'resolve-link-components
                                "unexpected relative path component"
                                "component"
                                piece)]))
    (if (link-exists? candidate)
        (resolve-path candidate)
        candidate)))

(define (canonical-path who value)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-arguments-error who
                                                      "path cannot be resolved safely"
                                                      "path"
                                                      value
                                                      "reason"
                                                      (exn-message e)))])
    (resolve-link-components (simplify-path (path->complete-path value) #f))))

(define (session-path-contained? root candidate)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define canonical-root (canonical-path 'session-path-contained? root))
    (define canonical-candidate (canonical-path 'session-path-contained? candidate))
    (define relative (find-relative-path canonical-root canonical-candidate))
    (define pieces (explode-path relative))
    (and (relative-path? relative)
         (not (null? pieces))
         (not (member 'up pieces))
         (not (member 'same pieces)))))

(define (require-contained who root candidate label)
  (define canonical-root (canonical-path who root))
  (define canonical-candidate (canonical-path who candidate))
  (unless (session-path-contained? canonical-root canonical-candidate)
    (raise-arguments-error who
                           "resolved path escapes the session root"
                           label
                           candidate
                           "session-root"
                           canonical-root))
  canonical-candidate)

(define (resolve-session-path root session-id [artifact #f])
  (unless (valid-session-id? session-id)
    (raise-arguments-error 'resolve-session-path
                           "expected a nonempty one-component session identifier"
                           "session-id"
                           session-id))
  (define canonical-root (canonical-path 'resolve-session-path root))
  (define raw-session-path (build-path canonical-root session-id))
  ;; Session and artifact links are rejected, even when they currently point
  ;; inward, so a later recursive mutation cannot follow a swapped target.
  (when (link-exists? raw-session-path)
    (raise-arguments-error 'resolve-session-path
                           "session path must not be a symbolic link"
                           "session-path"
                           raw-session-path))
  (define session-path
    (require-contained 'resolve-session-path canonical-root raw-session-path "session-path"))
  (if artifact
      (let ([raw-artifact-path (build-path session-path artifact)])
        (when (link-exists? raw-artifact-path)
          (raise-arguments-error 'resolve-session-path
                                 "session artifact must not be a symbolic link"
                                 "artifact-path"
                                 raw-artifact-path))
        (require-contained 'resolve-session-path session-path raw-artifact-path "artifact-path"))
      session-path))
