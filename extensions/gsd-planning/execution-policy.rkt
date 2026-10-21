#lang racket/base

;; extensions/gsd-planning/execution-policy.rkt — phase transitions, tool blocking rules
;;
;; Mode-based tool access control and state machine guard logic.

(require racket/file
         racket/list
         racket/path
         racket/string
         "../gsd/state-machine.rkt"
         "../hooks.rkt"
         (only-in "../gsd/session-state.rkt" current-gsd-ctx))

(provide gsd-tool-guard
         current-gsd-author-phase
         author-scope-denial
         author-scope-tool-names)

;; Trusted caller supplies an immutable snapshot. Validate at admission rather
;; than raising in a parameter guard: malformed scopes must deny even reads.
(define current-gsd-author-phase (make-parameter #f))

(define (literal-repo-path? p)
  (and (string? p)
       (positive? (string-length p))
       (relative-path? p)
       (not (regexp-match? #px"\\\\" p))
       (for/and ([part (in-list (string-split p "/" #:trim? #f))])
         (not (member part '("" "." ".." ".git" ".planning"))))))

(define (valid-author-scope? s)
  (and (hash? s)
       (hash-eq? s)
       (immutable? s)
       (= (hash-count s) 4)
       (memq (hash-ref s 'phase #f) '(test-author implementation))
       (let ([repo (hash-ref s 'repo #f)])
         (and (path-string? repo) (complete-path? repo) (directory-exists? repo)))
       (for/and ([key '(files tests)])
         (define paths (hash-ref s key #f))
         (and (list? paths) (andmap literal-repo-path? paths)))
       (andmap (lambda (p) (member p (hash-ref s 'files))) (hash-ref s 'tests))))

;; No symlink component is accepted, including ancestors of the repository.
;; This is tool admission, not isolation from concurrent trusted project code.
(define (safe-regular-or-new? p)
  (and (not (link-exists? p))
       (let-values ([(base name dir?) (split-path p)])
         (and (or (not (path? base)) (safe-directory-chain? base))
              (or (file-exists? p) (not (or (directory-exists? p) (file-or-directory-type p #f))))))))

(define (safe-directory-chain? p)
  (define bare
    (if (equal? p (build-path "/"))
        p
        (string->path (regexp-replace #px"/+$" (path->string p) ""))))
  (and (not (link-exists? bare))
       (or (directory-exists? p) (not (file-or-directory-type p #f)))
       (let-values ([(base name dir?) (split-path p)])
         (or (not (path? base)) (equal? base p) (safe-directory-chain? base)))))

;; One decision shared by the hook and final effective-invocation policy.
;; Diagnostics expose only owner scope, never arbitrary arguments or outside paths.
(define author-tool-names '("read" "grep" "find" "ls" "glob" "write" "edit"))
(define (author-scope-tool-names scope)
  (if (valid-author-scope? scope)
      author-tool-names
      '()))
(define (public-text value)
  (define text
    (if (path? value)
        (path->string value)
        value))
  (if (and (string? text)
           (<= (string-length text) 512)
           (regexp-match? #px"^[A-Za-z0-9_./ :,-]+$" text))
      text
      "[non-public target]"))
(define (author-scope-denial scope tool args)
  (define invalid
    "AUTHOR_SCOPE_INVALID: stop and report an owner configuration problem; do not broaden scope.")
  (with-handlers ([exn:fail? (lambda (_) invalid)])
    (cond
      [(not (valid-author-scope? scope)) invalid]
      [(not (member tool author-tool-names))
       (format
        "AUTHOR_TOOL_NOT_ALLOWED: ~a denied in phase ~a; permitted tools: ~a. Owner-only tools stay unavailable."
        (public-text tool)
        (hash-ref scope 'phase)
        (string-join author-tool-names ", "))]
      [(member tool '("read" "grep" "find" "ls" "glob")) #f]
      [else
       (define raw (and (hash? args) (hash-ref args 'path #f)))
       (define repo (simplify-path (hash-ref scope 'repo) #f))
       (define allowed
         (if (eq? (hash-ref scope 'phase) 'test-author)
             (hash-ref scope 'tests)
             (remove* (hash-ref scope 'tests) (hash-ref scope 'files))))
       (cond
         [(or (not (path-string? raw))
              (member 'up (explode-path raw))
              (regexp-match? #px"\\\\"
                             (if (path? raw)
                                 (path->string raw)
                                 raw)))
          "AUTHOR_PATH_UNSAFE: unsafe target or traversal refused; use an exact authorized path without traversal."]
         [else
          (define target (simplify-path (path->complete-path raw repo) #f))
          (cond
            [(not (for/or ([p (in-list allowed)])
                    (equal? target (simplify-path (build-path repo p) #f))))
             (define relative (find-relative-path repo target))
             (format (string-append
                      "AUTHOR_PATH_OUT_OF_SCOPE: ~a denied. Allowed paths: ~a. Physical cwd: ~a. "
                      "Author those files; do not switch phase.")
                     (if (literal-repo-path? (path->string relative))
                         (public-text relative)
                         "outside-checkout target")
                     (string-join (map public-text allowed) ", ")
                     (public-text repo))]
            [(not (safe-regular-or-new? target))
             "AUTHOR_PATH_UNSAFE: symlink or unsafe target refused; stop and report to the owner."]
            [else #f])])])))

(define protected-planning-path-rx
  #px"(?:^|/)\\.planning/(?:PLAN|STATE|VALIDATION)(?:-[^/]*)?\\.md$|(?:^|/)\\.planning/HANDOFF\\.json$")

(define (generic-mutation-targets-planning-artifact? tool-name payload)
  (and (member tool-name '("write" "edit"))
       (let* ([args (hash-ref payload 'tool-arguments (hasheq))]
              [raw-path (and (hash? args) (hash-ref args 'path #f))]
              [path-string (cond
                             [(path? raw-path) (path->string raw-path)]
                             [(string? raw-path) raw-path]
                             [else #f])])
         (and path-string
              (regexp-match? protected-planning-path-rx (string-replace path-string "\\" "/"))))))

(define (gsd-tool-guard payload)
  (define mode (gsm-ctx-current (current-gsd-ctx)))
  (define tool-name (hash-ref payload 'tool-name #f))
  (define phase-denied?
    (and (current-gsd-author-phase)
         (author-scope-denial (current-gsd-author-phase)
                              tool-name
                              (hash-ref payload 'tool-arguments #f))))
  (define allowed (and (not phase-denied?) (gsm-tool-allowed? tool-name)))
  (cond
    [phase-denied? (hook-block phase-denied?)]
    [(and (eq? mode 'executing) (equal? tool-name "planning-write"))
     (hook-block "Cannot update plan during /go. Focus on executing the existing plan.")]
    [(and (eq? mode 'executing) (generic-mutation-targets-planning-artifact? tool-name payload))
     (hook-block
      "Cannot mutate authoritative .planning artifacts through generic write/edit during /go.")]
    [(and (not allowed) (eq? mode 'plan-written))
     (hook-block "Plan written to PLAN.md. Use /go to start implementing.")]
    [(not allowed) (hook-block (format "Tool '~a' blocked in ~a mode." tool-name mode))]
    [else (hook-pass payload)]))
