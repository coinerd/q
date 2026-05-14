#lang racket/base

;; extensions/gsd/tool-handlers.rkt — planning-read / planning-write tool handlers
;; STABILITY: stable
;;
;; Extracted from extensions/gsd-planning.rkt (v0.34.6 W0b — A-02 decomposition).

(require racket/match
         racket/port
         racket/string
         racket/file
         racket/path
         json
         "../define-extension.rkt"
         "../context.rkt"
         "../tool-api.rkt"
         "state-machine.rkt"
         "session-state.rkt"
         "../gsd-planning/command-normalization.rkt"
         (only-in "../gsd/core.rkt" gsd-write-guard)
         (only-in "../gsd/policy.rkt" policy-decision policy-blocked? policy-reason)
         (only-in "../gsd/events.rkt"
                  [emit-gsd-event! events:emit-gsd-event!]
                  [set-gsd-event-bus! events:set-gsd-event-bus!])
         (only-in "event-structs.rkt" make-gsd-mode-changed-event))

(provide planning-read-schema
         planning-write-schema
         handle-planning-read
         handle-planning-write
         resolve-project-root
         planning-artifact-path
         get-base-dir
         read-planning-artifact
         write-planning-artifact!)

;; ============================================================
;; Path helpers
;; ============================================================

(define (resolve-project-root start-dir)
  (let loop ([dir (if (string? start-dir)
                      (string->path start-dir)
                      start-dir)])
    (define planning (build-path dir planning-dir-name))
    (match #t
      [_
       #:when (directory-exists? planning)
       dir]
      [_
       #:when (or (directory-exists? (build-path dir "q"))
                  (directory-exists? (build-path dir "BLUEPRINT"))
                  (directory-exists? (build-path dir ".git")))
       dir]
      [_
       #:when (let ([parent (simple-form-path (build-path dir 'up))])
                (equal? parent (simple-form-path dir)))
       start-dir]
      [_ (loop (simple-form-path (build-path dir 'up)))])))

(define (planning-artifact-path base-dir name)
  (define ext (assoc name artifact-extensions))
  (match ext
    [(cons _ suffix) (build-path base-dir planning-dir-name (string-append name suffix))]
    [_
     #:when (or (string-suffix? name ".md") (string-suffix? name ".json"))
     (build-path base-dir planning-dir-name name)]
    [_ #f]))

;; ============================================================
;; File I/O
;; ============================================================

(define (get-base-dir args [exec-ctx #f])
  (or (hash-ref args 'base_dir #f)
      (current-pinned-dir)
      (and exec-ctx (ctx-cwd exec-ctx))
      (current-directory)))

(define (read-planning-artifact base-dir name)
  (define path (planning-artifact-path base-dir name))
  (match path
    [#f #f]
    [(? (lambda (p) (not (file-exists? p)))) #f]
    [_
     #:when (json-artifact? name)
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "gsd-planning/read: ~a" (exn-message e)))
                                  #f)])
       (call-with-input-file path (lambda (in) (read-json in))))]
    [_ (call-with-input-file path (lambda (in) (port->string in)))]))

(define (write-planning-artifact! base-dir name content)
  (define path (planning-artifact-path base-dir name))
  (if (not path)
      #f
      (let ([parent (path-only path)])
        (when parent
          (unless (directory-exists? parent)
            (make-directory* parent)))
        (if (json-artifact? name)
            (call-with-output-file path
                                   (lambda (out)
                                     (write-json content out)
                                     (newline out))
                                   #:exists 'truncate)
            (call-with-output-file path (lambda (out) (display content out)) #:exists 'truncate))
        path)))

;; ============================================================
;; Tool schemas
;; ============================================================

(define planning-read-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'artifact
    (hasheq 'type
            "string"
            'description
            (string-append "Artifact name. Canonical: "
                           (string-join (map car artifact-extensions) ", ")
                           ". Or any .md/.json filename."))
    'base_dir
    (hasheq 'type
            "string"
            'description
            "Project root directory. Auto-resolved at session start. Override only if needed."))
   'required
   '("artifact")))

(define planning-write-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'artifact
    (hasheq 'type
            "string"
            'description
            (string-append "Artifact name. Use: PLAN, STATE, HANDOFF, etc. "
                           "For wave documents use: waves/W0-slug.md, waves/W1-slug.md. "
                           "Or any .md/.json filename in .planning/."))
    'content
    (hasheq 'type "string" 'description "Content to write (string for .md, JSON string for .json)")
    'base_dir
    (hasheq 'type
            "string"
            'description
            "Project root directory. Auto-resolved at session start. Override only if needed."))
   'required
   '("artifact" "content")))

;; ============================================================
;; Tool handlers
;; ============================================================

(define (handle-planning-read args [exec-ctx #f])
  (define name (hash-ref args 'artifact ""))
  (define base-dir (get-base-dir args exec-ctx))
  (match name
    ["" (make-error-result "Missing required argument: artifact")]
    [(? (lambda (n) (not (valid-artifact-name? n))))
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [_
     (define content (read-planning-artifact base-dir name))
     (match content
       [#f (make-error-result (format "Artifact '~a' not found in ~a/" name planning-dir-name))]
       [(? hash?) (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string content))))]
       [_ (make-success-result (list (hasheq 'type "text" 'text content)))])]))

(define (handle-planning-write args [exec-ctx #f])
  (define name (hash-ref args 'artifact ""))
  (define content-str (hash-ref args 'content ""))
  (define base-dir (get-base-dir args exec-ctx))
  (match (list name content-str)
    [(list "" _) (make-error-result "Missing required argument: artifact")]
    [(list _ "") (make-error-result "Missing required argument: content")]
    [(list (? (lambda (n) (not (valid-artifact-name? n)))) _)
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [_
     ;; v0.21.0: Use write guard from gsd/core.rkt
     (define art-path (planning-artifact-path base-dir name))
     (define guard-arg (and art-path (path->string art-path)))
     (define guard-result
       (if guard-arg
           (gsd-write-guard guard-arg (current-pinned-dir))
           (policy-decision #t #f '())))
     (match (policy-blocked? guard-result)
       [#t (make-error-result (format "Blocked: ~a" (policy-reason guard-result)))]
       [_
        (define parsed-content
          (if (json-artifact? name)
              (with-handlers ([exn:fail? (lambda (e) e)])
                (string->jsexpr content-str))
              content-str))
        (match parsed-content
          [(? exn:fail?)
           (make-error-result (format "Invalid JSON content: ~a" (exn-message parsed-content)))]
          [_
           (define result-path (write-planning-artifact! base-dir name parsed-content))
           (if (not result-path)
               (make-error-result (format "Failed to write artifact '~a'" name))
               (begin
                 (when (and (eq? (gsd-mode) 'planning) (string=? name "PLAN"))
                   (set-gsd-mode! 'plan-written)
                   (events:emit-gsd-event! 'gsd.mode.changed
                   (make-gsd-mode-changed-event #:session-id "" #:turn-id 0 #:mode 'plan-written)))
                 (make-success-result (list (hasheq 'type
                                                    "text"
                                                    'text
                                                    (format "Written: ~a"
                                                            (path->string result-path)))))))])])]))

;; Legacy mode wrappers (DEBT-01: migrated from gsd-planning-state.rkt)
(define (gsd-mode)
  (let ([s (gsm-current)])
    (cond
      [(eq? s 'idle) #f]
      [(eq? s 'exploring) 'planning]
      [else s])))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition-to! 'exploring)]
    [(eq? v 'plan-written) (gsm-transition-to! 'plan-written)]
    [(eq? v 'executing) (gsm-transition-to! 'executing)]
    [else (gsm-transition! v)]))
