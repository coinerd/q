#lang racket/base

;; Execute body, returning make-error-result on exception.
;; Shared macro for all GitHub handler files.
(define-syntax-rule (with-error-result ctx-msg body ...)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "~a: ~a" ctx-msg (exn-message e)))
                               (make-error-result (exn-message e)))])
    body ...))

;; extensions/github/helpers.rkt — Common GitHub CLI utilities
;;
;; Extracted from github-integration.rkt to reduce its size (Q01).
;; Provides shell quoting, input validation, gh/git execution, and repo info.

(require "../../util/error/error-helpers.rkt"
         (only-in "../../util/error/errors.rkt" raise-extension-error))
(require racket/contract
         racket/file
         racket/format
         racket/path
         racket/port
         racket/string
         racket/system
         json
         ;; SEC-16 (v0.22.0): consolidated shell-quote
         (only-in "../../util/shell-quote.rkt" shell-quote)
         (only-in "../tool-api.rkt" make-success-result make-error-result tool-result?))

;; Parameters (plain)
(provide gh-binary-path
         git-binary-path
         current-gh-exec-result
         current-git-exec-result
         ;; Macro (plain)
         with-error-result
         ;; Re-export (plain)
         shell-quote
         ;; Functions (contracted)
         (contract-out
          [valid-identifier? (-> any/c boolean?)]
          [valid-number? (-> any/c boolean?)]
          [valid-state? (-> any/c boolean?)]
          [valid-method? (-> any/c boolean?)]
          [resolve-git-root (-> path-string? (or/c path? #f))]
          [resolve-planning-root (-> path-string? (or/c path? #f))]
          [run-command
           (->* ((or/c path-string? path?))
                #:rest (listof string?)
                (values exact-integer? string? string?))]
          [gh-binary (-> (or/c path? #f))]
          [git-binary (-> (or/c path? #f))]
          [gh-unavailable-error (-> tool-result?)]
          [gh-exec-result (->* () #:rest (listof string?) (values exact-integer? string? string?))]
          [git-exec-result (->* () #:rest (listof string?) (values exact-integer? string? string?))]
          [gh-success (->* () #:rest (listof string?) tool-result?)]
          [gh-success-json (->* () #:rest (listof string?) tool-result?)]
          [git-success (->* () #:rest (listof string?) tool-result?)]
          [get-repo-info (-> (values (or/c string? #f) (or/c string? #f)))]))

;; ============================================================
;; Configuration
;; ============================================================

(define gh-binary-path (make-parameter #f))
(define git-binary-path (make-parameter #f))

;; Test/integration seams. A runner receives the command arguments and returns
;; (values exit-code stdout stderr). Production leaves both parameters at #f.
(define current-gh-exec-result (make-parameter #f))
(define current-git-exec-result (make-parameter #f))

;; ============================================================
;; Shell helpers + input validation
;; ============================================================

;; shell-quote imported from util/shell-quote.rkt (SEC-16 consolidation)

;; Validate that a string contains only safe identifier characters
(define (valid-identifier? s)
  (and (string? s) (regexp-match? #rx"^[a-zA-Z0-9_.-]+$" s)))

;; Validate issue/PR number is a positive integer or numeric string
(define (valid-number? n)
  (or (and (integer? n) (positive? n))
      (and (string? n)
           (regexp-match? #rx"^[0-9]+$" n)
           (let ([parsed (string->number n)]) (and (integer? parsed) (positive? parsed))))))

;; Validate state arg is one of known values
(define valid-states '("open" "closed" "all"))

(define (valid-state? s)
  (and (string? s) (member s valid-states) #t))

;; Validate merge method is one of known values
(define valid-methods '("squash" "merge" "rebase"))

(define (valid-method? s)
  (and (string? s) (member s valid-methods) #t))

;; Resolve repository and planning coordinates independently. In the supported
;; split layout, execution starts at <project>/ while Git lives at <project>/q
;; and canonical planning state lives at <project>/.planning.
(define (complete-simple-path p)
  (simplify-path (path->complete-path p)))

(define (git-marker? dir)
  (define marker (build-path dir ".git"))
  (or (directory-exists? marker) (file-exists? marker)))

(define (parent-path dir)
  (define-values (parent _name _dir?) (split-path dir))
  (and (path? parent) (not (equal? parent dir)) (simplify-path parent)))

(define (resolve-git-root start-dir)
  (let loop ([dir (complete-simple-path start-dir)])
    (define q-candidate (build-path dir "q"))
    (cond
      [(git-marker? dir) dir]
      [(git-marker? q-candidate) (simplify-path q-candidate)]
      [else
       (define parent (parent-path dir))
       (and parent (loop parent))])))

(define (resolve-planning-root start-dir)
  (define git-root (resolve-git-root start-dir))
  (define split-parent (and git-root (parent-path git-root)))
  (define split-planning (and split-parent (build-path split-parent ".planning")))
  (cond
    ;; The outer planning tree is canonical in <project>/q split layouts,
    ;; even when the tracked repository also contains q/.planning.
    [(and split-planning
          (equal? (path->string (file-name-from-path git-root)) "q")
          (directory-exists? split-planning))
     (simplify-path split-planning)]
    [else
     (let loop ([dir (complete-simple-path start-dir)])
       (define planning (build-path dir ".planning"))
       (cond
         [(directory-exists? planning) (simplify-path planning)]
         [else
          (define parent (parent-path dir))
          (and parent (loop parent))]))]))

;; Run a command with explicit arg list — no /bin/sh interpolation.
;; Returns (values exit-code stdout stderr)
(define (run-command cmd . args)
  (define-values (sp stdout-in stdin-out stderr-in) (apply subprocess #f #f #f cmd args))
  (define out-str (port->string stdout-in))
  (define err-str (port->string stderr-in))
  (close-input-port stdout-in)
  (close-input-port stderr-in)
  (when (output-port? stdin-out)
    (close-output-port stdin-out))
  (subprocess-wait sp)
  (values (subprocess-status sp) out-str err-str))

;; ============================================================
;; gh / git execution
;; ============================================================

(define (gh-binary)
  (define p (gh-binary-path))
  (cond
    [(eq? p 'disabled) #f]
    [p p]
    [else (find-executable-path "gh")]))

(define (git-binary)
  (or (git-binary-path) (find-executable-path "git")))

(define (gh-unavailable-error)
  (make-error-result "GitHub CLI (gh) not found. Install from https://cli.github.com"))

(define (gh-exec-result . args)
  (define runner (current-gh-exec-result))
  (if runner
      (apply runner args)
      (let ([bin (gh-binary)])
        (unless bin
          (raise-extension-error "GitHub CLI not found" 'github 'cli-check))
        (apply run-command bin args))))

(define (git-exec-result . args)
  (define runner (current-git-exec-result))
  (if runner
      (apply runner args)
      (let ([bin (git-binary)])
        (unless bin
          (error 'git "git not found"))
        (apply run-command bin args))))

(define (gh-success . args)
  (define-values (ec out err) (apply gh-exec-result args))
  (if (= ec 0)
      (let ([text (string-trim out)])
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (if (string=? text "")
                                               (string-trim err)
                                               text)))))
      (make-error-result (format "gh failed (exit ~a): ~a" ec (string-trim err)))))

(define (gh-success-json . args)
  (define-values (ec out err) (apply gh-exec-result args))
  (if (= ec 0)
      (let* ([raw (string-trim out)]
             [parsed (with-safe-fallback #f (string->jsexpr raw))])
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (if parsed
                                               (jsexpr->string parsed)
                                               raw)))))
      (make-error-result (format "gh failed (exit ~a): ~a" ec (string-trim err)))))

(define (git-success . args)
  (define-values (ec out err) (apply git-exec-result args))
  (if (= ec 0)
      (make-success-result (list (hasheq 'type
                                         "text"
                                         'text
                                         (let ([text (string-trim out)])
                                           (if (string=? text "")
                                               (string-trim err)
                                               text)))))
      (make-error-result (format "git failed (exit ~a): ~a" ec (string-trim err)))))

(define (get-repo-info)
  (define bin (gh-binary))
  (cond
    [(not bin) (values #f #f)]
    [else
     (define-values (ec out _)
       (run-command bin "repo" "view" "--json" "nameWithOwner" "-q" ".nameWithOwner"))
     (cond
       [(not (= ec 0)) (values #f #f)]
       [else
        (define parts (string-split (string-trim out) "/"))
        (if (>= (length parts) 2)
            (values (car parts) (cadr parts))
            (values #f #f))])]))
