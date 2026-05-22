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

(require "../../util/error-helpers.rkt"
         (only-in "../../util/errors.rkt" raise-extension-error))
(require racket/contract
         racket/format
         racket/port
         racket/string
         racket/system
         json
         ;; SEC-16 (v0.22.0): consolidated shell-quote
         (only-in "../../util/shell-quote.rkt" shell-quote)
         (only-in "../tool-api.rkt" make-success-result make-error-result))

;; Parameter (plain)
(provide gh-binary-path
         ;; Macro (plain)
         with-error-result
         ;; Re-export (plain)
         shell-quote
         ;; Functions (contracted)
         (contract-out [valid-identifier? (-> any/c boolean?)]
                       [valid-number? (-> any/c boolean?)]
                       [valid-state? (-> any/c boolean?)]
                       [valid-method? (-> any/c boolean?)]
                       [run-command (->* (any/c) #:rest (listof any/c) (values any/c any/c any/c))]
                       [gh-binary (-> any/c)]
                       [git-binary (-> any/c)]
                       [gh-unavailable-error (-> any/c)]
                       [gh-exec-result (->* () #:rest (listof any/c) (values any/c any/c any/c))]
                       [git-exec-result (->* () #:rest (listof any/c) (values any/c any/c any/c))]
                       [gh-success (->* () #:rest (listof any/c) any/c)]
                       [gh-success-json (->* () #:rest (listof any/c) any/c)]
                       [git-success (->* () #:rest (listof any/c) any/c)]
                       [get-repo-info (-> (values any/c any/c))]))

;; ============================================================
;; Configuration
;; ============================================================

(define gh-binary-path (make-parameter #f))

;; ============================================================
;; Shell helpers + input validation
;; ============================================================

;; shell-quote imported from util/shell-quote.rkt (SEC-16 consolidation)

;; Validate that a string contains only safe identifier characters
(define (valid-identifier? s)
  (and (string? s) (regexp-match? #rx"^[a-zA-Z0-9_.-]+$" s)))

;; Validate issue/PR number is a positive integer or numeric string
(define (valid-number? n)
  (or (and (integer? n) (positive? n)) (and (string? n) (regexp-match? #rx"^[0-9]+$" n))))

;; Validate state arg is one of known values
(define valid-states '("open" "closed" "all"))

(define (valid-state? s)
  (and (string? s) (member s valid-states) #t))

;; Validate merge method is one of known values
(define valid-methods '("squash" "merge" "rebase"))

(define (valid-method? s)
  (and (string? s) (member s valid-methods) #t))

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
  (find-executable-path "git"))

(define (gh-unavailable-error)
  (make-error-result "GitHub CLI (gh) not found. Install from https://cli.github.com"))

(define (gh-exec-result . args)
  (define bin (gh-binary))
  (unless bin
    (raise-extension-error "GitHub CLI not found" 'github 'cli-check))
  (apply run-command bin args))

(define (git-exec-result . args)
  (define bin (git-binary))
  (unless bin
    (error 'git "git not found"))
  (apply run-command bin args))

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
  (define-values (ec out _)
    (run-command (gh-binary) "repo" "view" "--json" "nameWithOwner" "-q" ".nameWithOwner"))
  (cond
    [(not (= ec 0)) (values #f #f)]
    [else
     (define parts (string-split (string-trim out) "/"))
     (if (>= (length parts) 2)
         (values (car parts) (cadr parts))
         (values #f #f))]))
