#lang racket/base

;; extensions/ext-commands.rkt — Extension slash command registration
;;
;; FEAT-60: Extension-facing API for registering slash commands.
;; The command-registry in extension-ctx should be a (boxof hash?).
;; Extensions register commands which appear in the TUI command palette.

(require racket/contract
         racket/list
         "context.rkt"
         "../util/command-types.rkt")

(provide (contract-out
          [ext-register-command!
           (-> extension-ctx? string? string? symbol? (listof string?) (listof string?) void?)]
          [ext-unregister-command! (-> extension-ctx? string? void?)]
          [ext-lookup-command (-> extension-ctx? string? (or/c cmd-entry? #f))]
          [ext-list-commands (-> extension-ctx? (listof cmd-entry?))]))

;; Get the underlying hash from the registry (handles box or raw hash)
(define (get-reg-hash ctx)
  (define reg (ctx-command-registry ctx))
  (cond
    [(not reg) #f]
    [(box? reg) (unbox reg)]
    [(hash? reg) reg]
    [else #f]))

;; Update the registry hash (handles box)
(define (update-reg-hash! ctx new-hash)
  (define reg (ctx-command-registry ctx))
  (cond
    [(box? reg) (set-box! reg new-hash)]
    [(hash? reg) (void)] ;; immutable, can't update
    [else (void)]))

(define (ext-register-command! ctx name summary category args-spec aliases)
  (define reg-hash (get-reg-hash ctx))
  (unless reg-hash
    (error 'ext-register-command!
           "no command-registry in extension context. Set #:command-registry."))
  (define entry (cmd-entry name summary category args-spec aliases))
  (define new-hash (register-command! reg-hash entry))
  (update-reg-hash! ctx new-hash))

(define (ext-unregister-command! ctx name)
  (define reg-hash (get-reg-hash ctx))
  (unless reg-hash
    (error 'ext-unregister-command! "no command-registry in extension context"))
  ;; Rebuild hash without the named command
  (define new-hash
    (for/hash ([(k v) (in-hash reg-hash)]
               #:unless (equal? k name))
      (values k v)))
  (update-reg-hash! ctx new-hash))

(define (ext-lookup-command ctx name)
  (define reg-hash (get-reg-hash ctx))
  (if reg-hash
      (lookup-command reg-hash name)
      #f))

(define (ext-list-commands ctx)
  (define reg-hash (get-reg-hash ctx))
  (if reg-hash
      (sort (hash-values reg-hash) string<? #:key cmd-entry-name)
      '()))
