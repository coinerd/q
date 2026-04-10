#lang racket/base

;; extensions/tiers.rkt — extension capability tiers and API version validation
;;
;; Provides:
;;   - Five capability tiers: hooks, commands, session, providers, tui
;;   - Tier→capability mapping (cumulative, ordered)
;;   - API version validation (current supported: "1")
;;   - Tier enforcement: hook-point→tier mapping and validation functions
;;
;; Hook point → tier mapping:
;;   context-assembly, tool-pre-exec, tool-post-exec        → 'hooks
;;   command-*                                               → 'commands
;;   session-start, session-end, pre-compact, post-compact   → 'session
;;   provider-register                                       → 'providers
;;   tui-*                                                   → 'tui
;;   (unknown hook points)                                   → 'hooks

(require racket/contract
         "api.rkt")

(provide
 ;; Tier predicate and queries
 tier?
 tier-capabilities
 capability-allowed?

 ;; API version validation
 valid-api-version?
 supported-api-versions

 ;; Extension tier validation
 validate-extension-tier
 extension-tier-valid?

 ;; Hook-point → tier mapping (exposed for introspection)
 hook-point-tier)

;; ============================================================
;; Tier ordering and capability map
;; ============================================================

;; Ordered from lowest to highest privilege.
;; Each tier includes all capabilities of lower tiers.
(define tier-order '(hooks commands session providers tui))

;; Cumulative capability map: each tier has all listed capabilities
;; plus those of lower tiers.
(define capabilities-map
  (hasheq 'hooks      '(hook-dispatch)
          'commands   '(hook-dispatch command-register)
          'session    '(hook-dispatch command-register
                        session-lifecycle compaction-hooks)
          'providers  '(hook-dispatch command-register
                        session-lifecycle compaction-hooks
                        provider-register)
          'tui        '(hook-dispatch command-register
                        session-lifecycle compaction-hooks
                        provider-register
                        tui-panels tui-keybindings)))

;; ============================================================
;; Hook point → tier mapping
;; ============================================================

;; Maps hook point symbols to the minimum tier required.
;; Unknown hook points default to 'hooks (the lowest tier).

(define hook-point-map
  (hasheq 'context-assembly  'hooks
          'tool-pre-exec     'hooks
          'tool-post-exec    'hooks
          'command-dispatch  'commands
          'command-register  'commands
          'command-list      'commands
          'session-start     'session
          'session-end       'session
          'pre-compact       'session
          'post-compact      'session
          'provider-register 'providers
          'tui-panel         'tui
          'tui-panels        'tui
          'tui-keybindings   'tui
          'tui-keybinding-help 'tui
          'tui-render        'tui))

;; Tier precedence for comparison: lower index = lower privilege
(define tier-index
  (for/hasheq ([t tier-order] [i (in-naturals)])
    (values t i)))

;; ============================================================
;; tier? : any/c -> boolean?
;; ============================================================
;; Returns #t if v is one of the five valid tier symbols.

(define (tier? v)
  (and (symbol? v) (hash-has-key? tier-index v)))

;; ============================================================
;; tier-capabilities : symbol? -> (listof symbol?)
;; ============================================================
;; Returns the cumulative capability list for the given tier.

(define (tier-capabilities t)
  (unless (tier? t)
    (raise-argument-error 'tier-capabilities "valid tier?" t))
  (hash-ref capabilities-map t))

;; ============================================================
;; capability-allowed? : symbol? symbol? -> boolean?
;; ============================================================
;; Returns #t if the given tier grants the specified capability.

(define (capability-allowed? t cap)
  (and (member cap (tier-capabilities t)) #t))

;; ============================================================
;; API version validation
;; ============================================================

(define supported-api-versions '("1"))

(define (valid-api-version? v)
  (and (member v supported-api-versions) #t))

;; ============================================================
;; Hook point → tier lookup
;; ============================================================
;; Returns the minimum tier required for a hook point.
;; Unknown hook points default to 'hooks.

(define (hook-point-tier hook-point)
  (hash-ref hook-point-map hook-point 'hooks))

;; ============================================================
;; Tier comparison helper
;; ============================================================
;; Returns #t if required-tier is at or below declared-tier.

(define (tier<=? required-tier declared-tier)
  (<= (hash-ref tier-index required-tier)
      (hash-ref tier-index declared-tier)))

;; ============================================================
;; validate-extension-tier : extension? symbol? -> (or/c #t (listof string?))
;; ============================================================
;; Checks that all hook points in the extension's hooks are allowed
;; at the declared tier. Returns #t if valid, or a list of error
;; message strings describing violations.

(define (validate-extension-tier ext declared-tier)
  (define hooks (extension-hooks ext))
  (define violations
    (for/list ([(hook-point _handler) (in-hash hooks)]
               #:when (not (tier<=? (hook-point-tier hook-point) declared-tier)))
      (format "hook '~a' requires tier '~a' but extension '~a' declares tier '~a'"
              hook-point
              (hook-point-tier hook-point)
              (extension-name ext)
              declared-tier)))
  (if (null? violations) #t violations))

;; ============================================================
;; extension-tier-valid? : extension? symbol? -> boolean?
;; ============================================================
;; Full validation: checks API version is supported AND all hooks
;; are within the declared tier. Returns #t only if both pass.

(define (extension-tier-valid? ext declared-tier)
  (and (valid-api-version? (extension-api-version ext))
       (eq? #t (validate-extension-tier ext declared-tier))))
