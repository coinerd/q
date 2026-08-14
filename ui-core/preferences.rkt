#lang racket/base

;; q/ui-core/preferences.rkt — unified frontend-neutral user preferences
;;
;; W4 (v0.99.96): ONE preference surface consumed by BOTH the TUI and the
;; GUI so "customization" is a product capability rather than a pile of
;; unrelated per-frontend settings.  Pure data + pure resolution — no
;; terminal, no widgets, no config I/O (frontends feed parsed config in).
;;
;; Covered surfaces (wave W4 action #3):
;;   reasoning-visibility : 'session | 'scrollback | 'never
;;   preview-length       : exact-nonnegative-integer?
;;   composer-max-rows    : exact-positive-integer?  (default 6, matches TUI)
;;   submit-key           : symbol for the submit modifier policy
;;   newline-keys         : list of modifier symbols that insert a newline
;;   keybindings          : hash intent-symbol -> key-spec (customizations)

(require racket/contract)

(provide preferences?
         default-preferences
         merge-preferences
         preferences->hash
         (contract-out
          [preferences-ref (->* (preferences? symbol?) (any/c) any/c)]
          [set-preference (-> preferences? symbol? any/c preferences?)]
          [reasoning-visibility-policy (-> preferences? (or/c 'session 'scrollback 'never))]
          [reasoning-persist? (-> preferences? boolean?)]
          [preview-length (-> preferences? exact-nonnegative-integer?)]
          [max-composer-rows (-> preferences? exact-positive-integer?)]
          [submit-key-policy (-> preferences? symbol?)]
          [newline-key-policies (-> preferences? (listof symbol?))]
          ;; Frontend-neutral key resolution: BOTH frontends call this so the
          ;; same physical key resolves to the same named intent.
          [resolve-key->intent
           (->* (any/c)
                (#:shift? boolean?
                 #:control? boolean?
                 #:alt? boolean?
                 #:at-start? (or/c boolean? #f)
                 #:at-end? (or/c boolean? #f)
                 #:prefs preferences?)
                (or/c symbol? #f))]
          [normalize-keybinding-spec (-> any/c any/c)]))

(define preferences? hash?)

;; ── Defaults ────────────────────────────────────────────────
;; Reasoning visible during the session but NOT written to the persisted
;; transcript by default ('scrollback = visible + persisted, 'never = hidden).
(define (default-preferences)
  (hasheq 'reasoning-visibility 'session
          'preview-length 400
          'composer-max-rows 6
          'submit-key 'enter
          'newline-keys '(shift control)
          'keybindings (hasheq)))

(define (normalize-keybinding-spec spec)
  (cond
    [(symbol? spec) (list spec)]
    [(string? spec) (list (string->symbol spec))]
    [(and (list? spec)
          (andmap (lambda (x) (or (symbol? x) (string? x))) spec))
     (map (lambda (x) (if (string? x) (string->symbol x) x)) spec)]
    [else #f]))

(define (merge-preferences prefs overrides)
  (define ov (if (hash? overrides) overrides (hasheq)))
  (for/fold ([acc (if (hash? prefs) prefs (default-preferences))])
            ([(k v) (in-hash ov)])
    (hash-set acc k v)))

(define (preferences->hash prefs) (if (hash? prefs) prefs (default-preferences)))

(define (preferences-ref prefs key [default #f])
  (hash-ref (preferences->hash prefs) key default))

(define (set-preference prefs key value)
  (hash-set (preferences->hash prefs) key value))

;; ── Typed accessors (with validation + fallback) ────────────

(define (reasoning-visibility-policy prefs)
  (define v (preferences-ref prefs 'reasoning-visibility 'session))
  (if (memq v '(session scrollback never)) v 'session))

(define (reasoning-persist? prefs)
  (eq? (reasoning-visibility-policy prefs) 'scrollback))

(define (preview-length prefs)
  (define v (preferences-ref prefs 'preview-length 400))
  (if (exact-nonnegative-integer? v) v 400))

(define (max-composer-rows prefs)
  (define v (preferences-ref prefs 'composer-max-rows 6))
  (if (and (integer? v) (> v 0)) v 6))

(define (submit-key-policy prefs)
  (define v (preferences-ref prefs 'submit-key 'enter))
  (if (symbol? v) v 'enter))

(define (newline-key-policies prefs)
  (define v (preferences-ref prefs 'newline-keys '(shift control)))
  (if (and (list? v) (andmap symbol? v)) v '(shift control)))

;; ── Key → named-intent resolution (shared by TUI and GUI) ───
;;
;; key: a key designator.  Both frontends normalize their native key events
;; to one of: 'return | 'n | a character | a symbol — the resolution below
;; only cares about 'return plus the modifier flags for composer policy.
;;
;; Custom keybindings (prefs 'keybindings) map an intent symbol to a key
;; spec and take precedence over the built-in policy for THAT intent.
;;
;; Returns a named intent symbol or #f:
;;   'ui.composer.submit | 'ui.composer.insert-newline
;;   'composer.history-up | 'composer.history-down
;;   'ui.transcript.toggle-detail
(define (resolve-key->intent key
                             #:shift? [shift? #f]
                             #:control? [control? #f]
                             #:alt? [alt? #f]
                             #:at-start? [at-start? #f]
                             #:at-end? [at-end? #f]
                             #:prefs [prefs (default-preferences)])
  (define (custom intent-kind)
    (define bindings (preferences-ref prefs 'keybindings (hasheq)))
    (and (hash? bindings)
         (hash-ref bindings intent-kind #f)))
  ;; 1. Custom bindings win: does any intent's spec match this key+modifiers?
  (define matched-custom
    (for/or ([kind (in-list '(ui.composer.submit
                              ui.composer.insert-newline
                              composer.history-up
                              composer.history-down
                              ui.transcript.toggle-detail))])
      (define spec (normalize-keybinding-spec (custom kind)))
      (and spec
           ;; A custom spec matches when the primary key matches and no
           ;; modifier required by the spec is missing.
           (member key spec)
           (for/and ([mod (in-list spec)])
             (or (eq? mod key)
                 (and (eq? mod 'shift) shift?)
                 (and (eq? mod 'control) control?)
                 (and (eq? mod 'alt) alt?)
                 ;; Negative modifiers: 'no-shift etc.
                 (and (eq? mod 'no-shift) (not shift?))
                 (and (eq? mod 'no-control) (not control?))))
           kind)))
  (cond
    [matched-custom matched-custom]
    ;; 2. Built-in policy
    [(and (or (eq? key 'return) (eq? key #\return))
          (or shift? control?))
     (if (or (memq 'shift (newline-key-policies prefs)) ; any newline policy key
             (memq 'control (newline-key-policies prefs)))
         'ui.composer.insert-newline
         #f)]
    [(or (eq? key 'return) (eq? key #\return))
     (and (not alt?) (eq? (submit-key-policy prefs) 'enter)
          'ui.composer.submit)]
    ;; History: Up at buffer start, Down at buffer end (both frontends).
    ;; at-start?/at-end? are tri-state: #t = yes, #f = unknown (eligible),
    ;; 'no = definitively not at boundary (cursor moves instead).
    [(eq? key 'up)
     (if (and at-start? (eq? at-start? 'no)) #f 'composer.history-up)]
    [(eq? key 'down)
     (if (and at-end? (eq? at-end? 'no)) #f 'composer.history-down)]
    ;; Detail toggle: Ctrl+O — same in both frontends (W2 parity).
    [(and control? (eq? key #\o)) 'ui.transcript.toggle-detail]
    [else #f]))

(module+ test
  (require rackunit)
  (define p (default-preferences))
  ;; Submit / newline policy
  (check-equal? (resolve-key->intent 'return #:prefs p) 'ui.composer.submit)
  (check-equal? (resolve-key->intent 'return #:shift? #t #:prefs p)
                'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent 'return #:control? #t #:prefs p)
                'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent #\return #:shift? #t #:prefs p)
                'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent 'return #:alt? #t #:prefs p) #f)
  ;; Detail toggle
  (check-equal? (resolve-key->intent #\o #:control? #t #:prefs p)
                'ui.transcript.toggle-detail)
  ;; History
  (check-equal? (resolve-key->intent 'up #:at-start? #t #:prefs p)
                'composer.history-up)
  (check-equal? (resolve-key->intent 'down #:at-end? #t #:prefs p)
                'composer.history-down)
  ;; Preference effects
  (check-equal? (reasoning-visibility-policy p) 'session)
  (check-false (reasoning-persist? p))
  (check-true (reasoning-persist?
               (set-preference p 'reasoning-visibility 'scrollback)))
  (check-equal? (max-composer-rows p) 6)
  (check-equal? (max-composer-rows (set-preference p 'composer-max-rows 12)) 12)
  (check-equal? (max-composer-rows (set-preference p 'composer-max-rows -3)) 6)
  (check-equal? (preview-length p) 400)
  ;; Custom keybinding overrides policy
  (define p2 (set-preference p 'keybindings
                             (hasheq 'ui.composer.submit
                                     '(control return))))
  (check-equal? (resolve-key->intent 'return #:control? #t #:prefs p2)
                'ui.composer.submit)
  (check-equal? (resolve-key->intent 'return #:prefs p2) 'ui.composer.submit)
  ;; merge
  (define p3 (merge-preferences p (hasheq 'composer-max-rows 3)))
  (check-equal? (max-composer-rows p3) 3)
  (check-equal? (reasoning-visibility-policy p3) 'session))
