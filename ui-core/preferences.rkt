#lang racket/base

;; q/ui-core/preferences.rkt — unified frontend-neutral user preferences
;;
;; W4 (v0.99.96): ONE preference surface consumed by BOTH the TUI and the
;; GUI so "customization" is a product capability rather than a pile of
;; unrelated per-frontend settings.  Pure data + pure resolution — no
;; terminal, no widgets.
;;
;; W3 (v1.00.02): the module additionally owns the ONE configuration
;; loader so both frontends start from the SAME immutable snapshot:
;; `load-preferences` reads the user config file (see
;; `preferences-config-paths`), validates it against the schema below,
;; and merges it over `default-preferences`.  Invalid values fall back to
;; defaults with a diagnostic (ui-core/ui-diagnostics.rkt).
;;
;; Covered surfaces (wave W4 action #3):
;;   reasoning-visibility : 'session | 'scrollback | 'never
;;   preview-length       : exact-nonnegative-integer?
;;   composer-max-rows    : exact-positive-integer?  (default 6, matches TUI)
;;   submit-key           : symbol for the submit modifier policy
;;   newline-keys         : list of modifier symbols that insert a newline
;;   keybindings          : hash intent-symbol -> key-spec (customizations)

(require racket/contract
         racket/string
         racket/list
         json
         (only-in "ui-diagnostics.rkt" ui-diagnostic!))

(provide preferences?
         default-preferences
         merge-preferences
         preferences->hash
         current-preferences
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
          [resolve-custom-key->intent
           (->* (any/c)
                (#:shift? boolean? #:control? boolean? #:alt? boolean? #:prefs preferences?)
                (or/c symbol? #f))]
          [normalize-keybinding-spec (-> any/c any/c)]
          ;; Configuration loader (W3, v1.00.02)
          [preferences-config-paths (-> (listof path?))]
          [default-preferences-path (-> path?)]
          [read-preferences-file (-> path? any/c)]
          [normalize-config-overrides (-> any/c (values hash? (listof string?)))]
          [parse-preferences (-> any/c preferences?)]
          [load-preferences (->* () ((or/c path? #f)) preferences?)]
          [keybinding-string->spec (-> string? any/c)]
          [collapsed-preview-lines (->* () (preferences?) exact-positive-integer?)]
          [preference-loads-artifact? (->* () (preferences?) boolean?)]))

(define preferences? hash?)

;; ── Defaults ────────────────────────────────────────────────
;; Reasoning visible during the session but NOT written to the persisted
;; transcript by default ('scrollback = visible + persisted, 'never = hidden).
(define (default-preferences)
  (hasheq 'reasoning-visibility
          'session
          'preview-length
          400
          'composer-max-rows
          6
          'submit-key
          'enter
          'newline-keys
          '(shift control)
          'keybindings
          (hasheq)))

(define (normalize-keybinding-spec spec)
  (cond
    [(symbol? spec) (list spec)]
    [(string? spec) (list (string->symbol spec))]
    [(and (list? spec) (andmap (lambda (x) (or (symbol? x) (string? x))) spec))
     (map (lambda (x)
            (if (string? x)
                (string->symbol x)
                x))
          spec)]
    [else #f]))

(define (merge-preferences prefs overrides)
  (define ov
    (if (hash? overrides)
        overrides
        (hasheq)))
  (for/fold ([acc (if (hash? prefs)
                      prefs
                      (default-preferences))])
            ([(k v) (in-hash ov)])
    (hash-set acc k v)))

(define (preferences->hash prefs)
  (if (hash? prefs)
      prefs
      (default-preferences)))

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
  ;; Canonical internal representation is 'enter (plain Enter submits);
  ;; the loader normalizes the JSON value "enter" to 'return, so accept
  ;; BOTH spellings here — otherwise a config file would break submit.
  (define v (preferences-ref prefs 'submit-key 'enter))
  (cond
    [(memq v '(enter return)) 'enter]
    [(symbol? v) v]
    [else 'enter]))

(define (newline-key-policies prefs)
  (define v (preferences-ref prefs 'newline-keys '(shift control)))
  (if (and (list? v) (andmap symbol? v))
      v
      '(shift control)))

;; ── Key → named-intent resolution (shared by TUI and GUI) ───
;;
;; key: a key designator.  Both frontends normalize their native key events
;; to one of: 'return | 'n | a character | a symbol — the resolution below
;; only cares about 'return plus the modifier flags for composer policy.
;;
;; Custom keybindings (prefs 'keybindings) map an intent symbol to a key
;; spec and take precedence over the built-in policy for THAT intent.

;; A spec element matches a key either directly (symbol key) or when the
;; key is a character whose one-character symbol form is in the spec
;; (JSON configs write "ctrl+m" -> '(control m); frontends report #\m).
(define (key-spec-match? key spec)
  (or (member key spec) (and (char? key) (memq (string->symbol (string key)) spec))))
;;
;; Returns a named intent symbol or #f:
;;   'ui.composer.submit | 'ui.composer.insert-newline
;;   'composer.history-up | 'composer.history-down
;;   'ui.transcript.toggle-detail
;; Custom-bindings-ONLY resolution (W3): returns the intent kind ONLY
;; when a user-configured keybinding spec matches this key + modifiers.
;; Callers use this to give custom bindings ABSOLUTE precedence (custom
;; -> keymap -> built-in fallbacks); `resolve-key->intent` itself also
;; consults custom bindings first, then the built-in policy.
(define (resolve-custom-key->intent key
                                    #:shift? [shift? #f]
                                    #:control? [control? #f]
                                    #:alt? [alt? #f]
                                    #:prefs [prefs (current-preferences)])
  (define bindings (preferences-ref prefs 'keybindings (hasheq)))
  ;; "ctrl+m" strings parse via keybinding-string->spec; pre-parsed
  ;; symbol/list specs (programmatic overrides, tests) normalize directly.
  (define (binding-spec v)
    (cond
      [(string? v) (keybinding-string->spec v)]
      [else (normalize-keybinding-spec v)]))
  ;; Frontends report character keys (#\m); specs name them as symbols (m).
  (define key-sym (and (char? key) (string->symbol (string key))))
  (and (hash? bindings)
       (for/or ([kind (in-list '(ui.composer.submit ui.composer.insert-newline
                                                    composer.history-up
                                                    composer.history-down
                                                    ui.transcript.toggle-detail))])
         (define spec (binding-spec (hash-ref bindings kind #f)))
         (and spec
              ;; A custom spec matches when the primary key matches and no
              ;; modifier required by the spec is missing.
              (key-spec-match? key spec)
              (for/and ([mod (in-list spec)])
                (or (eq? mod key)
                    (eq? mod key-sym)
                    (and (eq? mod 'shift) shift?)
                    (and (eq? mod 'control) control?)
                    (and (eq? mod 'alt) alt?)
                    ;; Negative modifiers: 'no-shift etc.
                    (and (eq? mod 'no-shift) (not shift?))
                    (and (eq? mod 'no-control) (not control?))))
              kind))))

(define (resolve-key->intent key
                             #:shift? [shift? #f]
                             #:control? [control? #f]
                             #:alt? [alt? #f]
                             #:at-start? [at-start? #f]
                             #:at-end? [at-end? #f]
                             #:prefs [prefs (current-preferences)])
  ;; 1. Custom bindings win: does any intent's spec match this key+modifiers?
  (define matched-custom
    (resolve-custom-key->intent key #:shift? shift? #:control? control? #:alt? alt? #:prefs prefs))
  (cond
    [matched-custom matched-custom]
    ;; 2. Built-in policy
    [(and (or (eq? key 'return) (eq? key #\return)) (or shift? control?))
     (if (or (memq 'shift (newline-key-policies prefs)) ; any newline policy key
             (memq 'control (newline-key-policies prefs)))
         'ui.composer.insert-newline
         #f)]
    [(or (eq? key 'return) (eq? key #\return))
     (and (not alt?) (eq? (submit-key-policy prefs) 'enter) 'ui.composer.submit)]
    ;; History: Up at buffer start, Down at buffer end (both frontends).
    ;; at-start?/at-end? are tri-state: #t = yes, #f = unknown (eligible),
    ;; 'no = definitively not at boundary (cursor moves instead).
    [(eq? key 'up) (if (and at-start? (eq? at-start? 'no)) #f 'composer.history-up)]
    [(eq? key 'down) (if (and at-end? (eq? at-end? 'no)) #f 'composer.history-down)]
    ;; Detail toggle: Ctrl+O — same in both frontends (W2 parity).
    [(and control? (eq? key #\o)) 'ui.transcript.toggle-detail]
    [else #f]))

;; -- Configuration loader (W3, v1.00.02) ----------------------
;;
;; ONE loader for BOTH frontends.  The user config file (JSON object,
;; see `preferences-config-paths`) is read, validated against the
;; schema below, and merged over `default-preferences` via
;; `merge-preferences`, producing ONE immutable snapshot.  Invalid
;; or unknown values fall back to defaults with a diagnostic
;; ('preferences.invalid, ui-core/ui-diagnostics.rkt).
;;
;; Schema (JSON keys -> preference keys):
;;   "reasoning-visibility"  in {"session","scrollback","never"}
;;   "disclosure-preview-length" | "preview-length"   nonneg int
;;   "max-composer-rows"     | "composer-max-rows"    pos int
;;   "submit-key"            non-empty string         (e.g. "enter")
;;   "newline-keys"          array of "shift"|"control"|"alt"
;;   "keybindings"           object: intent name -> "ctrl+m" | ["shift","return"]
;;
;; The loaded snapshot is installed into `current-preferences` by the
;; frontend bootstrap (tui-init / gui main) so leaf modules read the
;; SAME snapshot without threading it through every call.

(define (preferences-config-paths)
  (define env (current-environment-variables))
  (define (env-path var)
    (define b (environment-variables-ref env var))
    (and b (bytes->path b)))
  (define home (find-system-path 'home-dir))
  (define xdg (env-path #"XDG_CONFIG_HOME"))
  (filter values
          (list (env-path #"Q_PREFERENCES_FILE")
                (and xdg (build-path xdg "q" "preferences.json"))
                (build-path home ".config" "q" "preferences.json")
                (build-path home ".q" "preferences.json"))))

(define (default-preferences-path)
  (car (preferences-config-paths)))

;; read-json yields SYMBOL-keyed hashes for JSON objects, while the schema
;; normalizer below works on STRING keys (it formats/diagnoses them).
;; Convert once, recursively, at the read boundary so the normalizer sees
;; one canonical shape regardless of the JSON reader's key representation.
(define (jsexpr-keystrings v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (if (string? k)
                   k
                   (format "~a" k))
               (jsexpr-keystrings val)))]
    [(list? v) (map jsexpr-keystrings v)]
    [else v]))

;; Read + parse ONE config file.  Returns the parsed jsexpr, or #f when
;; the file is missing, unreadable, or not valid JSON (a diagnostic is
;; emitted for the latter two).  Never raises.
(define (read-preferences-file path)
  (with-handlers ([exn:fail? (lambda (e)
                               (ui-diagnostic! 'preferences.invalid
                                               (if (path? path)
                                                   (path->string path)
                                                   "config")
                                               (exn-message e))
                               #f)])
    (if (and (path? path) (file-exists? path))
        (jsexpr-keystrings (call-with-input-file path read-json))
        #f)))

;; "ctrl+m" / "control+m" -> '(control m); "return" -> '(return);
;; ["shift","return"] -> '(shift return).  #f when unparseable.
(define (keybinding-string->spec str)
  (define mods '("ctrl" "control" "alt" "meta" "shift" "cmd"))
  (define parts (string-split (string-trim str) "+"))
  (define (mod? p)
    (member (string-downcase p) mods))
  (define mod-syms
    (for/list ([p (in-list parts)]
               #:when (mod? p))
      (string->symbol (if (string-ci=? p "ctrl")
                          "control"
                          (string-downcase p)))))
  (define primary (findf (lambda (p) (not (mod? p))) parts))
  (and primary
       (append mod-syms
               (list (string->symbol (case (string-downcase primary)
                                       [("enter") "return"]
                                       [("esc") "escape"]
                                       [else (string-downcase primary)]))))))

;; Validate + normalize a parsed config value.  Returns two values:
;; a normalized overrides hash (symbol keys) and a list of human-readable
;; problems (one per dropped value).  PURE - no I/O, no diagnostics.
(define (normalize-config-overrides src)
  (define (as-int v)
    (and (real? v) (integer? v) (inexact->exact v)))
  (define (problem acc problems msg)
    (values acc (cons msg problems)))
  (define (accept acc k v problems)
    (values (hash-set acc k v) problems))
  (cond
    [(not src) (values (hasheq) '())]
    [(not (hash? src))
     (values (hasheq) (list (format "preferences config must be a JSON object, got: ~s" src)))]
    [else
     (define-values (acc problems)
       (for/fold ([acc (hasheq)]
                  [problems '()])
                 ([(k v) (in-hash src)])
         (cond
           [(not (string? k)) (problem acc problems (format "ignoring non-string key: ~s" k))]
           [(or (string=? k "preview-length") (string=? k "disclosure-preview-length"))
            (define n (as-int v))
            (if (and n (>= n 0))
                (accept acc 'preview-length n problems)
                (problem acc problems (format "invalid ~a: ~s" k v)))]
           [(or (string=? k "composer-max-rows") (string=? k "max-composer-rows"))
            (define n (as-int v))
            (if (and n (> n 0))
                (accept acc 'composer-max-rows n problems)
                (problem acc problems (format "invalid ~a: ~s" k v)))]
           [(string=? k "reasoning-visibility")
            (if (and (string? v) (member v '("session" "scrollback" "never")))
                (accept acc 'reasoning-visibility (string->symbol v) problems)
                (problem acc problems (format "invalid reasoning-visibility: ~s" v)))]
           [(string=? k "submit-key")
            (if (and (string? v) (non-empty-string? v))
                (accept acc
                        'submit-key
                        (string->symbol (if (string-ci=? v "enter")
                                            "return"
                                            (string-downcase v)))
                        problems)
                (problem acc problems (format "invalid submit-key: ~s" v)))]
           [(string=? k "newline-keys")
            (define ok?
              (and (list? v)
                   (andmap (lambda (x)
                             (and (string? x)
                                  (member (string-downcase x) '("shift" "control" "alt"))))
                           v)))
            (if ok?
                (accept acc
                        'newline-keys
                        (map (lambda (x) (string->symbol (string-downcase x))) v)
                        problems)
                (problem acc problems (format "invalid newline-keys: ~s" v)))]
           [(string=? k "keybindings")
            (define known
              '(ui.composer.submit ui.composer.insert-newline
                                   composer.history-up
                                   composer.history-down
                                   ui.transcript.toggle-detail))
            (if (not (hash? v))
                (problem acc problems (format "invalid keybindings (must be object): ~s" v))
                (let fold-b ([entries (hash->list v)]
                             [bacc (hasheq)]
                             [bproblems problems])
                  (cond
                    [(null? entries) (accept acc 'keybindings bacc bproblems)]
                    [else
                     (define ik (car (car entries)))
                     (define spec-val (cdr (car entries)))
                     (define intent (and (string? ik) (string->symbol (string-downcase ik))))
                     (define spec
                       (cond
                         [(string? spec-val) (keybinding-string->spec spec-val)]
                         [(list? spec-val)
                          (and (andmap string? spec-val)
                               (let ([specs (map keybinding-string->spec spec-val)])
                                 (and (andmap values specs) (apply append specs))))]
                         [else #f]))
                     (cond
                       [(not (memq intent known))
                        (fold-b (cdr entries)
                                bacc
                                (cons (format "unknown keybinding intent: ~s" ik) bproblems))]
                       [(not spec)
                        (fold-b (cdr entries)
                                bacc
                                (cons (format "invalid keybinding spec for ~s: ~s" ik spec-val)
                                      bproblems))]
                       [else (fold-b (cdr entries) (hash-set bacc intent spec) bproblems)])])))]
           [else (problem acc problems (format "unknown preference key: ~a" k))])))
     (values acc (reverse problems))]))

;; PURE: parsed config value -> ONE immutable snapshot merged over defaults.
(define (parse-preferences src)
  (define-values (overrides problems) (normalize-config-overrides src))
  (for ([msg (in-list problems)])
    (ui-diagnostic! 'preferences.invalid "config" msg))
  (merge-preferences (default-preferences) overrides))

;; Read the user config (first existing candidate, or `path` when given),
;; validate, and return ONE immutable snapshot.  Missing config -> defaults.
(define (load-preferences [path #f])
  (define src
    (if path
        (read-preferences-file path)
        (for/or ([p (in-list (preferences-config-paths))])
          (read-preferences-file p))))
  (parse-preferences src))

;; The ambient loaded snapshot.  Frontend bootstrap installs the loaded
;; snapshot once; leaf modules (layout, scrollback, gui-types, key
;; resolution) read it via `current-preferences`, with an explicit
;; `#:prefs` override where a caller already holds one.
(define current-preferences (make-parameter (default-preferences)))

;; Disclosure (W2) collapsed preview height, derived from preview-length
;; (~120 columns per line) - used by transcript rendering in both
;; frontends so the collapsed affordance is identical everywhere.
(define (collapsed-preview-lines [prefs (current-preferences)])
  (max 1 (quotient (preview-length prefs) 120)))

;; Reasoning persistence gate for scrollback artifact serialization:
;; only 'scrollback policy serializes full reasoning artifacts.
(define (preference-loads-artifact? [prefs (current-preferences)])
  (eq? (reasoning-visibility-policy prefs) 'scrollback))

(module+ test
  (require rackunit)
  (define p (default-preferences))
  ;; Submit / newline policy
  (check-equal? (resolve-key->intent 'return #:prefs p) 'ui.composer.submit)
  (check-equal? (resolve-key->intent 'return #:shift? #t #:prefs p) 'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent 'return #:control? #t #:prefs p) 'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent #\return #:shift? #t #:prefs p) 'ui.composer.insert-newline)
  (check-equal? (resolve-key->intent 'return #:alt? #t #:prefs p) #f)
  ;; Detail toggle
  (check-equal? (resolve-key->intent #\o #:control? #t #:prefs p) 'ui.transcript.toggle-detail)
  ;; History
  (check-equal? (resolve-key->intent 'up #:at-start? #t #:prefs p) 'composer.history-up)
  (check-equal? (resolve-key->intent 'down #:at-end? #t #:prefs p) 'composer.history-down)
  ;; Preference effects
  (check-equal? (reasoning-visibility-policy p) 'session)
  (check-false (reasoning-persist? p))
  (check-true (reasoning-persist? (set-preference p 'reasoning-visibility 'scrollback)))
  (check-equal? (max-composer-rows p) 6)
  (check-equal? (max-composer-rows (set-preference p 'composer-max-rows 12)) 12)
  (check-equal? (max-composer-rows (set-preference p 'composer-max-rows -3)) 6)
  (check-equal? (preview-length p) 400)
  ;; Custom keybinding overrides policy
  (define p2 (set-preference p 'keybindings (hasheq 'ui.composer.submit '(control return))))
  (check-equal? (resolve-key->intent 'return #:control? #t #:prefs p2) 'ui.composer.submit)
  (check-equal? (resolve-key->intent 'return #:prefs p2) 'ui.composer.submit)
  ;; Loader-normalized submit-key ('return, from JSON "enter") still submits
  (check-equal? (submit-key-policy (set-preference p 'submit-key 'return)) 'enter)
  (check-equal? (resolve-key->intent 'return #:prefs (set-preference p 'submit-key 'return))
                'ui.composer.submit)
  ;; merge
  (define p3 (merge-preferences p (hasheq 'composer-max-rows 3)))
  (check-equal? (max-composer-rows p3) 3)
  (check-equal? (reasoning-visibility-policy p3) 'session))
