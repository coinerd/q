#lang racket/base

;; q/tests/test-preferences-loader.rkt — W3 (v1.00.02) preference loader tests.
;;
;; Covers the W3 Done criteria that are not already covered by the
;; in-module suite (ui-core/preferences.rkt) or the parity suite
;; @speed fast  ;; @suite default
;; @boundary integration
;; (ux-gui-tui-parity-test.rkt):
;;
;;   1. Loader: valid config file -> merged snapshot (every schema key)
;;   2. Loader: invalid values fall back to defaults (snapshot stays valid)
;;   3. Loader: missing config file -> defaults, no error
;;   4. Loader: config discovery honors Q_PREFERENCES_FILE
;;   5. Snapshot: immutable, serializable via preferences->hash
;;   6. Policy: scrollback serialization honors session/scrollback/never
;;   7. Parity: one loaded snapshot yields identical key semantics in the
;;      TUI (resolve-key->intent) and GUI (gui-key->intent wrapper)

(require rackunit
         json
         racket/file
         racket/hash
         racket/list
         racket/system
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/preferences.rkt"
         "../ui-core/ui-intents.rkt"
         "../gui/components/input-helpers.rkt"
         "../tui/scrollback.rkt"
         "../tui/state-types.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (with-config-dir thunk)
  (define tmp-dir (make-temporary-file "prefs-test-~a" 'directory))
  (dynamic-wind void (lambda () (thunk tmp-dir)) (lambda () (delete-directory/files tmp-dir))))

(define (write-config! path jsexpr)
  (call-with-output-file path (lambda (out) (write-json jsexpr out)) #:exists 'replace))

(define (thinking-entry text)
  (transcript-entry 'thinking
                    text
                    12345.0
                    (hasheq 'artifact
                            (make-conversation-artifact #:id "a1"
                                                        #:turn-id "t1"
                                                        #:session-id "s1"
                                                        #:kind 'thinking
                                                        #:body text))
                    1))

;; ---------------------------------------------------------------------------
;; 1–4: the configuration loader
;; ---------------------------------------------------------------------------

(define preferences-loader-tests
  (test-suite "preferences-loader (W3)"

    (test-case "valid config: every schema key is merged over defaults"
      (with-config-dir
       (lambda (dir)
         (define path (build-path dir "preferences.json"))
         (write-config! path
                        (hasheq 'reasoning-visibility
                                "scrollback"
                                'disclosure-preview-length
                                240
                                'max-composer-rows
                                10
                                'submit-key
                                "return"
                                'newline-keys
                                '("shift" "alt")
                                'keybindings
                                (hasheq 'ui.transcript.toggle-detail "ctrl+t")))
         (define p (load-preferences path))
         (check-equal? (reasoning-visibility-policy p) 'scrollback)
         (check-true (reasoning-persist? p))
         (check-equal? (preview-length p) 240)
         (check-equal? (max-composer-rows p) 10)
         ;; Canonical submit policy is 'enter (both spellings accepted).
         (check-equal? (submit-key-policy p) 'enter)
         (check-not-false (memq 'shift (newline-key-policies p)))
         (check-not-false (memq 'alt (newline-key-policies p)))
         (check-equal? (hash-ref (preferences-ref p 'keybindings) 'ui.transcript.toggle-detail #f)
                       '(control t)))))

    (test-case "valid config: legacy JSON key aliases are accepted"
      (with-config-dir (lambda (dir)
                         (define path (build-path dir "preferences.json"))
                         (write-config! path (hasheq 'preview-length 100 'composer-max-rows 3))
                         (define p (load-preferences path))
                         (check-equal? (preview-length p) 100)
                         (check-equal? (max-composer-rows p) 3))))

    (test-case "invalid config: every invalid value falls back to defaults"
      (define d (default-preferences))
      (with-config-dir
       (lambda (dir)
         (define path (build-path dir "preferences.json"))
         (write-config! path
                        (hasheq 'reasoning-visibility
                                "sometimes" ; not in the enum
                                'disclosure-preview-length
                                -5 ; negative
                                'max-composer-rows
                                0 ; not positive
                                'submit-key
                                "" ; empty
                                'newline-keys
                                '("meta") ; unsupported modifier
                                'keybindings
                                (hasheq 'unknown.intent "ctrl+k")
                                'totally-unknown-key
                                42))
         (define p (load-preferences path))
         ;; The snapshot is still a valid, complete preference set: every
         ;; invalid value silently degrades to its default.
         (check-equal? (reasoning-visibility-policy p) (reasoning-visibility-policy d))
         (check-equal? (preview-length p) (preview-length d))
         (check-equal? (max-composer-rows p) (max-composer-rows d))
         (check-equal? (submit-key-policy p) (submit-key-policy d))
         (check-equal? (newline-key-policies p) (newline-key-policies d))
         (check-equal? (preferences-ref p 'keybindings) (preferences-ref d 'keybindings))
         ;; And it still resolves keys with default semantics.
         (check-equal? (resolve-key->intent 'return #:prefs p) 'ui.composer.submit))))

    (test-case "invalid config: unparseable JSON yields defaults, not an error"
      (with-config-dir
       (lambda (dir)
         (define path (build-path dir "preferences.json"))
         (call-with-output-file path
                                (lambda (out) (display "{\"reasoning-visibility\": oops" out))
                                #:exists 'replace)
         (define p (load-preferences path))
         (check-equal? (reasoning-visibility-policy p) 'session)
         (check-equal? (max-composer-rows p) (max-composer-rows (default-preferences))))))

    (test-case "missing config: nonexistent file yields pure defaults"
      (with-config-dir (lambda (dir)
                         (define p (load-preferences (build-path dir "nope.json")))
                         (check-equal? (reasoning-visibility-policy p) 'session)
                         (check-equal? (preview-length p) 400)
                         (check-equal? (max-composer-rows p) 6)
                         (check-false (reasoning-persist? p)))))

    (test-case "discovery: Q_PREFERENCES_FILE points at the config"
      (with-config-dir (lambda (dir)
                         (define path (build-path dir "preferences.json"))
                         (write-config! path (hasheq 'max-composer-rows 4))
                         (define env (environment-variables-copy (current-environment-variables)))
                         (environment-variables-set! env
                                                     #"Q_PREFERENCES_FILE"
                                                     (string->bytes/utf-8 (path->string path)))
                         (parameterize ([current-environment-variables env])
                           (define p (load-preferences))
                           (check-equal? (max-composer-rows p) 4)))))))

;; ---------------------------------------------------------------------------
;; 5: snapshot properties
;; ---------------------------------------------------------------------------

(define preferences-snapshot-tests
  (test-suite "preferences-snapshot (W3)"

    (test-case "snapshot is immutable"
      (define p (load-preferences #f))
      (check-exn exn:fail:contract? (lambda () (hash-set! (preferences->hash p) 'x 1))))

    (test-case "preferences->hash serializes every schema field"
      (define p (load-preferences #f))
      (define h (preferences->hash p))
      (check-true (hash? h))
      (for ([k '(reasoning-visibility preview-length
                                      composer-max-rows
                                      submit-key
                                      newline-keys
                                      keybindings)])
        (check-true (hash-has-key? h k) (format "preferences->hash missing ~a" k))))
    (test-case "merge-preferences: unknown overrides are ignored"
      (define p (merge-preferences (default-preferences) (hasheq 'no-such-field 9)))
      (check-equal? (max-composer-rows p) 6))))

;; ---------------------------------------------------------------------------
;; 6: policy-gated scrollback serialization (session / scrollback / never)
;; ---------------------------------------------------------------------------

(define preferences-policy-tests
  (test-suite "preferences-policy: scrollback honors reasoning persistence policy (W3)"

    (test-case "default (session): reasoning never reaches the jsexpr"
      (parameterize ([current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'session)])
        (define entry (thinking-entry "secret chain of thought"))
        (define j (transcript-entry->jsexpr entry))
        ;; The artifact is stripped from the serialized meta...
        (check-false (hash-ref (hash-ref j 'meta) 'artifact #f))
        ;; ...and the text is replaced by the session marker, not the body.
        (check-equal? (hash-ref j 'text)
                      "[reasoning not persisted (reasoning-visibility: session)]")))

    (test-case "never: reasoning bodies are stripped even from legacy text"
      (parameterize ([current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'never)])
        (define j (transcript-entry->jsexpr (thinking-entry "secret chain of thought")))
        (check-false (hash-ref (hash-ref j 'meta) 'artifact #f))
        (check-equal? (hash-ref j 'text) "[reasoning stripped (reasoning-visibility: never)]")))

    (test-case "scrollback: full artifacts serialize and roundtrip"
      (parameterize ([current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'scrollback)])
        (define j (transcript-entry->jsexpr (thinking-entry "chain of thought")))
        (define restored (jsexpr->transcript-entry j))
        (check-equal? (transcript-entry-kind restored) 'thinking)
        (check-equal? (transcript-entry-text restored) "chain of thought")
        (check-true (conversation-artifact?
                     (hash-ref (transcript-entry-meta restored) 'artifact #f)))))

    (test-case "policy gate: only 'scrollback persists reasoning"
      (for ([policy '(session scrollback never)])
        (define p (set-preference (default-preferences) 'reasoning-visibility policy))
        (check-equal? (preference-loads-artifact? p)
                      (eq? policy 'scrollback)
                      (format "policy ~a" policy))))

    (test-case "non-reasoning artifacts are NOT policy-gated"
      (for ([policy '(session never)])
        (parameterize ([current-preferences
                        (set-preference (default-preferences) 'reasoning-visibility policy)])
          (define entry
            (transcript-entry 'tool-end
                              "done"
                              1.0
                              (hasheq 'artifact
                                      (make-conversation-artifact #:id "t2"
                                                                  #:turn-id "t1"
                                                                  #:session-id "s1"
                                                                  #:kind 'tool-end
                                                                  #:body "done"))
                              2))
          (define j (transcript-entry->jsexpr entry))
          (check-equal? (hash-ref j 'text) "done" (format "policy ~a" policy))
          (check-not-false (hash-ref (hash-ref j 'meta) 'artifact #f)
                           (format "policy ~a" policy)))))))

;; ---------------------------------------------------------------------------
;; 7: one snapshot, identical key semantics in both frontends
;; ---------------------------------------------------------------------------

(define preferences-key-parity-tests
  (test-suite "preferences-key-parity (W3)"

    (test-case "one loaded snapshot drives identical TUI and GUI key semantics"
      (with-config-dir
       (lambda (dir)
         (define path (build-path dir "preferences.json"))
         ;; Remap submit to ctrl+return and detail-toggle to ctrl+t so the
         ;; parity check exercises CONFIGURED bindings, not just defaults.
         (write-config! path
                        (hasheq 'submit-key
                                "return"
                                'newline-keys
                                '("shift")
                                'keybindings
                                (hasheq 'ui.transcript.toggle-detail "ctrl+t")))
         (define p (load-preferences path))
         ;; resolve-key->intent returns intent SYMBOLS; gui-key->intent wraps
         ;; the SAME resolver and lifts symbols into intent STRUCTS.  Parity =
         ;; identical kinds for identical physical keys.
         (define (same? key #:s [s #f] #:c [c #f] #:a [a #f])
           (define tui (resolve-key->intent key #:shift? s #:control? c #:alt? a #:prefs p))
           (define gui (gui-key->intent key #:shift? s #:control? c #:alt? a #:prefs p))
           (check-equal? (and gui (ui-intent-kind gui))
                         tui
                         (format "key ~a s=~a c=~a a=~a" key s c a)))
         (parameterize ([current-preferences p])
           (same? 'return)
           (same? 'return #:s #t)
           (same? #\o #:c #t)
           (same? #\t #:c #t)
           (same? 'up)
           (same? 'down)
           ;; and through the GUI ambient parameter path as well
           (check-equal? (ui-intent-kind (gui-key->intent #\t #:control? #t))
                         'ui.transcript.toggle-detail)))))))

(module+ main
  (require rackunit/text-ui)
  (void (run-tests preferences-loader-tests)
        (run-tests preferences-snapshot-tests)
        (run-tests preferences-policy-tests)
        (run-tests preferences-key-parity-tests)))

(module+ test
  (require (submod ".." main)))
