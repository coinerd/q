#lang racket

;; @speed fast  ;; @suite arch

;; BOUNDARY: integration

;; tests/test-extension-exception-fitness.rkt — Extension exception fitness
;;
;; v0.99.87 W1 (roadmap §W1): the Extension exceptions are verified
;; against real import targets, metadata, and expiry:
;;   1. No stale exceptions (source file still exists)
;;   2. No ghost destinations (each declared destination is still imported)
;;   3. Complete metadata (owner, rationale, lifecycle)
;;   4. No expired exceptions (revisit-by in the future)
;;   5. Pair-precise exemptions — destinations declared; the source crosses
;;      its boundary exactly at the declared destinations (no blanket waiver)
;; @boundary unit
;;   6. Runtime and TUI exceptions are reported separately
;;
;; Positive/negative probes verify the checker against deliberately
;; malformed entries and against a valid entry.
;;
;; Data source: docs/architecture/dependency-policy.rktd
;; Refs: ARCH-FITNESS / F04

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         racket/date
         "helpers/arch-utils.rkt")

(define policy-path (build-path q-dir "docs" "architecture" "dependency-policy.rktd"))
(define policy (call-with-input-file policy-path read))

(define (policy-ref section . keys)
  (let loop ([data (cdr (assoc section policy))]
             [ks keys])
    (if (null? ks)
        data
        (loop (cdr (assoc (car ks) data)) (cdr ks)))))

(define extension-exceptions (policy-ref 'known-exceptions 'extensions))

(define (entry-name entry)
  (if (symbol? (car entry))
      (symbol->string (car entry))
      (car entry)))

(define (entry-fields entry)
  (cdr entry))

(define (field fields
               key)
  (and (assoc key fields) (cdr (assoc key fields))))

(define today-str
  (parameterize ([date-display-format 'iso-8601])
    (date->string (seconds->date (current-seconds)))))

;; Normalize a require path like "../tui/component.rkt" to "tui/component.rkt".
(define (normalize-import-path p)
  (let loop ([s p])
    (cond
      [(string-prefix? s "../") (loop (substring s 3))]
      [(string-prefix? s "./") (loop (substring s 2))]
      [else s])))

(define (source-imports name)
  (define path (build-path q-dir "extensions" name))
  (if (file-exists? path)
      (map normalize-import-path (append* (map require-spec->paths (extract-requires path))))
      '()))

(define (boundary-prefix boundary)
  (case boundary
    [(tui) "tui/"]
    [(runtime) "runtime/"]
    [(ui) "ui-core/"]
    [else #f]))

;; Pure checker: returns a list of problem strings for one exception entry
;; given its source file's normalized import paths. Returns '() when fit.
(define (extension-exception-problems name fields source-imports*)
  (define problems '())
  (define (problem! msg)
    (set! problems (cons msg problems)))
  (define rationale
    (field fields
           'rationale))
  (define owner
    (field fields
           'owner))
  (define revisit
    (field fields
           'revisit-by))
  (define permanent
    (field fields
           'permanent-waiver))
  (define justification
    (field fields
           'waiver-justification))
  (define boundary
    (field fields
           'boundary))
  (define destinations
    (field fields
           'destinations))

  ;; Metadata completeness.
  (unless (and rationale (string? rationale) (positive? (string-length rationale)))
    (problem! (format "~a: missing or empty rationale" name)))
  (unless (and owner (string? owner) (positive? (string-length owner)))
    (problem! (format "~a: missing or empty owner" name)))
  (cond
    [(and revisit (not (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" revisit)))
     (problem! (format "~a: malformed revisit-by ~a" name revisit))]
    [(and revisit permanent)
     (problem! (format "~a: both revisit-by and permanent-waiver present" name))]
    [(and (not revisit) (not permanent))
     (problem! (format "~a: missing lifecycle (revisit-by or permanent-waiver)" name))]
    [(and permanent (not (eq? permanent #t)))
     (problem! (format "~a: permanent-waiver must be #t" name))]
    [(and permanent
          (not (and justification (string? justification) (positive? (string-length justification)))))
     (problem! (format "~a: permanent waiver lacks justification" name))])

  ;; Expiry.
  (when (and revisit (string<? revisit today-str))
    (problem! (format "~a: revisit-by ~a expired" name revisit)))

  ;; Pair precision: destinations must be declared and non-empty.
  (unless (and destinations (pair? destinations))
    (problem! (format "~a: missing destinations (blanket file waiver)" name)))
  ;; Boundary must classify the crossing.
  (unless (memq boundary '(tui runtime ui))
    (problem! (format "~a: boundary must be tui|runtime|ui, got ~a" name boundary)))

  ;; Ghost destinations and overbreadth.
  (when (and destinations (pair? destinations))
    (for ([dest (in-list destinations)])
      (unless (member dest source-imports*)
        (problem! (format "~a: declared destination ~a no longer imported" name dest))))
    (define prefix (boundary-prefix boundary))
    (when prefix
      (for ([imp (in-list source-imports*)]
            #:when (string-prefix? imp prefix)
            #:unless (member imp destinations))
        (problem! (format "~a: imports ~a beyond declared destinations" name imp)))))
  ;; tui/ is the only hard-forbidden layer for extensions: a non-tui-boundary
  ;; exception must not import tui/ at all, and a tui-boundary exception must
  ;; cover every tui/ import via destinations (checked above).
  (unless (eq? boundary 'tui)
    (for ([imp (in-list source-imports*)]
          #:when (string-prefix? imp "tui/"))
      (problem! (format "~a: imports ~a despite non-tui boundary" name imp))))

  (reverse problems))

;; ============================================================
;; Fitness tests
;; ============================================================

(define fitness-tests
  (test-suite "extension-exception-fitness"

    (test-case "Every extension exception source file exists (no stale)"
      (for ([entry (in-list extension-exceptions)])
        (define name (entry-name entry))
        (check-true (file-exists? (build-path q-dir "extensions" name))
                    (format "extensions/~a no longer exists — retire the exception" name))))

    (test-case "Every extension exception is metadata-complete and non-expired"
      (for ([entry (in-list extension-exceptions)])
        (define problems
          (extension-exception-problems (entry-name entry)
                                        (entry-fields entry)
                                        (source-imports (entry-name entry))))
        (check-equal? problems '() (format "Extension exception fitness problems: ~a" problems))))

    (test-case "Extension exception set is stable (count and membership)"
      (check-equal?
       (length extension-exceptions)
       3
       "Extensions known-exceptions must remain at 3 (v0.99.88 W4: dialog-api/ui-surface/widget-lifecycle permanent pair waivers)")
      (check-equal? (sort (map entry-name extension-exceptions) string<?)
                    '("dialog-api.rkt" "ui-surface.rkt" "widget-lifecycle.rkt")
                    "Extension exception membership must match v0.99.88 W4 baseline"))

    (test-case "Runtime and TUI exceptions are reported separately"
      (define runtime-entries
        (filter (lambda (e)
                  (eq? (field (entry-fields e)
                              'boundary)
                       'runtime))
                extension-exceptions))
      (define tui-entries
        (filter (lambda (e)
                  (eq? (field (entry-fields e)
                              'boundary)
                       'tui))
                extension-exceptions))
      (define ui-entries
        (filter (lambda (e)
                  (eq? (field (entry-fields e)
                              'boundary)
                       'ui))
                extension-exceptions))
      ;; Runtime-boundary exceptions (fragile runtime service coupling).
      ;; v0.99.88 W2: context.rkt removed — provider registry is injected as a
      ;; neutral host capability. v0.99.88 W3: ext-package-manager.rkt removed
      ;; — package lifecycle is injected as a neutral package-host-service
      ;; (MA-04 closed). Zero runtime-boundary extension exceptions remain.
      (check-equal? (sort (map entry-name runtime-entries) string<?)
                    '()
                    "Runtime-boundary extension exceptions")
      ;; TUI-boundary exceptions (direct tui/ import).
      (check-equal? (sort (map entry-name tui-entries) string<?)
                    '("widget-lifecycle.rkt")
                    "TUI-boundary extension exceptions")
      ;; UI-boundary exceptions (shared ui-core protocol layer).
      (check-equal? (sort (map entry-name ui-entries) string<?)
                    '("dialog-api.rkt" "ui-surface.rkt")
                    "UI-boundary extension exceptions")
      ;; Every exception has exactly one boundary classification.
      (check-equal? (+ (length runtime-entries) (length tui-entries) (length ui-entries))
                    (length extension-exceptions)
                    "All extension exceptions must be boundary-classified"))

    (test-case "All extension exceptions are evidence-backed permanent pair waivers (v0.99.88 W4)"
      ;; v0.99.88 W4 decision: dialog-api / ui-surface / widget-lifecycle are
      ;; intentional UI/TUI bridges — no neutral UI protocol exists (building
      ;; one would be an abstract UI framework, which the roadmap prohibits).
      ;; Each keeps boundary + destinations (pair-precise) and becomes a
      ;; permanent waiver with a non-empty consumer-evidence justification.
      ;; None may be dated/expired (acceptance: no expired exception).
      (for ([entry (in-list extension-exceptions)])
        (define fields (entry-fields entry))
        (define permanent
          (field fields
                 'permanent-waiver))
        (define justification
          (field fields
                 'waiver-justification))
        (define revisit
          (field fields
                 'revisit-by))
        (check-true (eq? permanent #t) (format "~a: not a permanent waiver" (entry-name entry)))
        (check-false revisit (format "~a: must not carry a revisit-by date" (entry-name entry)))
        (check-true (and (string? justification) (positive? (string-length justification)))
                    (format "~a: permanent waiver lacks evidence-backed justification"
                            (entry-name entry)))
        ;; MA-05 closure: each carries pair-precise destinations + boundary.
        (check-true (and (field fields
                                'destinations)
                         (pair? (field fields
                                       'destinations)))
                    (format "~a: permanent waiver must remain pair-precise" (entry-name entry)))
        (check-true (and (memq (field fields
                                      'boundary)
                               '(tui runtime ui))
                         #t)
                    (format "~a: permanent waiver must classify its boundary" (entry-name entry)))))

    ;; ── Negative probes: deliberately malformed entries must be flagged ──
    (test-case "Negative probe: undeclared boundary import is flagged"
      (define problems
        (extension-exception-problems
         "widget-lifecycle.rkt"
         '((rationale . "Imports tui/component.rkt") (owner . "extensions")
                                                     (revisit-by . "2026-10-01")
                                                     (boundary . tui)
                                                     (destinations . ("tui/component.rkt")))
         '("tui/component.rkt" "tui/extra.rkt")))
      (check-true (ormap (lambda (p) (string-contains? p "extra.rkt")) problems)
                  "Overbreadth import must be reported"))

    (test-case "Negative probe: ghost destination is flagged"
      (define problems
        (extension-exception-problems
         "context.rkt"
         '((rationale . "imports runtime/provider") (owner . "extensions")
                                                    (revisit-by . "2026-10-01")
                                                    (boundary . runtime)
                                                    (destinations . ("runtime/gone.rkt")))
         '("runtime/provider/provider-registry.rkt")))
      (check-true (ormap (lambda (p) (string-contains? p "no longer imported")) problems)
                  "Stale destination must be reported"))

    (test-case "Negative probe: tui import on non-tui-boundary exception is flagged"
      (define problems
        (extension-exception-problems "context.rkt"
                                      '((rationale . "imports runtime/provider")
                                        (owner . "extensions")
                                        (revisit-by . "2026-10-01")
                                        (boundary . runtime)
                                        (destinations . ("runtime/provider/provider-registry.rkt")))
                                      '("runtime/provider/provider-registry.rkt"
                                        "tui/component.rkt")))
      (check-true (ormap (lambda (p) (string-contains? p "non-tui boundary")) problems)
                  "tui/ import on non-tui exception must be reported"))

    (test-case "Negative probe: expired exception is flagged"
      (define problems
        (extension-exception-problems "widget-lifecycle.rkt"
                                      '((rationale . "r") (owner . "extensions")
                                                          (revisit-by . "2020-01-01")
                                                          (boundary . tui)
                                                          (destinations . ("tui/component.rkt")))
                                      '("tui/component.rkt")))
      (check-true (ormap (lambda (p) (string-contains? p "expired")) problems)
                  "Expired exception must be reported"))

    (test-case "Negative probe: missing destinations (blanket waiver) is flagged"
      (define problems
        (extension-exception-problems
         "widget-lifecycle.rkt"
         '((rationale . "r") (owner . "extensions") (revisit-by . "2026-10-01") (boundary . tui))
         '("tui/component.rkt")))
      (check-true (ormap (lambda (p) (string-contains? p "blanket file waiver")) problems)
                  "Blanket waiver without destinations must be reported"))

    ;; ── Positive probe: a valid exception must pass ──
    (test-case "Positive probe: valid pair-precise dated exception passes"
      (define problems
        (extension-exception-problems
         "widget-lifecycle.rkt"
         '((rationale . "Imports tui/component.rkt for q-component? bridge")
           (owner . "extensions")
           (revisit-by . "2099-01-01")
           (boundary . tui)
           (destinations . ("tui/component.rkt")))
         '("tui/component.rkt")))
      (check-equal? problems '() "Valid dated exception must produce no problems"))

    (test-case "Positive probe: valid permanent pair waiver passes"
      (define problems
        (extension-exception-problems
         "dialog-api.rkt"
         '((rationale . "UI dialog primitives bridge to shared ui-core protocol layer")
           (owner . "tui")
           (permanent-waiver . #t)
           (waiver-justification . "Intentional UI bridge with consumer evidence")
           (boundary . ui)
           (destinations . ("ui-core/ui-state-protocol.rkt")))
         '("ui-core/ui-state-protocol.rkt")))
      (check-equal? problems '() "Valid permanent pair waiver must produce no problems"))))

(module+ test
  (run-tests fitness-tests))
