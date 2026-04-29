#lang racket

;; scripts/arch-report.rkt — Architecture fitness report generator
;;
;; Generates a summary of architecture health:
;;   - Module sizes (lines of code)
;;   - Oversized modules (> 900 LOC threshold)
;;   - Layer boundary violations
;;   - STABILITY annotation coverage
;;   - Dependency policy compliance
;;
;; Usage:
;;   racket scripts/arch-report.rkt              # full report
;;   racket scripts/arch-report.rkt --ci         # CI mode: exit 1 on violations
;;   racket scripts/arch-report.rkt --json       # JSON output

(require racket/port
         racket/string
         racket/list
         racket/file
         racket/match
         json)

;; ── Configuration ──

(define MODULE-SIZE-THRESHOLD 900)
(define Q-DIR
  (simplify-path (if (getenv "Q_DIR")
                     (string->path (getenv "Q_DIR"))
                     ;; Script is in q/scripts/; go up two levels to project root, then into q/
                     (build-path (current-directory) ".." "q"))))

;; Known large modules that are exempt from size threshold
;; (tracked in docs/architecture/dependency-policy.rktd)
(define KNOWN-LARGE
  '("runtime/agent-session.rkt" "runtime/iteration.rkt"
                                "runtime/session-lifecycle.rkt"
                                "runtime/runtime-helpers.rkt"
                                "runtime/tool-coordinator.rkt"
                                "runtime/turn-orchestrator.rkt"
                                "runtime/package.rkt"
                                "runtime/extension-catalog.rkt"))

;; ── Helpers ──

(define (rkt-files-under dir)
  (define full-dir (build-path Q-DIR dir))
  (if (directory-exists? full-dir)
      (sort (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
                    (find-files (lambda (p) (file-exists? p)) full-dir))
            path<?)
      '()))

(define (source-file? abs-path)
  (define rel (relative-path abs-path))
  (not (or (string-prefix? rel "tests/")
           (string-prefix? rel "examples/")
           (string-prefix? rel "benchmarks/"))))

(define (all-rkt-files)
  (sort (filter (λ (f) (and (regexp-match? #rx"\\.rkt$" (path->string f)) (source-file? f)))
                (find-files (lambda (p) (file-exists? p)) Q-DIR))
        path<?))

(define (line-count filepath)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (length (string-split (file->string filepath) "\n"))))

(define (relative-path abs-path)
  (path->string (find-relative-path Q-DIR abs-path)))

(define (has-stability-annotation? filepath)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define src (file->string filepath))
    (or (regexp-match? #rx";;\\s*STABILITY:" src) (regexp-match? #rx"#lang typed/racket" src))))

(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define lines (string-split src "\n"))
    (define rest
      (string-join (if (string-prefix? (car lines) "#lang")
                       (cdr lines)
                       lines)
                   "\n"))
    (define forms (port->list read (open-input-string rest)))
    (append* (for/list ([form forms])
               (cond
                 [(and (pair? form) (eq? (car form) 'require)) (cdr form)]
                 [else '()])))))

(define (require-spec->paths spec)
  (cond
    [(string? spec) (list spec)]
    [(symbol? spec) '()]
    [(pair? spec)
     (case (car spec)
       [(only-in prefix-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (list (cadr spec))
            '())]
       [else (append* (map require-spec->paths (cdr spec)))])]
    [else '()]))

;; ── Layer definitions ──

(define LAYERS
  '(("foundation" . ("util/" "agent/types.rkt"))
    ("core" . ("llm/" "agent/event-bus.rkt" "agent/loop.rkt" "agent/queue.rkt"))
    ("tools" . ("tools/" "sandbox/"))
    ("runtime" . ("runtime/" "extensions/" "agent/iteration.rkt"))
    ("interfaces" . ("interfaces/" "cli/" "tui/"))))

(define (layer-of path)
  (for/first ([layer LAYERS]
              #:when (for/or ([prefix (cdr layer)])
                       (string-prefix? path prefix)))
    (car layer)))

;; For layer boundary checks: which layers can depend on which
;; foundation → nothing below
;; core → foundation
;; tools → foundation, core
;; runtime → foundation, core, tools
;; interfaces → all
(define ALLOWED-DEPS
  '(("foundation" . ()) ("core" . ("foundation"))
                        ("tools" . ("foundation" "core"))
                        ("runtime" . ("foundation" "core" "tools"))
                        ("interfaces" . ("foundation" "core" "tools" "runtime"))))

;; ── Report generation ──

(define (generate-report)
  (define files (all-rkt-files))
  (define results (make-hash))

  ;; Module sizes
  (define sizes
    (for/list ([f files])
      (define rel (relative-path f))
      (define lc (line-count f))
      (list rel lc)))

  ;; Oversized modules
  (define oversized
    (filter (λ (s) (and (> (second s) MODULE-SIZE-THRESHOLD) (not (member (first s) KNOWN-LARGE))))
            sizes))

  ;; STABILITY annotations
  (define unannotated (filter (λ (f) (not (has-stability-annotation? f))) files))

  ;; Layer boundary violations
  (define violations
    (for*/list ([f files]
                [req (extract-requires f)]
                [path (require-spec->paths req)]
                #:when (string-prefix? path ".."))
      (define rel (relative-path f))
      (define source-layer (layer-of rel))
      (define target-rel (path->string (simplify-path (build-path (path-only rel) path))))
      (define target-layer (layer-of target-rel))
      (define allowed (and source-layer target-layer (cdr (assoc source-layer ALLOWED-DEPS))))
      (if (and source-layer
               target-layer
               (not (equal? source-layer target-layer))
               (not (member target-layer (or allowed '()))))
          (list rel path source-layer target-layer)
          #f)))

  (hash 'total-modules
        (length files)
        'total-loc
        (apply + (map second sizes))
        'oversized-modules
        (length oversized)
        'oversized-details
        oversized
        'unannotated-modules
        (length unannotated)
        'unannotated-details
        (map relative-path unannotated)
        'boundary-violations
        (length (filter values violations))
        'violation-details
        (filter values violations)))

(define (path-only p)
  (define parts (string-split p "/"))
  (string-join (drop-right parts 1) "/"))

(define (print-report report)
  (printf "=== Architecture Fitness Report ===\n\n")
  (printf "Total modules: ~a\n" (hash-ref report 'total-modules))
  (printf "Total LOC: ~a\n" (hash-ref report 'total-loc))
  (printf "Oversized modules (>~a LOC): ~a\n"
          MODULE-SIZE-THRESHOLD
          (hash-ref report 'oversized-modules))
  (for ([d (hash-ref report 'oversized-details)])
    (printf "  ⚠ ~a (~a LOC)\n" (first d) (second d)))
  (printf "Unannotated modules: ~a\n" (hash-ref report 'unannotated-modules))
  (for ([d (hash-ref report 'unannotated-details)])
    (printf "  ⚠ ~a\n" d))
  (printf "Boundary violations: ~a\n" (hash-ref report 'boundary-violations))
  (for ([d (hash-ref report 'violation-details)])
    (printf "  ✗ ~a → ~a (~a → ~a)\n" (first d) (second d) (third d) (fourth d))))

(define (has-issues? report)
  (or (> (hash-ref report 'oversized-modules) 0) (> (hash-ref report 'boundary-violations) 0)))

;; ── Main ──

(define ci-mode? (member "--ci" (vector->list (current-command-line-arguments))))
(define json-mode? (member "--json" (vector->list (current-command-line-arguments))))

(define report (generate-report))

(cond
  [json-mode? (write-json report)]
  [else (print-report report)])

(when ci-mode?
  (when (has-issues? report)
    (printf "\n❌ Architecture gate FAILED\n")
    (exit 1))
  (printf "\n✅ Architecture gate PASSED\n"))
