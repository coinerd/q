#lang racket

;; scripts/audit-project.rkt — Reproducible project audit (RA-4, v0.24.9)
;;
;; Automated module inventory, risky API usage scan, TODO/deprecation check.
;; Output: timestamped markdown report to docs/audits/.
;; CI mode: --ci exits non-zero on critical findings.
;;
;; Usage:
;;   racket scripts/audit-project.rkt              # full report
;;   racket scripts/audit-project.rkt --ci         # CI mode: exit 1 on critical
;;   racket scripts/audit-project.rkt --stdout     # print to stdout
;;   racket scripts/audit-project.rkt --json      # JSON output

(require racket/port
         racket/string
         racket/list
         racket/file
         racket/match
         racket/date
         racket/path
         json)

;; ── Configuration ──

(define Q-DIR
  (simplify-path (let ([try (current-directory)])
                   (if (directory-exists? (build-path try "runtime"))
                       try
                       (build-path try "q")))))

(define AUDITS-DIR (build-path Q-DIR "docs" "audits"))
(define SIZE-THRESHOLD 800)

;; ── Helpers ──

(define SKIP-DIRS '("compiled" ".git" "node_modules"))

(define (in-skip-dir? p skip-list)
  (define rel (path->string (find-relative-path Q-DIR p)))
  (for/or ([d (in-list skip-list)])
    (string-contains? rel (string-append d "/"))))

(define (find-rkt-files #:skip-dirs [skip-dirs SKIP-DIRS])
  (define raw (find-files (lambda (p) #t) Q-DIR))
  (filter (lambda (f)
            (and (file-exists? f)
                 (let ([ext (path-get-extension f)])
                   (and ext (member (bytes->string/utf-8 ext) '(".rkt" ".rktl"))))
                 (not (in-skip-dir? f skip-dirs))))
          raw))

;; ── Finding data ──

(struct finding (severity category file line message) #:transparent)

(define (finding->md f)
  (match-define (finding sev cat file line msg) f)
  (format "- **[~a]** ~a: `~a` (line ~a): ~a" sev cat (find-relative-path Q-DIR file) line msg))

;; ── Scanners ──

;; Scanner 1: Module inventory (informational)
(define (scan-module-inventory)
  (define rkt-files (find-rkt-files))
  (define total-modules (length rkt-files))
  (define total-lines
    (for/sum ([f (in-list rkt-files)]) (length (string-split (file->string f) "\n"))))
  (define by-layer
    (for/fold ([acc (hash)]) ([f (in-list rkt-files)])
      (define rel (path->string (find-relative-path Q-DIR f)))
      (define layer
        (cond
          [(string-prefix? rel "runtime/") "runtime"]
          [(string-prefix? rel "agent/") "agent"]
          [(string-prefix? rel "llm/") "llm"]
          [(string-prefix? rel "tools/") "tools"]
          [(string-prefix? rel "tui/") "tui"]
          [(string-prefix? rel "interfaces/") "interfaces"]
          [(string-prefix? rel "extensions/") "extensions"]
          [(string-prefix? rel "sandbox/") "sandbox"]
          [(string-prefix? rel "util/") "util"]
          [(string-prefix? rel "cli/") "cli"]
          [(string-prefix? rel "tests/") "tests"]
          [(string-prefix? rel "scripts/") "scripts"]
          [else "other"]))
      (hash-update acc layer add1 0)))
  (hasheq 'total-modules total-modules 'total-lines total-lines 'by-layer by-layer))

;; Scanner 2: Risky API usage
(define RISKY-PATTERNS
  (list (list #rx"\\(eval\\b" "eval usage — potential code injection risk")
        (list #rx"\\(dynamic-require\\b" "dynamic-require — consider static require")
        (list #rx"\\(system\\b" "raw system call — prefer subprocess sandbox")
        (list #rx"\\(process\\b" "raw process call — verify resource limits")
        (list #rx"\\(set!\\b" "mutation — prefer functional style")
        (list #rx"\\(with-handlers.*#f" "handler returning #f may swallow errors")
        (list #rx"\\(system\\*\\b" "system* — verify process spawning is in sandbox/")
        (list #rx"\\(eval-syntax\\b" "eval-syntax — consider compile-time alternatives")
        (list #rx"\\(open-input-file\\b" "open-input-file — ensure file access is in tools/ layer")
        (list #rx"\\(call-with-input-file\\b"
              "call-with-input-file — ensure file access is in tools/ layer")))

(define (scan-risky-api)
  (define rkt-files
    (find-rkt-files #:skip-dirs '("compiled" ".git" "node_modules" "tests" "scripts")))
  (apply append
         (for/list ([f (in-list rkt-files)])
           (define lines (string-split (file->string f) "\n"))
           (apply append
                  (for/list ([line-text (in-list lines)]
                             [line-num (in-naturals 1)]
                             #:when #t
                             [pattern (in-list RISKY-PATTERNS)])
                    (match-define (list rx msg) pattern)
                    (if (regexp-match? rx line-text)
                        (list (finding 'warning "risky-api" f line-num msg))
                        '()))))))

;; Scanner 3: TODO/deprecation check
(define TODO-PATTERNS
  (list (list #rx";;.*TODO.*remove" "TODO with 'remove' — track for cleanup")
        (list #rx";;.*TODO.*deprecat" "TODO with 'deprecate' — track for cleanup")
        (list #rx";;.*TODO.*v0\\." "TODO with version gate — verify target milestone")
        (list #rx";;.*FIXME" "FIXME marker — needs resolution")
        (list #rx";;.*HACK" "HACK marker — needs proper fix")
        (list #rx";;.*XXX" "XXX marker — needs review")))

(define (scan-todos)
  (define rkt-files (find-rkt-files))
  (apply
   append
   (for/list ([f (in-list rkt-files)])
     (define lines (string-split (file->string f) "\n"))
     (apply
      append
      (for/list ([line-text (in-list lines)]
                 [line-num (in-naturals 1)]
                 #:when #t
                 [pattern (in-list TODO-PATTERNS)])
        (match-define (list rx msg) pattern)
        (if (regexp-match? rx line-text)
            (list
             (finding 'info "todo-marker" f line-num (format "~a: ~a" msg (string-trim line-text))))
            '()))))))

;; Scanner 4: Oversized modules
(define (scan-oversized)
  (define rkt-files
    (find-rkt-files #:skip-dirs '("compiled" ".git" "node_modules" "tests" "scripts" "benchmark")))
  (filter identity
          (for/list ([f (in-list rkt-files)])
            (define line-count (length (string-split (file->string f) "\n")))
            (if (> line-count SIZE-THRESHOLD)
                (finding 'warning
                         "oversized"
                         f
                         line-count
                         (format "~a lines (threshold: ~a)" line-count SIZE-THRESHOLD))
                #f))))

;; Scanner 5: High struct density
(define STRUCT-THRESHOLD 15)

(define (scan-struct-density)
  (define rkt-files
    (find-rkt-files #:skip-dirs '("compiled" ".git" "node_modules" "tests" "scripts")))
  (filter
   identity
   (for/list ([f (in-list rkt-files)])
     (define content (file->string f))
     (define struct-count (length (regexp-match* #rx"[(]struct " content)))
     (if (> struct-count STRUCT-THRESHOLD)
         (finding 'warning
                  "high-struct-density"
                  f
                  struct-count
                  (format "~a struct definitions (threshold: ~a)" struct-count STRUCT-THRESHOLD))
         #f))))

;; ── Report generation ──

(define (generate-report)
  (define inv (scan-module-inventory))
  (define risky (scan-risky-api))
  (define todos (scan-todos))
  (define oversized (scan-oversized))
  (define struct-dens (scan-struct-density))
  (define all-findings (append risky todos oversized struct-dens))

  (define ts
    (parameterize ([date-display-format 'iso-8601])
      (date->string (current-date) #t)))

  (define critical-count (count (lambda (f) (eq? (finding-severity f) 'critical)) all-findings))
  (define warning-count (count (lambda (f) (eq? (finding-severity f) 'warning)) all-findings))
  (define info-count (count (lambda (f) (eq? (finding-severity f) 'info)) all-findings))

  (define inventory-md
    (string-append "## Module Inventory\n\n"
                   (format "- **Total modules**: ~a\n- **Total lines**: ~a\n- **By layer**:\n"
                           (hash-ref inv 'total-modules)
                           (hash-ref inv 'total-lines))
                   (string-join (for/list ([(k v) (in-hash (hash-ref inv 'by-layer))])
                                  (format "  - `~a`: ~a modules" k v))
                                "\n")
                   "\n"))

  (define findings-md
    (cond
      [(null? all-findings) "## Findings\n\nNo findings.\n"]
      [else
       (string-append "## Findings\n\n"
                      (format "- **Critical**: ~a\n- **Warning**: ~a\n- **Info**: ~a\n\n"
                              critical-count
                              warning-count
                              info-count)
                      (if (pair? risky)
                          (string-append "### Risky API Usage\n\n"
                                         (string-join (map finding->md risky) "\n")
                                         "\n\n")
                          "")
                      (if (pair? oversized)
                          (string-append "### Oversized Modules\n\n"
                                         (string-join (map finding->md oversized) "\n")
                                         "\n\n")
                          "")
                      (if (pair? todos)
                          (string-append "### TODO/FIXME Markers\n\n"
                                         (string-join (map finding->md todos) "\n")
                                         "\n\n")
                          "")
                      (if (pair? struct-dens)
                          (string-append "### High Struct Density\n\n"
                                         (string-join (map finding->md struct-dens) "\n")
                                         "\n\n")
                          ""))]))

  (define report
    (string-append (format "# Q Project Audit Report\n\nGenerated: ~a\n\n" ts)
                   inventory-md
                   findings-md
                   (format "## Summary\n\nTotal findings: ~a (critical: ~a, warning: ~a, info: ~a)\n"
                           (length all-findings)
                           critical-count
                           warning-count
                           info-count)))

  (values report critical-count all-findings))

;; ── JSON output ──

(define (read-version-string)
  (define p (build-path Q-DIR "util" "version.rkt"))
  (define content (file->string p))
  (define m (regexp-match #rx"q-version \"" content))
  (if m
      (let* ([full (regexp-match-positions #rx"q-version \"([^\"]+)\"" content)]
             [start (caadr full)]
             [end (cdadr full)])
        (substring content start end))
      "unknown"))

(define (generate-json-report report-text critical-count findings inv)
  (define risky (filter (lambda (f) (equal? (finding-category f) "risky-api")) findings))
  (define oversized (filter (lambda (f) (equal? (finding-category f) "oversized")) findings))
  (define todos (filter (lambda (f) (equal? (finding-category f) "todo-marker")) findings))
  (define struct-dens
    (filter (lambda (f) (equal? (finding-category f) "high-struct-density")) findings))
  (define (finding->jsexpr f)
    (hasheq 'severity
            (symbol->string (finding-severity f))
            'category
            (finding-category f)
            'file
            (path->string (find-relative-path Q-DIR (finding-file f)))
            'line
            (finding-line f)
            'message
            (finding-message f)))
  (jsexpr->string (hasheq 'timestamp
                          (parameterize ([date-display-format 'iso-8601])
                            (date->string (current-date) #t))
                          'version
                          (read-version-string)
                          'modules
                          (hasheq 'total
                                  (hash-ref inv 'total-modules)
                                  'lines
                                  (hash-ref inv 'total-lines)
                                  'by-layer
                                  (for/hash ([(k v) (in-hash (hash-ref inv 'by-layer))])
                                    (values (if (string? k)
                                                (string->symbol k)
                                                k)
                                            v)))
                          'large_files
                          (map finding->jsexpr oversized)
                          'risky_apis
                          (map finding->jsexpr risky)
                          'todos
                          (map finding->jsexpr todos)
                          'high_struct_density
                          (map finding->jsexpr struct-dens)
                          'critical_count
                          critical-count)))

;; ── Main ──

(define ci-mode? (member "--ci" (vector->list (current-command-line-arguments))))
(define stdout-mode? (member "--stdout" (vector->list (current-command-line-arguments))))
(define json-mode? (member "--json" (vector->list (current-command-line-arguments))))

(define-values (report critical-count findings) (generate-report))

(cond
  [json-mode?
   ;; Re-scan inventory for JSON output
   (define inv (scan-module-inventory))
   (displayln (generate-json-report report critical-count findings inv))]
  [stdout-mode? (displayln report)]
  [else
   (make-directory* AUDITS-DIR)
   (define out-ts
     (string-replace (parameterize ([date-display-format 'iso-8601])
                       (date->string (current-date) #t))
                     ":"
                     "-"))
   (define out-path (build-path AUDITS-DIR (format "audit-~a.md" out-ts)))
   (call-with-output-file out-path (lambda (out) (display report out)) #:exists 'replace)
   (printf "Audit report written to: ~a\n" (find-relative-path Q-DIR out-path))
   (printf "Findings: ~a total (~a critical)\n" (length findings) critical-count)])

(when ci-mode?
  (if (> critical-count 0)
      (begin
        (printf "CI FAIL: ~a critical findings\n" critical-count)
        (exit 1))
      (printf "CI PASS: no critical findings\n")))
