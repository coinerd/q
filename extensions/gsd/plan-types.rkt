#lang racket/base

;; extensions/gsd/plan-types.rkt — Structured Plan/Task/Wave types
;;
;; Wave 0 of v0.21.0: Machine-parseable plan types with validation.
;; Plans use a structured format with required fields.
;; The extension validates plan structure before allowing /go.

(require racket/match
         racket/string
         racket/list)

;; ============================================================
;; Core structs (immutable)
;; ============================================================

(struct gsd-task (name files action verify done status) #:transparent)

(struct gsd-wave (index title status root-cause files tasks verify done-criteria) #:transparent)

(struct gsd-plan (waves context-bundle constraints must-haves) #:transparent)

(struct validation-result (errors warnings) #:transparent)

;; Aliases for cleaner API
(define validation-errors validation-result-errors)
(define validation-warnings validation-result-warnings)

;; ============================================================
;; Convenience constructors
;; ============================================================

(define (make-gsd-task name files action verify [done ""])
  (gsd-task name files action verify done 'pending))

(define (make-gsd-wave index title root-cause files tasks verify done-criteria)
  (gsd-wave index title 'pending root-cause files tasks verify done-criteria))

;; ============================================================
;; Status setters (return new struct — immutable)
;; ============================================================

(define (gsd-task-set-status t status)
  (struct-copy gsd-task t [status status]))

(define (gsd-wave-set-status w status)
  (struct-copy gsd-wave w [status status]))

;; ============================================================
;; Validation helpers
;; ============================================================

(define (validation-valid? vr)
  (null? (validation-errors vr)))

;; ============================================================
;; Parsing: markdown → (listof gsd-wave)
;; ============================================================

;; Parse PLAN.md content into structured waves.
;; Looks for "## Wave N: Title" headers and extracts fields from
;; bullet points within each wave section.
(define (parse-waves-from-markdown md-text)
  (define lines (string-split md-text "\n"))
  ;; Find wave header line indices
  (define wave-starts
    (for/list ([line lines]
               [idx (in-naturals)]
               #:when (regexp-match #rx"^## +[Ww]ave +[0-9]+" line))
      idx))
  (define wave-end-idxs
    (if (< (length wave-starts) 2)
        (list (sub1 (length lines)))
        (append (map sub1 (cdr wave-starts)) (list (sub1 (length lines))))))
  ;; Parse each wave section
  (for/list ([start wave-starts]
             [end wave-end-idxs])
    (parse-wave-section (take (drop lines start) (add1 (- end start))))))

;; Clean file path: strip surrounding backticks and whitespace.
;; Handles: `path` → path, ```path``` → path, path → path
(define (clean-file-path s)
  (define trimmed (string-trim s))
  (cond
    [(>= (string-length trimmed) 6)
     ;; Check for triple backticks: ```path```
     (define triple-back (string-prefix? trimmed "```"))
     (define triple-end (string-suffix? trimmed "```"))
     (if (and triple-back triple-end)
         (string-trim (substring trimmed 3 (- (string-length trimmed) 3)))
         ;; Check for single backticks: `path`
         (if (and (string-prefix? trimmed "`") (string-suffix? trimmed "`"))
             (string-trim (substring trimmed 1 (- (string-length trimmed) 1)))
             trimmed))]
    [(>= (string-length trimmed) 2)
     (if (and (string-prefix? trimmed "`") (string-suffix? trimmed "`"))
         (string-trim (substring trimmed 1 (- (string-length trimmed) 1)))
         trimmed)]
    [else trimmed]))

;; Parse structured fields from wave document content.
;; Handles both bullet-style and heading-style formats.
;; Single source of truth for field extraction (F3 consolidation).
(define (parse-wave-content content)
  (define lines (string-split content "\n"))
  (define n (length lines))
  (define root-cause "")
  (define files '())
  (define verify-cmd "")
  (define done-criteria '())
  (define in-files-section #f) ; track ## Files heading section
  (for ([line lines]
        [i (in-naturals)])
    (define trimmed (string-trim line))
    ;; Update section tracking BEFORE cond dispatch
    (when (regexp-match? #rx"^## " trimmed)
      (set! in-files-section (string-prefix? trimmed "## Files")))
    (cond
      ;; Heading-style: ## Verify — collect non-empty, non-backtick lines after heading
      [(string-prefix? trimmed "## Verify")
       (define after
         (for/list ([j (in-range (add1 i) (min n (+ i 5)))]
                    #:when (and (> (string-length (string-trim (list-ref lines j))) 0)
                                (not (string-contains? (list-ref lines j) "```"))))
           (string-trim (list-ref lines j))))
       (when (and (string=? verify-cmd "") (not (null? after)))
         (set! verify-cmd (string-join after "; ")))]
      ;; Bullet-style Root cause
      [(regexp-match #rx"^- +[Rr]oot *[Cc]ause *: *(.+)$" line)
       =>
       (lambda (m) (set! root-cause (string-trim (cadr m))))]
      ;; Bullet-style Files (plural, comma-separated)
      [(regexp-match #rx"^- +[Ff]iles *: *(.+)$" line)
       =>
       (lambda (m)
         (define paths (string-split (string-trim (cadr m)) ","))
         (set! files (append files (map (lambda (p) (clean-file-path (string-trim p))) paths))))]
      ;; Bullet-style File (singular)
      [(regexp-match #rx"^- +[Ff]ile *: *(.+)$" line)
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      ;; Bare bullets inside ## Files section
      [(and in-files-section (regexp-match #rx"^- +(.+)$" line))
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      ;; Bullet-style Verify
      [(regexp-match #rx"^- +[Vv]erify *: *(.+)$" line)
       =>
       (lambda (m) (set! verify-cmd (string-trim (cadr m))))]
      ;; Bullet-style Done
      [(regexp-match #rx"^- +[Dd]one *: *(.+)$" line)
       =>
       (lambda (m) (set! done-criteria (append done-criteria (list (string-trim (cadr m))))))]))
  (hasheq 'root-cause root-cause 'files files 'verify verify-cmd 'done done-criteria))

;; Parse a single wave section (list of lines, first is header).
;; Uses unified parse-wave-content for field extraction.
(define (parse-wave-section lines)
  (define header (car lines))
  (define body-lines (cdr lines))
  (define header-match (regexp-match #rx"^## +[Ww]ave +([0-9]+) *: *(.+)$" header))
  (define idx
    (if header-match
        (string->number (cadr header-match))
        0))
  (define title
    (if header-match
        (string-trim (caddr header-match))
        ""))
  ;; Use unified parser
  (define fields (parse-wave-content (string-join body-lines "\n")))
  (gsd-wave idx
            title
            'pending
            (hash-ref fields 'root-cause "")
            (hash-ref fields 'files '())
            '()
            (hash-ref fields 'verify "")
            (hash-ref fields 'done '())))

;; ============================================================
;; Validation
;; ============================================================

(define (validate-plan plan)
  (define waves (gsd-plan-waves plan))
  (define errors '())
  (define warnings '())
  ;; Check: plan has at least 1 wave
  (when (null? waves)
    (set! errors (cons "Plan has no waves" errors)))
  ;; Per-wave checks
  (for ([w waves])
    (define widx (gsd-wave-index w))
    (define prefix (format "Wave ~a" widx))
    (when (string=? (gsd-wave-title w) "")
      (set! errors (cons (format "~a: missing title" prefix) errors)))
    (when (null? (gsd-wave-files w))
      (set! warnings (cons (format "~a: no file references" prefix) warnings)))
    (when (string=? (gsd-wave-verify w) "")
      (set! warnings (cons (format "~a: no verify command" prefix) warnings))))
  ;; Error: ALL waves are file-less — plan has nothing to execute on
  (when (and (not (null? waves))
             (for/and ([w waves])
               (null? (gsd-wave-files w))))
    (set! errors (cons "Plan has no file references in any wave — nothing to execute" errors)))
  (validation-result (reverse errors) (reverse warnings)))

;; ============================================================
;; Plan utilities
;; ============================================================

(define (plan-wave-ref plan idx)
  (for/first ([w (gsd-plan-waves plan)]
              #:when (= (gsd-wave-index w) idx))
    w))

(define (plan-pending-waves plan)
  (filter (lambda (w) (eq? (gsd-wave-status w) 'pending)) (gsd-plan-waves plan)))

(define (plan-next-pending-wave plan)
  (for/first ([w (gsd-plan-waves plan)]
              #:when (eq? (gsd-wave-status w) 'pending))
    w))

;; ============================================================
;; Status conversion helpers
;; ============================================================

(define (wave-status->string sym)
  (cond
    [(eq? sym 'pending) "Inbox"]
    [(eq? sym 'in-progress) "In-Progress"]
    [(eq? sym 'completed) "DONE"]
    [(eq? sym 'failed) "FAILED"]
    [(eq? sym 'skipped) "DEFERRED"]
    [else (symbol->string sym)]))

(define (string->wave-status str)
  (cond
    [(string=? str "Inbox") 'pending]
    [(string=? str "In-Progress") 'in-progress]
    [(string=? str "DONE") 'completed]
    [(string=? str "FAILED") 'failed]
    [(string=? str "DEFERRED") 'skipped]
    [else 'pending]))

(define (gsd-wave-slug w)
  (define title (gsd-wave-title w))
  (if (and (string? title) (> (string-length title) 0))
      (let* ([s (string-trim title)]
             [chars (for/list ([c (in-string s)])
                      (cond
                        [(char-alphabetic? c) (char-downcase c)]
                        [(char-numeric? c) c]
                        [(char=? c #\-) #\-]
                        [(char=? c #\space) #\-]
                        [else #f]))]
             [result (list->string (filter values chars))])
        (if (> (string-length result) 40)
            (string-trim (substring result 0 40) "-" #:right? #t)
            (if (string=? result "") "wave" result)))
      "wave"))

;; ============================================================
;; Provide (after all definitions)
;; ============================================================

;; Struct types + accessors
(provide gsd-task
         gsd-task?
         gsd-task-name
         gsd-task-files
         gsd-task-action
         gsd-task-verify
         gsd-task-done
         gsd-task-status
         gsd-task-set-status

         gsd-wave
         gsd-wave?
         gsd-wave-index
         gsd-wave-title
         gsd-wave-status
         gsd-wave-root-cause
         gsd-wave-files
         gsd-wave-tasks
         gsd-wave-verify
         gsd-wave-done-criteria
         gsd-wave-set-status

         gsd-plan
         gsd-plan?
         gsd-plan-waves
         gsd-plan-context-bundle
         gsd-plan-constraints
         gsd-plan-must-haves

         ;; Constructors with defaults
         make-gsd-task
         make-gsd-wave

         ;; Parsing
         parse-waves-from-markdown
         parse-wave-content
         clean-file-path

         ;; Validation
         validation-result
         validation-result?
         validation-errors
         validation-warnings
         validation-valid?
         validate-plan

         ;; Plan utilities
         plan-wave-ref
         plan-pending-waves
         plan-next-pending-wave

         ;; Status conversion helpers
         wave-status->string
         string->wave-status
         gsd-wave-slug)
