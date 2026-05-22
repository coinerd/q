#lang racket/base

;; extensions/gsd/wave-docs.rkt — Wave document I/O and PLAN.md index management
;; STABILITY: evolving
;;
;; v0.21.1 W0: Per-wave document files in .planning/waves/.
;; PLAN.md index with status markers ([Inbox], [DONE], [DEFERRED], [FAILED]).
;; Dual-write: wave doc + PLAN.md index on status transitions.
;;
;; DUAL-WRITE INVARIANT (I-24, v0.35.1):
;; Every wave status change must update BOTH:
;;   1. The per-wave document file (.planning/waves/W*.md)
;;   2. The PLAN.md index table (status column)
;; The mark-wave-status! helper enforces this. Direct file writes
;; that update only one of the two are a bug.

(require racket/contract
         racket/format
         racket/string
         racket/file
         racket/path
         racket/port
         (only-in "shared.rkt" slugify)
         (only-in "wave-status.rkt"
                  STATUS-INBOX
                  STATUS-DONE
                  STATUS-DEFERRED
                  STATUS-FAILED
                  done-or-deferred?
                  active-status?
                  normalize-status!))

;; Struct exports (plain)
(provide wave-index-entry
         wave-index-entry?
         wave-index-entry-idx
         wave-index-entry-title
         wave-index-entry-slug
         wave-index-entry-status
         ;; Functions (contracted)
         (contract-out [wave-doc-path (-> any/c any/c any/c any/c)]
                       [write-wave-doc! (-> any/c any/c any/c any/c any/c any/c)]
                       [read-wave-doc (-> any/c any/c any/c any/c)]
                       [parse-wave-doc-from-string (->* (any/c any/c any/c) (any/c) any/c)]
                       [slugify (-> string? string?)]
                       [parse-plan-index (-> string? any/c)]
                       [update-wave-in-index! (-> any/c any/c any/c any/c)]
                       [update-plan-index-text (-> string? any/c string? string?)]
                       [next-inbox-wave (-> any/c any/c)]
                       [find-next-inbox-entry (-> any/c any/c)]
                       [mark-wave-status! (-> any/c any/c any/c any/c)]
                       [plan-overall-status (-> any/c symbol?)]
                       [compute-plan-overall-status (-> any/c symbol?)]
                       [wave-exists? (-> any/c any/c any/c boolean?)]
                       [wave-status-markers (-> any/c)]))

;; ============================================================
;; Constants
;; ============================================================

(define WAVE-STATUS-MARKERS
  (list (cons STATUS-INBOX "[Inbox]")
        (cons "In-Progress" "[In-Progress]")
        (cons STATUS-DONE "[DONE]")
        (cons STATUS-DEFERRED "[DEFERRED]")
        (cons STATUS-FAILED "[FAILED]")))

(define (wave-status-markers)
  WAVE-STATUS-MARKERS)

;; Pre-compiled regex patterns (using #rx for proper \n handling)
(define wave-header-rx #rx"^# Wave [0-9]+\nStatus: ([^\n]+)\n")
(define wave-header-full-rx #rx"^# Wave [0-9]+\nStatus: [^\n]+\n\n(.*)$")
(define index-line-rx #rx"^[-*] +\\[([A-Za-z-]+)\\] +W([0-9]+): +(.+?)(?: +\u2192 +(.+))?$")
(define slug-from-target-rx #rx"waves/W[0-9]+-(.+?)\\.md")

;; ============================================================
;; Slug generation
;; ============================================================

;; slugify + collapse-hyphens: imported from shared.rkt (v0.32.1 Wave 1 DRY)

;; ============================================================
;; Wave document path
;; ============================================================

(define (wave-doc-path base-dir idx slug)
  (build-path base-dir ".planning" "waves" (format "W~a-~a.md" idx slug)))

;; ============================================================
;; Wave document I/O
;; ============================================================

(define (wave-exists? base-dir idx slug)
  (file-exists? (wave-doc-path base-dir idx slug)))

(define (write-wave-doc! base-dir idx slug content status)
  (define path (wave-doc-path base-dir idx slug))
  (define dir (path-only path))
  (unless (directory-exists? dir)
    (make-directory* dir))
  (define header (format "# Wave ~a\nStatus: ~a\n\n" idx status))
  (call-with-output-file path
                         (lambda (out)
                           (display header out)
                           (display content out))
                         #:exists 'truncate)
  path)

(define (parse-wave-doc-from-string text idx slug [path #f])
  (define status (extract-status text))
  (define content (strip-status-header text))
  (hasheq 'index
          idx
          'slug
          slug
          'status
          status
          'content
          content
          'path
          (if path
              (path->string path)
              #f)))

(define (read-wave-doc base-dir idx slug)
  (define path (wave-doc-path base-dir idx slug))
  (cond
    [(not (file-exists? path)) #f]
    [else
     (define text (call-with-input-file path port->string))
     (parse-wave-doc-from-string text idx slug path)]))

(define (extract-status text)
  (define m (regexp-match wave-header-rx text))
  (if m
      (string-trim (cadr m))
      "Inbox"))

(define (strip-status-header text)
  (define m (regexp-match wave-header-full-rx text))
  (if m
      (cadr m)
      text))

;; ============================================================
;; PLAN.md index parsing
;; ============================================================

(struct wave-index-entry (idx title slug status) #:transparent)

(define (parse-plan-index md-text)
  (define lines (string-split md-text "\n"))
  (for/fold ([entries '()]) ([line lines])
    (define m (regexp-match index-line-rx line))
    (if m
        (let* ([raw-status (cadr m)]
               [status (or (normalize-status! raw-status) raw-status)]
               [idx (string->number (caddr m))]
               [title (string-trim (cadddr m))]
               [target (and (list? m) (> (length m) 4) (list-ref m 4))]
               [slug (if target
                         (extract-slug-from-target target)
                         (slugify title))])
          (append entries (list (wave-index-entry idx title slug status))))
        entries)))

(define (extract-slug-from-target target)
  (define m (regexp-match slug-from-target-rx target))
  (if m
      (cadr m)
      (slugify target)))

;; ============================================================
;; PLAN.md index status update
;; ============================================================

(define (update-plan-index-text text wave-idx new-status)
  (define marker (status->marker new-status))
  (update-index-line text wave-idx marker))

(define (update-wave-in-index! base-dir wave-idx new-status)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define new-text (update-plan-index-text text wave-idx new-status))
     (call-with-output-file plan-path (lambda (out) (display new-text out)) #:exists 'truncate)
     #t]))

(define (status->marker status)
  (cond
    [(string? status)
     (define entry (assoc status WAVE-STATUS-MARKERS))
     (if entry
         (cdr entry)
         (format "[~a]" status))]
    [(symbol? status) (status->marker (symbol->string status))]
    [else "[Inbox]"]))

(define (update-index-line text wave-idx new-marker)
  (define lines (string-split text "\n"))
  (define update-rx
    (regexp (string-append "^([-*] +)\\[([A-Za-z-]+)\\]( +W" (number->string wave-idx) ":.*)$")))
  (define new-lines
    (for/list ([line lines])
      (define m (regexp-match update-rx line))
      (if m
          (string-append (cadr m) new-marker (list-ref m 3))
          line)))
  (string-join new-lines "\n"))

;; ============================================================
;; Wave queries
;; ============================================================

(define (find-next-inbox-entry entries)
  (for/first ([e entries]
              #:when (or (string=? (wave-index-entry-status e) STATUS-INBOX)
                         (string=? (wave-index-entry-status e) STATUS-FAILED)))
    e))

(define (next-inbox-wave base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (find-next-inbox-entry (parse-plan-index text))]))

;; ============================================================
;; Dual-write status transition
;; ============================================================

(define (mark-wave-status! base-dir wave-idx new-status)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (define entry
       (for/first ([e entries]
                   #:when (= (wave-index-entry-idx e) wave-idx))
         e))
     (cond
       [(not entry) #f]
       [else
        (define slug (wave-index-entry-slug entry))
        (define wave-path (wave-doc-path base-dir wave-idx slug))
        (when (file-exists? wave-path)
          (define wave-text (call-with-input-file wave-path port->string))
          (define content (strip-status-header wave-text))
          (write-wave-doc! base-dir wave-idx slug content new-status))
        (update-wave-in-index! base-dir wave-idx new-status)
        #t])]))

;; ============================================================
;; Plan overall status
;; ============================================================

(define (compute-plan-overall-status entries)
  (cond
    [(null? entries) 'not-started]
    [else
     (define statuses (map wave-index-entry-status entries))
     (define all-done?
       (for/and ([s statuses])
         (done-or-deferred? s)))
     (define any-progress?
       (for/or ([s statuses])
         (not (string=? s STATUS-INBOX))))
     (cond
       [all-done? 'all-done]
       [any-progress? 'partly-done]
       [else 'in-progress])]))

(define (plan-overall-status base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) 'not-started]
    [else
     (define text (call-with-input-file plan-path port->string))
     (compute-plan-overall-status (parse-plan-index text))]))
