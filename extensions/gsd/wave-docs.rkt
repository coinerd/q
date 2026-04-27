#lang racket/base

;; extensions/gsd/wave-docs.rkt — Wave document I/O and PLAN.md index management
;;
;; v0.21.1 W0: Per-wave document files in .planning/waves/.
;; PLAN.md index with status markers ([Inbox], [DONE], [DEFERRED], [FAILED]).
;; Dual-write: wave doc + PLAN.md index on status transitions.

(require racket/format
         racket/string
         racket/file
         racket/path
         racket/port)

(provide wave-doc-path
         write-wave-doc!
         read-wave-doc
         slugify
         parse-plan-index
         update-wave-in-index!
         next-inbox-wave
         mark-wave-status!
         plan-overall-status
         wave-exists?
         wave-status-markers
         wave-index-entry
         wave-index-entry?
         wave-index-entry-idx
         wave-index-entry-title
         wave-index-entry-slug
         wave-index-entry-status)

;; ============================================================
;; Constants
;; ============================================================

(define WAVE-STATUS-MARKERS
  '(("Inbox" . "[Inbox]") ("In-Progress" . "[In-Progress]")
                          ("DONE" . "[DONE]")
                          ("DEFERRED" . "[DEFERRED]")
                          ("FAILED" . "[FAILED]")))

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

(define (slugify title)
  (define s (string-trim title))
  (define slug-chars
    (for/list ([c (in-string s)])
      (cond
        [(char-alphabetic? c) (char-downcase c)]
        [(char-numeric? c) c]
        [(char=? c #\-) #\-]
        [(char=? c #\space) #\-]
        [else #f])))
  (define cleaned (collapse-hyphens (filter values slug-chars)))
  (define result (list->string cleaned))
  (define truncated
    (if (> (string-length result) 40)
        (let ([s40 (substring result 0 40)]) (string-trim s40 "-" #:right? #t))
        result))
  (if (string=? truncated "") "wave" truncated))

(define (collapse-hyphens chars)
  (let loop ([cs chars]
             [prev-hyphen? #f]
             [acc '()])
    (cond
      [(null? cs) (reverse acc)]
      [(and prev-hyphen? (char=? (car cs) #\-)) (loop (cdr cs) #t acc)]
      [(char=? (car cs) #\-) (loop (cdr cs) #t (cons #\- acc))]
      [else (loop (cdr cs) #f (cons (car cs) acc))])))

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

(define (read-wave-doc base-dir idx slug)
  (define path (wave-doc-path base-dir idx slug))
  (cond
    [(not (file-exists? path)) #f]
    [else
     (define text (call-with-input-file path port->string))
     (define status (extract-status text))
     (define content (strip-status-header text))
     (hasheq 'index idx 'slug slug 'status status 'content content 'path (path->string path))]))

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
        (let* ([status (cadr m)]
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

(define (update-wave-in-index! base-dir wave-idx new-status)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define marker (status->marker new-status))
     (define new-text (update-index-line text wave-idx marker))
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

(define (next-inbox-wave base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (for/first ([e entries]
                 #:when (or (string=? (wave-index-entry-status e) "Inbox")
                            (string=? (wave-index-entry-status e) "FAILED")))
       e)]))

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

(define (plan-overall-status base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) 'not-started]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (cond
       [(null? entries) 'not-started]
       [else
        (define statuses (map wave-index-entry-status entries))
        (define all-done?
          (for/and ([s statuses])
            (or (string=? s "DONE") (string=? s "DEFERRED"))))
        (define any-done?
          (for/or ([s statuses])
            (or (string=? s "DONE")
                (string=? s "DEFERRED")
                (string=? s "FAILED")
                (string=? s "In-Progress"))))
        (cond
          [all-done? 'all-done]
          [any-done? 'partly-done]
          [else 'in-progress])])]))
