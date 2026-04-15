#lang racket/base

;; runtime/session-metadata.rkt — Session Metadata, Labeling, File Tracking (#710-#712)
;;
;; Provides:
;;   #710: Session naming — get/set session name for display
;;   #711: Entry labeling — mark important session points
;;   #712: Parent feature integration

(require racket/contract
         racket/list
         racket/port
         (only-in "../util/protocol-types.rkt"
                  message? message-content message-meta message-kind
                  make-message make-text-part content-part->jsexpr)
         "../util/jsonl.rkt"
         "../runtime/session-store.rkt")

(provide
 ;; #710: Session naming
 set-session-name!
 get-session-name
 session-name-entry?

 ;; #711: Entry labeling
 set-entry-label!
 get-entry-label
 entry-label?
 label-type?
 list-labeled-entries
 ;; Label types
 LABEL-CHECKPOINT
 LABEL-MILESTONE
 LABEL-BRANCH-POINT)

;; ============================================================
;; Constants
;; ============================================================

(define LABEL-CHECKPOINT 'checkpoint)
(define LABEL-MILESTONE 'milestone)
(define LABEL-BRANCH-POINT 'branch-point)

(define (label-type? v)
  (and (symbol? v)
       (or (eq? v LABEL-CHECKPOINT)
           (eq? v LABEL-MILESTONE)
           (eq? v LABEL-BRANCH-POINT))))

;; ============================================================
;; #710: Session naming
;; ============================================================

;; Set (rename) a session. Persists to first JSONL entry.
(define (set-session-name! log-path name)
  (write-session-name! log-path name))

;; Get the current session name from the log.
;; Returns the last set name, or #f if no name is set.
(define (get-session-name log-path)
  (define entries (load-session-log log-path))
  (define name-entries
    (filter session-name-entry? entries))
  (if (null? name-entries)
      #f
      (hash-ref (message-meta (car (reverse name-entries))) 'name #f)))

;; Check if an entry is a session-name entry.
(define (session-name-entry? entry)
  (and (message? entry)
       (eq? (message-kind entry) 'session-info)
       (hash-has-key? (message-meta entry) 'name)))

;; ============================================================
;; #711: Entry labeling
;; ============================================================

;; Set a label on a specific entry in the session log.
;; Appends a label entry that references the target entry.
(define (set-entry-label! log-path target-entry-id label-type
                           #:description [description #f])
  (unless (label-type? label-type)
    (raise-argument-error 'set-entry-label! "label-type?" label-type))
  (define label-msg
    (make-message (format "label-~a-~a" label-type (current-inexact-milliseconds))
                  target-entry-id
                  'system
                  'entry-label
                  (list (make-text-part (or description
                                           (format "~a label for ~a" label-type target-entry-id))))
                  (current-seconds)
                  (hasheq 'label-type (symbol->string label-type)
                          'target-id target-entry-id
                          'description description)))
  (append-entry! log-path label-msg))

;; Get the label for a specific entry, if any.
;; Returns the label-type symbol or #f.
(define (get-entry-label entries target-entry-id)
  (define label-entries
    (filter (lambda (e) (and (eq? (message-kind e) 'entry-label)
                              (equal? (hash-ref (message-meta e) 'target-id #f)
                                      target-entry-id)))
            entries))
  (if (null? label-entries)
      #f
      (let ([raw (hash-ref (message-meta (car (reverse label-entries))) 'label-type #f)])
        (if (string? raw) (string->symbol raw) raw))))

;; Check if an entry is a label entry.
(define (entry-label? entry)
  (and (message? entry)
       (eq? (message-kind entry) 'entry-label)))

;; List all labeled entries from a session log.
;; Returns list of (cons target-id label-type).
(define (list-labeled-entries entries)
  (filter-map
   (lambda (e)
     (and (entry-label? e)
          (let ([raw (hash-ref (message-meta e) 'label-type #f)])
            (cons (hash-ref (message-meta e) 'target-id #f)
                  (if (string? raw) (string->symbol raw) raw)))))
   entries))
