#lang racket/base

;; runtime/compaction/file-tracker.rkt — file access tracking utilities
;;
;; Extracted from compactor.rkt in v0.99.58 W2-3 (P2-FT).
;; Pure data transformations: analyze tool calls to track which files
;; were read/modified across the conversation history.

(require racket/contract
         racket/set
         racket/string
         (only-in "../../util/content/content-parts.rkt"
                  tool-call-part
                  tool-call-part-arguments
                  tool-call-part-name
                  tool-call-part?)
         (only-in "../../util/message/message.rkt" message-content message-kind message-meta-safe))

(provide (contract-out [extract-file-tracker (-> list? hash?)]
                       [find-previous-file-tracker (-> list? hash?)]
                       [merge-file-trackers (->* () () #:rest list? hash?)]))

;; Extract file read/modified tracking from a list of messages.
;; Returns a hash: 'readFiles -> list of paths,
;;                  'modifiedFiles -> list of paths.
(define (extract-file-tracker messages)
  (define-values (reads writes)
    (for*/fold ([reads (set)]
                [writes (set)])
               ([m (in-list messages)]
                [part (in-list (message-content m))])
      (cond
        [(not (tool-call-part? part)) (values reads writes)]
        [else
         (define tool-name (tool-call-part-name part))
         (define args (tool-call-part-arguments part))
         (define path
           (cond
             [(hash? args) (hash-ref args 'path #f)]
             [(string? args)
              (and (string-contains? args "path")
                   (let ([m (regexp-match #rx"\"path\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" args)])
                     (and m (cadr m))))]
             [else #f]))
         (cond
           [(not path) (values reads writes)]
           [(member tool-name '("read" "find" "grep" "ls")) (values (set-add reads path) writes)]
           [(member tool-name '("edit" "write")) (values reads (set-add writes path))]
           [else (values reads writes)])])))
  (hasheq 'readFiles (set->list reads) 'modifiedFiles (set->list writes)))

;; #768: Find the file tracker from the most recent compaction summary.
;; Returns a hash like (hasheq 'readFiles (...) 'modifiedFiles (...)) or (hasheq).
(define (find-previous-file-tracker messages)
  (or (for/first ([m (in-list (reverse messages))]
                  #:when (eq? (message-kind m) 'compaction-summary))
        (define meta (message-meta-safe m))
        (hash-ref meta 'fileTracker (hasheq)))
      (hasheq)))

;; #768: Merge two file trackers, deduplicating file paths.
(define (merge-file-trackers . trackers)
  (define-values (all-reads all-writes)
    (for*/fold ([reads (set)]
                [writes (set)])
               ([ft (in-list trackers)])
      (values (for/fold ([r reads]) ([path (in-list (hash-ref ft 'readFiles '()))])
                (set-add r path))
              (for/fold ([w writes]) ([path (in-list (hash-ref ft 'modifiedFiles '()))])
                (set-add w path)))))
  (hasheq 'readFiles (set->list all-reads) 'modifiedFiles (set->list all-writes)))
