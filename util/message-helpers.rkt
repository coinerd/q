#lang racket/base

;; util/message-helpers.rkt — shared message analysis utilities
;;
;; Consolidates functions that were duplicated across:
;;   - runtime/compactor.rkt
;;   - runtime/cutpoint-rules.rkt
;;   - runtime/context-manager.rkt
;;   - runtime/session-store.rkt
;;   - runtime/session-index.rkt

(require racket/list
         racket/path
         racket/file
         "../util/protocol-types.rkt")

(provide has-tool-calls?
         tool-result-message?
         get-tool-call-ids
         ensure-parent-dirs!)

;; ============================================================
;; Message analysis
;; ============================================================

;; Check if a message contains any tool-call parts.
;; Used by compactor, cutpoint-rules, and context-manager
;; to preserve tool_call/result pair atomicity.
(define (has-tool-calls? msg)
  (define content (message-content msg))
  (and (list? content) (ormap tool-call-part? content)))

;; Check if a message is a tool-result or bash-execution entry.
;; Used by compactor and cutpoint-rules for cutpoint validation.
(define (tool-result-message? msg)
  (memq (message-kind msg) '(tool-result bash-execution)))

;; Extract all tool-call IDs from a message's content parts.
;; Used by context-manager for pair-aware trimming.
(define (get-tool-call-ids msg)
  (for/list ([cp (in-list (message-content msg))]
             #:when (tool-call-part? cp))
    (tool-call-part-id cp)))

;; ============================================================
;; File system helpers
;; ============================================================

;; Create parent directories of path if they don't exist.
;; Consolidated from session-store.rkt and session-index.rkt.
(define (ensure-parent-dirs! path)
  (define dir (let ([p (path-only path)]) (if p p #f)))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir)))
