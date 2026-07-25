#lang racket/base

;; tools/registry-table.rkt — Declarative tool spec table (thin facade)
;; STABILITY: internal
;;
;; Encodes all built-in tools as tool-spec structs.
;; Domain-specific specs live in registry-table/*.rkt.
;;
;; register-tools-from-specs! converts specs → make-tool calls.

(require "tool.rkt"
         "registry-table/spec.rkt"
         "registry-table/core-tools.rkt"
         "registry-table/browser-tools.rkt"
         "registry-table/memory-tools.rkt"
         "registry-table/skill-tools.rkt"
         "tool-classification.rkt")

(provide register-tools-from-specs!
         dangerous-tool-names
         externalizable-tool-names
         mutates-filesystem-tool-names
         tool-specs
         tool-spec
         tool-spec?
         tool-spec-name
         tool-spec-description
         tool-spec-schema
         tool-spec-handler
         tool-spec-prompt-guidelines
         tool-spec-required-capability)

;; ============================================================
;; Combined tool spec table
;; ============================================================

(define tool-specs (append core-tool-specs browser-tool-specs memory-tool-specs skill-tool-specs))

;; Compatibility/introspection view only. Classification remains authoritative;
;; registration never consults this derived list.
(define dangerous-tool-names (filter tool-name-needs-approval? (all-classified-tool-names)))

;; M2: Tools explicitly marked as externalizable (safe to run in worker process).
;; The worker process supports: bash, write, edit, delete-lines (plus git via worker-tools.rkt).
;; All other tools default to #:externalizable? #f (run in-process even when
;; execution plane is enabled).
;;
;; v0.99.20 W2 (§3.2): Added delete-lines — it's a pure file-edit operation
;; (reads file, deletes line range, writes back) fully implementable in the worker.
;;
;; NOTE: browser_click, browser_type, browser_press are dangerous but NOT
;; externalizable because they require a running Chromium process that only
;; exists in the main process. A pass-through proxy architecture is planned
;; for M4 (v1.0.0-rc2) per the MAS Enablement Strategy §3.2.
;; Tools that mutate the filesystem (write, edit, delete-lines)
;; Used to select filesystem mutation serialization via with-file-mutation-queue.
(define mutates-filesystem-tool-names '("write" "edit" "delete-lines"))

(define externalizable-tool-names '("bash" "write" "edit" "delete-lines"))

;; Register tools from tool-spec structs.
(define (register-tools-from-specs! registry specs #:only [only #f])
  (for ([spec (in-list specs)])
    (cond
      [(tool-spec? spec)
       (define name (tool-spec-name spec))
       (when (or (not only) (member name only))
         (define pg (tool-spec-prompt-guidelines spec))
         (define rc (tool-spec-required-capability spec))
         ;; v0.99.66 R1: classification is the sole authority for danger metadata.
         (define dangerous? (tool-name-needs-approval? name))
         (define externalizable? (and (member name externalizable-tool-names) #t))
         (define mutates-filesystem? (and (member name mutates-filesystem-tool-names) #t))
         (if pg
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:prompt-guidelines pg
                                        #:dangerous? dangerous?
                                        #:mutates-filesystem? mutates-filesystem?
                                        #:required-capability rc
                                        #:externalizable? externalizable?))
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:dangerous? dangerous?
                                        #:mutates-filesystem? mutates-filesystem?
                                        #:required-capability rc
                                        #:externalizable? externalizable?))))]))
  (void))
