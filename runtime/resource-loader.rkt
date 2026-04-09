#lang racket/base

;; runtime/resource-loader.rkt — discover and load local/global resources
;;
;; Responsibility: discover and load system instructions, skills,
;; prompt templates, and local config from global (~/.q/) and
;; project-local (.q/ or .pi/) directories.
;;
;; This module re-exports all bindings from skills/types.rkt for
;; backward compatibility. The actual implementations are in
;; skills/types.rkt to avoid circular dependencies.

(require "../skills/types.rkt")

(provide
 ;; Structs (re-exported from skills/types.rkt)
 (struct-out resource)
 (struct-out resource-set)
 empty-resource-set

 ;; Main API (re-exported from skills/types.rkt)
 load-global-resources
 load-project-resources
 merge-resources

 ;; Template rendering (re-exported from skills/types.rkt)
 render-template)
