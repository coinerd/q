#!/usr/bin/env racket
#lang racket/base

;; scripts/architecture-graph-analysis.rkt — graph views over a baseline snapshot.
;;
;; CL0 (#9977): reads the raw .rktd emitted by scripts/architecture-baseline.rkt
;; and computes strongly connected components, transitive dependency closure,
;; closure-weighted hubs/dependents, cross-subsystem edges and a per-subsystem
;; summary. Pure analysis of an already-pinned snapshot; never reads Git.
;;
;; Usage:
;;   racket scripts/architecture-graph-analysis.rkt \
;;     --snapshot docs/reports/architecture-baseline-CL0-003e425d.rktd \
;;     --raw docs/reports/architecture-graph-CL0-003e425d.rktd \
;;     --markdown docs/reports/architecture-graph-CL0-003e425d.md
;;
;; Options: --top N limits hub/edge listings (default 25). Both --raw and
;; --markdown are required so every run leaves machine-readable evidence.

(require racket/cmdline
         racket/file
         "architecture-graph-helpers.rkt")

(define snapshot-path #f)
(define raw-path #f)
(define markdown-path #f)
(define top 25)

(command-line
 #:program "architecture-graph-analysis.rkt"
 #:once-each
 [("--snapshot") path "Input: architecture-baseline raw .rktd snapshot" (set! snapshot-path path)]
 [("--raw") path "Machine-readable analysis output path (.rktd)" (set! raw-path path)]
 [("--markdown") path "Markdown report output path" (set! markdown-path path)]
 [("--top")
  count
  "Rows in each ranked listing (default 25)"
  (define parsed (string->number count))
  (unless (and (exact-integer? parsed) (positive? parsed))
    (raise-user-error 'architecture-graph-analysis "--top must be a positive integer"))
  (set! top parsed)])

(unless snapshot-path
  (raise-user-error 'architecture-graph-analysis "missing required --snapshot PATH"))
(unless raw-path
  (raise-user-error 'architecture-graph-analysis "missing required --raw PATH"))
(unless markdown-path
  (raise-user-error 'architecture-graph-analysis "missing required --markdown PATH"))

(define snapshot (call-with-input-file snapshot-path read))
(define analysis (analyze-module-graph snapshot #:top top))

(call-with-output-file raw-path
                       (lambda (output) (writeln analysis output))
                       #:exists 'truncate/replace)
(call-with-output-file markdown-path
                       (lambda (output) (display (graph-analysis->markdown analysis) output))
                       #:exists 'truncate/replace)
