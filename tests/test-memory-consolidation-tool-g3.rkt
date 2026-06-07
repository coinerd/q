#lang racket/base
;;; test-memory-consolidation-tool-g3.rkt — W0 characterization for G3 consolidate-memory gap

(require rackunit
         racket/file
         racket/string
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt")

(define memory-tools-source (file->string "../tools/builtins/memory-tools.rkt"))

(test-case "W0 G3 characterization: consolidate-memory tool is currently absent"
  (check-false (string-contains? memory-tools-source "consolidate-memory")))

(test-case "W0 G3 characterization: existing cleanup-expired-memory tool remains present"
  (check-pred tool? cleanup-expired-memory))
