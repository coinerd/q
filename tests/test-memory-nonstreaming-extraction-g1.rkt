#lang racket/base
;;; test-memory-nonstreaming-extraction-g1.rkt — W0 characterization for G1b non-streaming extraction gap

(require rackunit
         racket/file
         racket/string)

(define main-loop-source (file->string "agent/iteration/main-loop.rkt"))

(test-case "W0 G1b characterization: non-streaming main loop currently has no auto-extraction hook"
  (check-false (string-contains? main-loop-source "maybe-auto-extract-after-response!")))

(test-case "W0 G1b characterization: streaming loop already imports auto-extraction hook"
  (define stream-source (file->string "agent/loop-stream.rkt"))
  (check-true (string-contains? stream-source "maybe-auto-extract-after-response!")))
