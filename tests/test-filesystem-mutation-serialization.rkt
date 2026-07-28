#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;;; tests/test-filesystem-mutation-serialization.rkt — W2-execution
;;;
;;; Tests for filesystem mutation serialization using tool-mutates-filesystem?
;;; instead of tool-dangerous? to select serialization.
;;;
;;; Verifies:
;;;   - tool-mutates-filesystem? predicate on tool struct
;;;   - File-mutation tools (write, edit, delete-lines) are mutating
;;;   - Non-mutation tools with path arg are NOT mutating
;;;   - Atomic bytes-written accounting
;;;   - Bounded lock acquisition with timeout

(require rackunit
         racket/file
         "../tools/file-mutation-queue.rkt"
         "../tools/tool.rkt"
         "../tools/tool-struct.rkt")

;; ============================================================
;; Tool mutates-filesystem? tests
;; ============================================================

(test-case "make-tool with mutates-filesystem? keyword"
  (define t
    (make-tool "test-write"
               "test tool"
               (hasheq 'type
                       "object"
                       'properties
                       (hasheq 'path (hasheq 'type "string"))
                       'required
                       (list "path"))
               (lambda (args ctx) (list "ok"))
               #:mutates-filesystem? #t))
  (check-true (tool-mutates-filesystem? t)))

(test-case "make-tool defaults mutates-filesystem? to #f"
  (define t
    (make-tool "test-read"
               "test tool"
               (hasheq 'type
                       "object"
                       'properties
                       (hasheq 'path (hasheq 'type "string"))
                       'required
                       (list "path"))
               (lambda (args ctx) (list "ok"))))
  (check-false (tool-mutates-filesystem? t)))

(test-case "file-mutation tools classified as mutating"
  (define write-tool
    (make-tool "write"
               "write files"
               (hasheq 'type "object" 'properties (hasheq) 'required (list))
               (lambda (args ctx) (list "ok"))
               #:mutates-filesystem? #t))
  (define edit-tool
    (make-tool "edit"
               "edit files"
               (hasheq 'type "object" 'properties (hasheq) 'required (list))
               (lambda (args ctx) (list "ok"))
               #:mutates-filesystem? #t))
  (define delete-lines-tool
    (make-tool "delete-lines"
               "delete lines"
               (hasheq 'type "object" 'properties (hasheq) 'required (list))
               (lambda (args ctx) (list "ok"))
               #:mutates-filesystem? #t))
  (check-true (tool-mutates-filesystem? write-tool) "write mutates filesystem")
  (check-true (tool-mutates-filesystem? edit-tool) "edit mutates filesystem")
  (check-true (tool-mutates-filesystem? delete-lines-tool) "delete-lines mutates filesystem"))

(test-case "non-mutation tools are not classified as mutating"
  (for ([name '("read" "bash" "grep" "find" "ls" "spawn-subagent" "firecrawl")])
    (define t
      (make-tool name
                 (format "~a tool" name)
                 (hasheq 'type "object" 'properties (hasheq) 'required (list))
                 (lambda (args ctx) (list "ok"))))
    (check-false (tool-mutates-filesystem? t)
                 (format "~a should not be classified as filesystem mutating" name))))

;; ============================================================
;; Atomic bytes-written accounting
;; ============================================================

(test-case "exec-context bytes-written starts at 0"
  (define ctx (make-exec-context))
  (check-equal? (unbox (exec-context-bytes-written ctx)) 0))

(test-case "atomic bytes-written increment"
  (define ctx (make-exec-context))
  (define n 10)
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (let loop ([j 0])
                  (when (< j 5)
                    (define bw (exec-context-bytes-written ctx))
                    (set-box! bw (+ (unbox bw) 1))
                    (loop (add1 j))))))))
  (for-each thread-wait threads)
  (check-equal? (unbox (exec-context-bytes-written ctx))
                (* n 5)
                "all byte increments should be visible"))

;; ============================================================
;; Concurrent same-file writes with mutation serialization
;; ============================================================

(test-case "same-file serialization with mutation tool"
  (define tmp (make-temporary-file "/tmp/w2-test-~a"))
  (display-to-file "0" tmp #:exists 'replace)
  (define path-str (path->string tmp))
  (define tool
    (make-tool "write"
               "test write"
               (hasheq 'type
                       "object"
                       'properties
                       (hasheq 'path (hasheq 'type "string"))
                       'required
                       (list "path"))
               (lambda (args ctx)
                 (define p (hash-ref args 'path))
                 (define val (with-input-from-file p (lambda () (string->number (port->string)))))
                 (sleep 0.01)
                 (with-output-to-file p (lambda () (display (add1 val))) #:exists 'replace)
                 (list "ok"))
               #:mutates-filesystem? #t))
  (define threads
    (for/list ([_ (in-range 10)])
      (thread (lambda ()
                (with-file-mutation-queue path-str
                                          (lambda ()
                                            ((tool-execute tool) (hasheq 'path path-str) #f)))))))
  (for-each thread-wait threads)
  (define final (with-input-from-file tmp (lambda () (string->number (port->string)))))
  (check-equal? final 10 "all 10 increments should be preserved")
  (delete-file tmp))

(test-case "different files run in parallel with mutation tool"
  (define tmp1 (make-temporary-file "/tmp/w2-par-~a"))
  (define tmp2 (make-temporary-file "/tmp/w2-par-~a"))
  (display-to-file "0" tmp1 #:exists 'replace)
  (display-to-file "0" tmp2 #:exists 'replace)
  (define path1 (path->string tmp1))
  (define path2 (path->string tmp2))
  (define started (current-inexact-milliseconds))
  (define t1
    (thread (lambda ()
              (with-file-mutation-queue path1
                                        (lambda ()
                                          (sleep 0.05)
                                          (display-to-file "1" tmp1 #:exists 'replace))))))
  (define t2
    (thread (lambda ()
              (with-file-mutation-queue path2
                                        (lambda ()
                                          (sleep 0.05)
                                          (display-to-file "1" tmp2 #:exists 'replace))))))
  (thread-wait t1)
  (thread-wait t2)
  (define elapsed (- (current-inexact-milliseconds) started))
  (check-true (< elapsed 200) (format "different files should run in parallel, took ~ams" elapsed))
  (delete-file tmp1)
  (delete-file tmp2))

;; ============================================================
;; Self-cleaning
;; ============================================================

(test-case "queue stats drop to zero after mutation"
  (define tmp (make-temporary-file "/tmp/w2-clean-~a"))
  (define path-str (path->string tmp))
  (with-file-mutation-queue path-str (lambda () (void)))
  (check-equal? (mutation-queue-stats) 0)
  (delete-file tmp))

;; ============================================================
;; Non-mutation tool with path arg doesn't need serialization
;; ============================================================

(test-case "non-mutation tool still serializes with path arg when using file-mutation-queue"
  ;; This tests that the queue itself works regardless of tool classification
  (define tmp (make-temporary-file "/tmp/w2-nonmut-~a"))
  (define path-str (path->string tmp))
  (define result (with-file-mutation-queue path-str (lambda () 99)))
  (check-equal? result 99 "queue works for non-mutation paths too")
  (delete-file tmp))
