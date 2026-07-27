#lang racket
;; @speed fast  ;; @suite arch
;; BOUNDARY: integration
;; tests/test-arch-tool-constructor.rkt — Raw tool constructor import guard (#15)
;;
;; Verifies that no external module imports the raw `tool` constructor
;; from tool-struct.rkt. Only tools/tool.rkt (via make-tool) is permitted
;; to construct tool structs directly.
;;
;; This test scans all Racket source files for direct imports of
;; (submod "tool-struct.rkt" internal) or the `tool` constructor symbol
;; outside of the tools/ directory.
;;
;; Refs: #15, v0.99.72 W0

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "helpers/arch-utils.rkt")

;; ══════════════════════════════════════════════════════════════════
;; R-15: Tool constructor must not be imported outside tools/
;; ══════════════════════════════════════════════════════════════════

(define q-dir
  ;; Determine q/ root directory
  (let ([here (current-directory)])
    (if (string-suffix? (path->string here) "tests")
        (build-path here "..")
        here)))

;; Collect all .rkt files recursively, excluding compiled/ and tmp/
(define (collect-rkt-files dir)
  (sort (with-handlers ([exn:fail? (lambda (_) '())])
          (for/list ([f (in-directory dir)]
                     #:when (and (string-suffix? (path->string f) ".rkt")
                                 (not (string-contains? (path->string f) "/compiled/"))
                                 (not (string-contains? (path->string f) "/tmp-"))
                                 (not (string-contains? (path->string f) "tmp-debug"))))
            (path->string f)))
        string<?))

;; Check a single file for dangerous imports
(define (check-file-for-tool-import fqpath q-dir-path)
  (define relative
    (let ([q-str (path->string q-dir-path)])
      (if (string-prefix? fqpath q-str)
          (substring fqpath (add1 (string-length q-str)))
          fqpath)))
  ;; Skip the authoritative construction file and the struct definition itself
  (cond
    [(string-contains? relative "tools/tool.rkt") #f]
    [(string-contains? relative "tools/tool-struct.rkt") #f]
    [(string-contains? relative "test-arch-tool-constructor") #f]
    [else
     (define content
       (with-handlers ([exn:fail? (lambda (_) "")])
         (file->string fqpath)))
     (cond
       [(regexp-match #rx"submod.*\"tool-struct\\.rkt\"\\s+internal" content)
        (format "~a imports (submod \"tool-struct.rkt\" internal)" relative)]
       [(regexp-match #rx"\"tool-struct\\.rkt\"[^)]*\\btool\b" content)
        (format "~a imports 'tool' identifier from tool-struct.rkt" relative)]
       [else #f])]))

;; ── Tests ─────────────────────────────────────────────────────────

(define tool-constructor-tests
  (test-suite "Tool constructor import guard (R-15, #15)"

    (test-case "No external module imports raw tool constructor"
      (define all-files (collect-rkt-files q-dir))
      (define violations
        (for/list ([f (in-list all-files)]
                   #:when (check-file-for-tool-import f q-dir))
          (check-file-for-tool-import f q-dir)))
      (if (null? violations)
          (check-true #t "No violations found — tool constructor is properly sealed")
          (begin
            (for ([v (in-list violations)])
              (printf "VIOLATION: ~a~n" v))
            (check-equal? (length violations)
                          0
                          (format "Found ~a modules importing raw tool constructor"
                                  (length violations))))))))

(define all-tests
  (test-suite "Architecture tool constructor tests (R-15)"
    tool-constructor-tests))

(module+ main
  (run-tests all-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
