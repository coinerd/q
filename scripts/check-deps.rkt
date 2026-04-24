#!/usr/bin/env racket
#lang racket/base

;; scripts/check-deps.rkt — Static dependency verification for info.rkt.
;;
;; Scans all .rkt files for require forms, extracts external package names,
;; cross-references with info.rkt deps/build-deps, and reports gaps.
;;
;; Strategy: look for `name/subpath` tokens on require lines, then filter out:
;;   - Internal q modules (agent/, cli/, extensions/, llm/, runtime/, etc.)
;;   - Base packages (racket/, rackunit/, ffi/, net/, etc.)
;;   - Dynamic requires (readline/, tui/) with fallback stubs
;; What remains are undeclared external package dependencies.
;;
;; Usage:
;;   cd q/ && racket scripts/check-deps.rkt      # check deps
;;   cd q/ && racket scripts/check-deps.rkt -v   # verbose
;;
;; Exit 0 if complete, 1 if gaps found.

(require racket/file
         racket/list
         racket/string
         racket/set
         racket/port)

(define verbose? #f)
(for ([arg (in-vector (current-command-line-arguments))])
  (cond
    [(or (string=? arg "-v") (string=? arg "--verbose")) (set! verbose? #t)]
    [(not (string=? arg "")) (printf "Unknown flag: ~a~n" arg)]))

;; Packages bundled with "base" — no explicit dep needed
(define base-packages
  (set "racket" "rackunit" "ffi" "net" "raco" "setup" "compiler" "syntax" "json" "file" "pkg" "info"))

;; Internal q module prefixes — not external packages
(define internal-prefixes
  '("agent" "cli"
            "extensions"
            "interfaces"
            "llm"
            "runtime"
            "sandbox"
            "tools"
            "tui"
            "util"
            "scripts"
            "skills"
            "pkg"
            "benchmarks"
            "docs"
            "wiki"
            "collab"
            "helpers"
            "examples"))

;; Dynamically required with fallbacks — not hard deps
(define dynamic-packages (set "readline" "tui"))

;; Known external packages that may be used as bare names (no slash)
;; These are packages not in base that appear as (require pkgname)
(define known-external-bare '("quickcheck"))

(define (extract-external-packages content)
  ;; Find all name/subpath tokens in require forms
  (define slash-deps
    (for*/list ([line (in-list (string-split content "\n"))]
                #:when (or (regexp-match? #rx"[(]require" line)
                           (regexp-match? (regexp "^\\s+[a-zA-Z]") line))
                [tok (in-list (regexp-match* #rx"[a-zA-Z_][a-zA-Z0-9_-]*/[a-zA-Z0-9_.-]+" line))]
                #:when (let ([prefix (car (string-split tok "/"))])
                         (and (not (set-member? base-packages prefix))
                              (not (member prefix internal-prefixes))
                              (not (set-member? dynamic-packages prefix)))))
      tok))
  ;; Find bare name requires for known external packages
  ;; Scan entire file content, not just (require lines
  (define bare-deps
    (for/list ([pkg (in-list known-external-bare)]
               #:when (regexp-match? (pregexp (format "\\b~a\\b" pkg)) content))
      pkg))
  (remove-duplicates (append slash-deps bare-deps)))

(define (parse-info-deps)
  (define info-content (file->string "info.rkt"))
  (define (extract-from-section rx)
    (apply append
           (for/list ([m (in-list (regexp-match* rx info-content #:match-select values))])
             (define inside (cadr m))
             (map string-trim (regexp-match* #rx"[a-zA-Z][a-zA-Z0-9_-]+" inside)))))
  (append (extract-from-section #px"[(]define +deps +['(]([^)]*)[)]")
          (extract-from-section #px"[(]define +build-deps +['(]([^)]*)[)]")))

(define (main)
  (unless (file-exists? "info.rkt")
    (printf "ERROR: Run from the q/ directory (info.rkt not found).~n")
    (exit 1))

  (printf "=== Dependency Completeness Check ===~n~n")

  (define all-files
    (for/list ([f (in-directory ".")]
               #:when (and (file-exists? f)
                           (string-suffix? (path->string f) ".rkt")
                           (not (string-contains? (path->string f) "/compiled/"))
                           (not (string-contains? (path->string f) ".zo"))))
      f))
  (printf "Scanned: ~a .rkt files~n" (length all-files))

  ;; Extract unique external package top-level names
  (define used-packages
    (remove-duplicates
     (sort (for/list ([tok (in-list (remove-duplicates (apply append
                                                              (for/list ([f (in-list all-files)])
                                                                (extract-external-packages
                                                                 (file->string f))))))])
             (car (string-split tok "/")))
           string<?)))

  (when verbose?
    (printf "~nExternal packages used:~n")
    (for ([p (in-list used-packages)])
      (printf "  ~a~n" p)))

  (define declared (parse-info-deps))
  (printf "Declared deps: ~a~n"
          (string-join (if (null? declared)
                           '("(none)")
                           declared)
                       ", "))

  (define missing (filter (lambda (p) (not (member p declared))) used-packages))

  (printf "~n--- Results ---~n")
  (cond
    [(null? missing)
     (printf "All external packages declared in info.rkt. OK~n")
     (exit 0)]
    [else
     (printf "MISSING from info.rkt build-deps:~n")
     (for ([p (in-list missing)])
       (printf "  x ~a~n" p))
     (printf "~nFix: add to (build-deps '(...)) in info.rkt~n")
     (exit 1)]))

(main)
