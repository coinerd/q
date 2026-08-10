#lang racket/base

;; tests/test-gsd-facade-compat.rkt — GSD facade compatibility + purity sweep
;; @speed fast
;; @suite arch
;;
;; v0.99.89 W4 (#9230): "Reduce GSD facades to composition/re-export without
;; public API breakage." This test is the Kontrolle from the roadmap:
;;
;;   1. dynamic-require probes — the extension loader resolves extensions via
;;      (dynamic-require mod-path 'the-extension); we probe both the loader
;;      convention and every pinned export name of the two facades
;;      (gsd-planning.rkt, gsd/core.rkt) so future thinning cannot silently
;;      break the public surface.
;;   2. Export-surface pins — explicit name lists; each name must resolve.
;;   3. Pure-domain I/O-free sweep (Acceptance) — every inventory module in a
;;      pure domain (pure-planning / transition-logic / event-projection /
;;      command-parsing) with no declared effects must import NO I/O module.
;;   4. Pure-kernel dependency whitelists — transition-kernel, projection-
;;      kernel, command-parser may depend only on base/collections.

(require racket/string
         racket/path
         racket/match
         racket/file
         rackunit
         "../extensions/gsd/responsibility-inventory.rkt")

;; Repo root, robust to both `raco test` (cwd = tests/) and direct runs.
(define q-dir (if (file-exists? "main.rkt") "." ".."))

;; Absolute module path for dynamic-require probes (relative paths in
;; dynamic-require resolve from the current directory, not the test file).
;; Returns a path object (module-path? accepts paths, not strings).
(define (abs-mod path)
  (simplify-path (build-path q-dir path)))

;; ============================================================
;; require extraction (read-based, same as test-gsd-command-intent.rkt)
;; ============================================================

(define (spec-module-path spec)
  (cond
    [(symbol? spec) (symbol->string spec)]
    [(string? spec) spec]
    [(pair? spec)
     (case (car spec)
       [(only-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (cadr spec)
            #f)]
       ;; (prefix-in "pref" module-path): module path is the SECOND arg.
       [(prefix-in)
        (if (and (pair? (cdr spec)) (pair? (cddr spec)) (string? (caddr spec)))
            (caddr spec)
            #f)]
       [else #f])]
    [else #f]))

(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define port (open-input-string src))
    (define out '())
    (let loop ()
      (define datum (read port))
      (unless (eof-object? datum)
        (when (and (pair? datum) (eq? (car datum) 'require))
          (set! out (append out (cdr datum))))
        (loop)))
    (close-input-port port)
    out))

(define (module-imports path)
  (define reqs (extract-requires (path->string path)))
  (for/list ([spec (in-list reqs)]
             #:when (spec-module-path spec))
    (spec-module-path spec)))

;; Normalize an import to its module basename: "../../util/command-helpers.rkt"
;; → "command-helpers", "racket/file" → "file".
(define (normalize-import i)
  (define base (car (reverse (string-split i "/"))))
  (if (string-suffix? base ".rkt")
      (substring base 0 (- (string-length base) 4))
      base))

(define forbidden-io-imports
  '("file" "port"
           "path"
           "date"
           "system"
           "runtime-path"
           "openssl"
           "net"
           "tcp"
           "process"
           "subprocess"
           "sandbox"))

;; ============================================================
;; 1 + 2. dynamic-require probes and export-surface pins
;; ============================================================

;; Pinned public export surface of extensions/gsd-planning.rkt (post-W4).
(define gsd-planning-export-surface
  '(the-extension gsd-planning-extension
                  gsd-mode
                  gsd-mode?
                  set-gsd-mode!
                  pinned-planning-dir
                  set-pinned-planning-dir!
                  current-max-old-text-len
                  set-current-max-old-text-len!
                  completed-waves
                  total-waves
                  set-total-waves!
                  mark-wave-complete!
                  wave-complete?
                  next-pending-wave
                  current-wave-index
                  set-current-wave-index!
                  gsd-event-bus
                  set-gsd-event-bus!
                  emit-gsd-event!
                  gsd-session-cleanup
                  planning-system-prompt
                  planning-artifact-path
                  valid-artifact-name?
                  read-planning-artifact
                  write-planning-artifact!
                  handle-planning-read
                  handle-planning-write
                  planning-implement-prompt
                  gsd-tool-guard
                  reset-all-gsd-state!
                  parse-wave-headers))

;; Pinned public export surface of extensions/gsd/core.rkt.
(define gsd-core-export-surface
  '(gsd-commands gsd-command-result
                 gsd-command-result?
                 gsd-command-result-success
                 gsd-command-result-mode
                 gsd-command-result-message
                 gsd-command-result-data
                 gsd-ok
                 gsd-err
                 gsd-result?
                 gsd-success?
                 gsd-failed?
                 gsd-command-dispatch
                 gsd-write-guard
                 gsd-show-status
                 cmd-replan
                 cmd-skip
                 cmd-reset
                 cmd-done
                 cmd-wave-done
                 reset-all-gsd-state!
                 with-gsd-transaction))

(test-case "facade: dynamic-require loader convention (the-extension)"
  (define ext (dynamic-require (abs-mod "extensions/gsd-planning.rkt") 'the-extension))
  (check-not-false ext)
  (check-not-false (dynamic-require (abs-mod "extensions/gsd-planning.rkt") 'gsd-planning-extension)))

(test-case "facade: every pinned gsd-planning.rkt export dynamic-requires"
  (for ([name (in-list gsd-planning-export-surface)])
    (check-not-false (dynamic-require (abs-mod "extensions/gsd-planning.rkt") name)
                     (format "gsd-planning.rkt must provide ~a" name))))

(test-case "facade: every pinned gsd/core.rkt export dynamic-requires"
  (for ([name (in-list gsd-core-export-surface)])
    (check-not-false (dynamic-require (abs-mod "extensions/gsd/core.rkt") name)
                     (format "gsd/core.rkt must provide ~a" name))))

;; ============================================================
;; Legacy wrapper smoke (fresh parameterized ctx, no global mutation)
;; ============================================================

(test-case "facade: legacy wrappers smoke without crash"
  (define planning-path (abs-mod "extensions/gsd-planning.rkt"))
  (define gsd-mode (dynamic-require planning-path 'gsd-mode))
  (define set-gsd-mode! (dynamic-require planning-path 'set-gsd-mode!))
  (define pinned (dynamic-require planning-path 'pinned-planning-dir))
  (define total (dynamic-require planning-path 'total-waves))
  ;; Calling the wrappers must not raise (they read the default session ctx).
  (check-not-false (member (gsd-mode) '(#f planning plan-written executing verifying idle)))
  (check-true (or (not (pinned)) (path? (pinned))))
  (check-true (or (not (total)) (exact-nonnegative-integer? (total))))
  ;; set-gsd-mode! must not raise and must leave the machine consistent.
  (set-gsd-mode! 'planning)
  (check-not-false (member (gsd-mode) '(planning exploring executing))))

;; ============================================================
;; 3. Pure-domain I/O-free sweep (Acceptance)
;; ============================================================

(define pure-domains '(pure-planning transition-logic event-projection command-parsing))

(define pure-modules
  (for/list ([e (in-list inventory)]
             #:when (and (member (entry-domain e) pure-domains) (null? (entry-effects e))))
    (entry-module-file e)))

(test-case "purity: pure-domain inventory modules have no direct I/O imports"
  (check-true (pair? pure-modules) "inventory must classify at least one pure module")
  (for ([mod (in-list pure-modules)])
    (define path (build-path q-dir "extensions" "gsd" mod))
    (check-true (file-exists? path) (format "~a must exist" mod))
    (define imports (map normalize-import (module-imports path)))
    (define violations
      (for/list ([i (in-list imports)]
                 #:when (member i forbidden-io-imports))
        i))
    (check-equal? violations '() (format "~a imports I/O modules: ~a" mod violations))))

(test-case "purity: pure kernel dependency whitelists"
  (define whitelists
    '(("transition-kernel.rkt" "base" "match" "set")
      ("projection-kernel.rkt" "base" "string")
      ("command-parser.rkt" "base" "match" "string" "command-helpers" "command-types")))
  (for ([entry (in-list whitelists)])
    (match-define (list mod allowed ...) entry)
    (define path (build-path q-dir "extensions" "gsd" mod))
    (define imports (map normalize-import (module-imports path)))
    (define unexpected
      (for/list ([i (in-list imports)]
                 #:unless (member i allowed))
        i))
    (check-equal? unexpected '() (format "~a imports outside whitelist: ~a" mod unexpected))))

(module+ test
  (require rackunit/text-ui)
  (run-tests (test-suite "gsd-facade-compat"
               (test-case "pure-domain sweep covers the three kernels"
                 (check-not-false (member "transition-kernel.rkt" pure-modules))
                 (check-not-false (member "projection-kernel.rkt" pure-modules))
                 (check-not-false (member "command-parser.rkt" pure-modules))))))
