#lang racket/base

;; q/scripts/run-tests/work-counters.rkt — opt-in dynamic work-type counters.
;;
;; v1.00.28 W0 runtime census (RUNTIME-AUDIT-SPEC-v1.00.28.md §2/§4.5):
;; project-owned fixture/process/git helpers call `q-work-count!` to record
;; work-type events (subprocess launches, git commands, temp directories,
;; session/git fixtures, worktrees, requested sleeps, timeouts).
;;
;; OPT-IN CONTRACT (diagnostic-only, per PLAN-v1.00.28 §3):
;;  - Counting is inactive unless the runtime census armed the process by
;;    leaving the marker file `<base-dir>/.q-census-counters` containing the
;;    census counters directory path. In every other execution context
;;    (plain `racket tests/x.rkt`, run-tests.rkt suites, CI) helpers pay a
;;    single `file-exists?` check at module load.
;;  - Test pass/fail semantics never depend on counters: `q-work-count!` is
;;    a no-op when unarmed, and dump failures are swallowed.
;;  - Categories not instrumented on a given test path serialize as
;;    `unknown` (JSON null) in the census, never `0`.

(require racket/file
         racket/format
         racket/path
         racket/string
         json)

(provide q-work-count!
         q-work-counters-armed?
         q-work-counters-dump!)

;; ---------------------------------------------------------------------------
;; Arming
;; ---------------------------------------------------------------------------

(define armed-dirs
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define candidates (list (find-system-path 'orig-dir) (find-system-path 'run-dir)))
    (for/list ([root (in-list candidates)]
               #:when (path? root)
               [p (in-value (build-path root ".q-census-counters"))]
               #:when (file-exists? p)
               [dir (in-value (string-trim (file->string p)))]
               #:when (and (non-empty-string? dir) (directory-exists? dir)))
      dir)))

(define armed? (pair? armed-dirs))

(define (q-work-counters-armed?)
  armed?)

;; ---------------------------------------------------------------------------
;; In-process counters: one JSON object {kind: n, ...} dumped per sample.
;; ---------------------------------------------------------------------------

(define counters
  (if armed?
      (make-hasheq)
      #f))

(define (q-work-count! kind [n 1])
  (when armed?
    (hash-update! counters kind (lambda (v) (+ v n)) 0)
    (void)))

;; Dump at process end (plumber flush hook below); diagnostic-only.
(define (q-work-counters-dump!)
  (when armed?
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (for ([dir (in-list armed-dirs)])
        (define out-file
          (build-path (string->path dir)
                      (string-append (~a (current-milliseconds))
                                     "-"
                                     (~a (equal-hash-code (current-thread)) #x10)
                                     ".json")))
        (call-with-output-file out-file
                               (lambda (out)
                                 (write-json (for/hash ([(k v) (in-hash counters)])
                                               (values (symbol->string k) v)))
                                 (newline out))
                               #:exists 'replace)))))

(when armed?
  (define plumb (current-plumber))
  (plumber-add-flush! plumb (lambda (_plumb) (q-work-counters-dump!))))
