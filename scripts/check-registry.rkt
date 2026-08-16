#!/usr/bin/env racket
#lang racket
;;; check-registry.rkt — bug-registry self-healing checker (BUG-0013)
;;;
;;; Recomputes the derived lines of .planning/bugs/INDEX.md from the actual
;;; row files instead of trusting hand-edited totals, and validates registry
;;; invariants. Part of the planning concurrency protocol documented in
;;; .planning/bugs/README.md:
;;;   - counts / next-free-ID are ALWAYS recomputed by this script (never hand-edited)
;;;   - before committing a registry change, re-run this script after re-fetch
;;
;;; Usage:
;;;   racket scripts/check-registry.rkt [--registry <dir>] [--check] [--quiet]
;;;
;;;   --registry  registry directory (default: <repo>/.planning/bugs, resolved
;;;               from the script location, else ./planning/bugs)
;;;   --check     report-only: never rewrite the INDEX (CI mode)
;;;
;;; Exit codes: 0 = registry consistent (nothing to fix, all invariants hold)
;;;             1 = drift found (healed unless --check) or invariant violated

(require racket/file
         racket/string
         racket/list
         racket/format)

;; ---------------------------------------------------------------- helpers

(define verbose? #t)

(define (log/ fmt . args)
  (when verbose? (apply printf fmt args)))

(define (->id s)
  (string->number (substring s 4)))

(define (id->s n) (format "BUG-~a" (~a n #:align 'right #:pad-string "0" #:width 4)))

;; ---------------------------------------------------------- arg parsing

(define-values (registry-dir check-only?)
  (let loop ([args (vector->list (current-command-line-arguments))]
             [dir #f] [check #f])
    (cond
      [(null? args)
       (define d (or dir
                     (for/or ([cand (in-list
                                     (filter values
                                             (list (build-path (path-only (find-system-path 'run-file)) 'up ".planning" "bugs")
                                                   (build-path (current-directory) ".planning" "bugs"))))])
                       (and (directory-exists? cand) cand))))
       (unless d
         (eprintf "check-registry: no .planning/bugs found; pass --registry <dir>~n")
         (exit 2))
       (values d check)]
      [(member (car args) '("--registry" "-r"))
       (loop (cddr args) (path->complete-path (cadr args)) check)]
      [(member (car args) '("--check" "-n")) (loop (cdr args) dir #t)]
      [(member (car args) '("--quiet" "-q"))
       (set! verbose? #f) (loop (cdr args) dir check)]
      [(member (car args) '("--help" "-h"))
       (printf "usage: check-registry.rkt [--registry <dir>] [--check] [--quiet]~n")
       (exit 0)]
      [else (eprintf "check-registry: unknown argument ~a~n" (car args)) (exit 2)])))

;; ------------------------------------------------------------ parsing

;; INDEX row: | ID | date | title | component | severity | status | fixed-in | [file](file) |
(define row-rx
  #px"^\\|\\s*(BUG-\\d{4})\\s*\\|([^|]*)\\|([^|]*)\\|([^|]*)\\|([^|]*)\\|\\s*([a-z-]+)\\s*\\|([^|]*)\\|\\s*\\[[^\\]]*\\]\\(([^)]+)\\)\\s*\\|")

(define next-free-rx #px"^\\*\\*Next free ID:\\s*(BUG-\\d{4})\\*\\*")
(define counts-rx #px"^\\*\\*Open:\\s*(\\d+)([^*]*)Fixed/validated:\\s*(\\d+)([^*]*)Terminal \\(wontfix/dup/deferred\\):\\s*(\\d+)\\*\\*")
(define archived-rx #px"^\\*\\*Archived \\(closed\\):\\s*(\\d+)\\s*→")

(struct row (line id date title component severity status fixed-in file) #:transparent)

(define (parse-index path)
  (define rows '())
  (define derived '())       ; (cons line-idx (list kind old-value))
  (define lines (file->lines path))
  (for ([l (in-list lines)] [i (in-naturals)])
    (cond [(regexp-match row-rx l)
           => (lambda (m)
                (set! rows (cons (row l (list-ref m 1) (list-ref m 2) (list-ref m 3)
                                  (list-ref m 4) (list-ref m 5) (string-trim (list-ref m 6))
                                  (string-trim (list-ref m 7)) (list-ref m 8))
                              rows)))]
          [(regexp-match next-free-rx l)
           => (lambda (m) (set! derived (cons (list i 'next-free (list-ref m 1)) derived)))]
          [(regexp-match counts-rx l)
           => (lambda (m)
                (set! derived (cons (list i 'counts (list-tail m 1)) derived)))]
          [(regexp-match archived-rx l)
           => (lambda (m) (set! derived (cons (list i 'archived (list-ref m 1)) derived)))]))
  (values (reverse rows) (reverse derived) lines))

;; ------------------------------------------------------------- status sets

(define open-statuses '("reported" "triaged" "in-progress"))
(define fixed-statuses '("fixed" "validated"))
(define terminal-statuses '("wontfix" "duplicate" "deferred"))

(define (status-class s)
  (cond [(member s open-statuses) 'open]
        [(member s fixed-statuses) 'fixed]
        [(member s terminal-statuses) 'terminal]
        [else 'unknown]))

;; ------------------------------------------------------------- main check

(define problems '())    ; strings — invariant violations (never auto-healed)
(define fixes '())       ; strings — derived-line drift (healed unless --check)

(define (problem! fmt . args) (set! problems (cons (apply format fmt args) problems)))
(define (fix! fmt . args) (set! fixes (cons (apply format fmt args) fixes)))

(define index-path (build-path registry-dir "INDEX.md"))
(define archive-path (build-path registry-dir "archive" "ARCHIVE-INDEX.md"))

(unless (file-exists? index-path)
  (eprintf "check-registry: INDEX.md not found in ~a~n" registry-dir) (exit 2))

(define-values (rows derived lines) (parse-index index-path))

(log/ "registry: ~a  (~a rows)~n" registry-dir (length rows))

;; --- duplicate IDs -------------------------------------------------------
(define seen-ids '())
(for ([r (in-list rows)])
  (when (member (row-id r) seen-ids)
    (problem! "duplicate row for ~a" (row-id r)))
  (set! seen-ids (cons (row-id r) seen-ids)))

;; --- unknown status values ----------------------------------------------
(for ([r (in-list rows)])
  (when (eq? (status-class (row-status r)) 'unknown)
    (problem! "~a: unknown status '~a' in INDEX row" (row-id r) (row-status r))))

;; --- referenced files exist; row/file status agree -----------------------
(define file-status-rx #px"^[-*]\\s*\\*\\*Status:\\*\\*\\s*([a-z-]+)")
(for ([r (in-list rows)])
  (define f (build-path registry-dir (row-file r)))
  (if (file-exists? f)
      (let ([m (for/or ([l (in-list (file->lines f))]) (regexp-match file-status-rx l))])
        (cond [m (define st (cadr m))
               (unless (string=? st (row-status r))
                 (problem! "~a: INDEX says '~a' but report file says '~a'"
                           (row-id r) (row-status r) st))]
              [else (problem! "~a: report file has no '**Status:**' line" (row-id r))]))
      (problem! "~a: referenced report file missing: ~a" (row-id r) (row-file r))))

;; --- unindexed report files ----------------------------------------------
(define (bug-files-in dir)
  (if (directory-exists? dir)
      (filter (lambda (f) (regexp-match? #px"^BUG-\\d{4}-.+\\.md$" f))
              (map path->string (directory-list dir)))
      '()))
(define indexed-files (list->set (map row-file rows)))
(for ([f (in-list (bug-files-in registry-dir))])
  (unless (set-member? indexed-files f)
    (problem! "report file ~a exists but has no INDEX row" f)))

;; --- archive rows excluded from open counts ------------------------------
(define archive-rows
  (if (file-exists? archive-path)
      (let-values ([(rs _s _n) (parse-index archive-path)]) rs)
      '()))
(for ([r (in-list archive-rows)])
  (when (eq? (status-class (row-status r)) 'open)
    (problem! "~a: archived row still marked open ('~a')" (row-id r) (row-status r))))

;; --- recompute derived values -------------------------------------------
(define open-rows (filter (lambda (r) (eq? (status-class (row-status r)) 'open)) rows))
(define fixed-rows (filter (lambda (r) (eq? (status-class (row-status r)) 'fixed)) rows))
(define term-rows (filter (lambda (r) (eq? (status-class (row-status r)) 'terminal)) rows))

(define all-ids
  (append (map (compose ->id row-id) rows)
          (map (compose ->id row-id) archive-rows)
          (map (lambda (f) (->id (car (regexp-match #px"BUG-\\d{4}" f))))
               (append (bug-files-in registry-dir)
                       (bug-files-in (build-path registry-dir "archive"))))))
(define computed-next-free (id->s (add1 (apply max 0 all-ids))))

(define all-in-progress?
  (and (pair? open-rows) (andmap (lambda (r) (string=? (row-status r) "in-progress")) open-rows)))
(define computed-counts-line
  (format "**Open: ~a~a · Fixed/validated: ~a · Terminal (wontfix/dup/deferred): ~a**"
          (length open-rows)
          (if all-in-progress? " (all in-progress)" "")
          (length fixed-rows)
          (length term-rows)))
(define computed-archived (length archive-rows))

;; --- compare with what the INDEX claims; heal ----------------------------
(define new-lines (list->vector lines))

(for ([d (in-list derived)])
  (match-define (list-rest i kind old) d)
  (case kind
    [(next-free)
     (define old-id (car old))
     (unless (string=? old-id computed-next-free)
       (fix! "Next free ID: ~a → ~a" old-id computed-next-free)
       (vector-set! new-lines i (format "**Next free ID: ~a**" computed-next-free)))]
    [(counts)
     (define old-line (vector-ref new-lines i))
     (unless (string=? (string-normalize-spaces old-line) (string-normalize-spaces computed-counts-line))
       (fix! "counts line: \"~a\" → \"~a\"" old-line computed-counts-line)
       (vector-set! new-lines i computed-counts-line))]
    [(archived)
     (define old-n (string->number (car old)))
     (unless (= old-n computed-archived)
       (fix! "Archived (closed): ~a → ~a" old-n computed-archived)
       (vector-set! new-lines i
                    (regexp-replace archived-rx (vector-ref new-lines i)
                                    (format "**Archived (closed): ~a →" computed-archived))))]))

(when (and (pair? fixes) (not check-only?))
  (call-with-output-file index-path
    #:exists 'truncate
    (lambda (o) (for ([l (in-vector new-lines)]) (displayln l o)))))

;; --------------------------------------------------------------- report

(for ([f (in-list (reverse fixes))])
  (log/ "  drift: ~a~n" f))
(when (and (pair? fixes) check-only?)
  (log/ "  (--check: not rewritten; drop --check to heal)~n"))
(for ([p (in-list (reverse problems))])
  (log/ "  ERROR: ~a~n" p))

(log/ "next free: ~a · open ~a · fixed/validated ~a · terminal ~a · archived ~a~n"
      computed-next-free (length open-rows) (length fixed-rows) (length term-rows) computed-archived)

(cond [(or (pair? problems) (pair? fixes))
       (printf "REGISTRY: ~a — ~a drift, ~a error(s)~n"
               (if check-only? "INCONSISTENT (check-only)" "INCONSISTENT (healed where derivable)")
               (length fixes) (length problems))
       (exit 1)]
      [else (printf "REGISTRY: consistent~n") (exit 0)])
