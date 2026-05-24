#lang racket

;; scripts/hotspot-report.rkt — Architecture hotspot report
;;
;; Computes hotspot scores for source files using:
;;   score = change-frequency × LOC
;;
;; High scores indicate files that change often AND are large —
;; high-risk targets for decomposition or stabilization.
;;
;; Usage:
;;   racket scripts/hotspot-report.rkt              # top 20 hotspots
;;   racket scripts/hotspot-report.rkt --all        # all source files
;;   racket scripts/hotspot-report.rkt --ci         # CI mode: warn on high scores
;;   racket scripts/hotspot-report.rkt --json       # JSON output

(require racket/port
         racket/string
         racket/list
         racket/match
         racket/system
         json)

;; ── Configuration ──

(define TOP-N 20)
(define CI-WARN-THRESHOLD 5000)
(define Q-DIR
  (simplify-path (if (getenv "Q_DIR")
                     (string->path (getenv "Q_DIR"))
                     (build-path (current-directory) ".." "q"))))

;; ── Change frequency via git ──

(define (git-change-counts)
  ;; Returns hash: relative-path → count of commits touching that file
  (define git-result
    (with-handlers ([exn:fail? (λ (e) #f)])
      (define-values (sp out in err)
        (subprocess #f #f #f (find-executable-path "git") "log" "--format=" "--name-only"))
      (close-output-port in)
      (define text (port->string out))
      (close-input-port out)
      (close-input-port err)
      (subprocess-wait sp)
      (define code (subprocess-status sp))
      (if (= code 0) text #f)))
  (define h (make-hash))
  (when git-result
    (for ([line (in-list (string-split git-result "\n"))])
      (define trimmed (string-trim line))
      (when (> (string-length trimmed) 0)
        (hash-update! h trimmed add1 0))))
  h)

;; ── LOC counting ──

(define (rkt-source-files)
  ;; All .rkt files under Q-DIR, excluding tests/examples/benchmarks
  (define all
    (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
            (find-files (λ (p) (file-exists? p)) Q-DIR)))
  (define abs-q-dir (simplify-path Q-DIR))
  (filter (λ (f)
            (define rel (path->string (find-relative-path abs-q-dir f)))
            (not (or (string-prefix? rel "tests/")
                     (string-prefix? rel "examples/")
                     (string-prefix? rel "benchmarks/"))))
          all))

(define (line-count filepath)
  (with-handlers ([exn:fail? (λ (e) 0)])
    (length (string-split (file->string filepath) "\n"))))

;; ── Scoring ──

(define (compute-hotspots)
  (define change-counts (git-change-counts))
  (define abs-q-dir (simplify-path Q-DIR))
  (define files (rkt-source-files))
  (for/list ([f (in-list files)])
    (define rel (path->string (find-relative-path abs-q-dir f)))
    (define loc (line-count f))
    (define freq (hash-ref change-counts rel 0))
    (define score (* freq loc))
    (list rel loc freq score)))

(define (hotspots-by-score)
  (sort (compute-hotspots) > #:key fourth))

;; ── Reporting ──

(define (print-hotspot-table entries)
  (printf "~a   ~a   ~a   ~a~n"
          (string-pad-right "File" 50)
          (string-pad-left "LOC" 7)
          (string-pad-left "Freq" 7)
          (string-pad-left "Score" 10))
  (printf "~a   ~a   ~a   ~a~n"
          (make-string 50 #\-)
          (make-string 7 #\-)
          (make-string 7 #\-)
          (make-string 10 #\-))
  (for ([e (in-list entries)])
    (printf "~a   ~a   ~a   ~a~n"
            (string-pad-right (first e) 50)
            (string-pad-left (number->string (second e)) 7)
            (string-pad-left (number->string (third e)) 7)
            (string-pad-left (number->string (fourth e)) 10))))

(define (string-pad-right s n)
  (define len (string-length s))
  (if (>= len n)
      (substring s 0 n)
      (string-append s (make-string (- n len) #\space))))

(define (string-pad-left s n)
  (define len (string-length s))
  (if (>= len n)
      s
      (string-append (make-string (- n len) #\space) s)))

(define (print-report entries top-n)
  (printf "=== Architecture Hotspot Report ===~n~n")
  (printf "Score = Change Frequency × LOC~n")
  (printf "High scores indicate high-risk files (frequently changed + large).~n~n")
  (define shown (take entries (min top-n (length entries))))
  (print-hotspot-table shown)
  (printf "~nTotal source files: ~a~n" (length entries))
  (printf "Showing top ~a of ~a~n" (min top-n (length entries)) (length entries)))

(define (ci-warn entries threshold)
  (define hot (filter (λ (e) (> (fourth e) threshold)) entries))
  (for ([e (in-list hot)])
    (printf "  ⚠ ~a: score ~a (freq=~a × loc=~a)~n" (first e) (fourth e) (third e) (second e)))
  (when (> (length hot) 0)
    (printf "~n⚠ ~a files exceed score threshold ~a~n" (length hot) threshold)))

;; ── JSON output ──

(define (entries->json entries)
  (for/list ([e (in-list entries)])
    (hash 'file (first e) 'loc (second e) 'frequency (third e) 'score (fourth e))))

;; ── Main ──

(define args (vector->list (current-command-line-arguments)))
(define show-all? (member "--all" args))
(define ci-mode? (member "--ci" args))
(define json-mode? (member "--json" args))

(define entries (hotspots-by-score))

(cond
  [json-mode?
   (write-json (hash 'hotspots
                     (entries->json (if show-all?
                                        entries
                                        (take entries (min TOP-N (length entries)))))))]
  [else
   (print-report entries
                 (if show-all?
                     (length entries)
                     TOP-N))
   (when ci-mode?
     (ci-warn entries CI-WARN-THRESHOLD))])
