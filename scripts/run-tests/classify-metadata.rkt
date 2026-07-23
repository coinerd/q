#lang racket/base

;; q/scripts/run-tests/classify-metadata.rkt — File metadata parsing + base-dir resolution
;;
;; Extracted from classify.rkt in v0.99.58 W3-1 (P3-CL).
;; Shared infrastructure used by both classify.rkt (file collection)
;; and classify-filters.rkt (classification predicates).
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/file)

(provide base-dir
         q-root-candidate?
         resolve-base-dir
         metadata-cache
         clear-metadata-cache!
         metadata-tokens
         metadata-bool
         metadata-line-match
         get-file-metadata)

;; ============================================================
;; Base directory resolution
;; ============================================================

(define (q-root-candidate? p)
  (and (directory-exists? (build-path p "tests"))
       (file-exists? (build-path p "scripts" "run-tests.rkt"))))

(define (resolve-base-dir orig)
  (define parent (simplify-path (build-path orig "..")))
  (define candidates
    (list (simplify-path (build-path orig "q")) (simplify-path (build-path parent "q")) orig parent))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (q-root-candidate? candidate))
        candidate)
      orig))

(define base-dir (resolve-base-dir (find-system-path 'orig-dir)))

;; ============================================================
;; Metadata parser
;; ============================================================

(define metadata-cache (make-hash))

(define (clear-metadata-cache!)
  (hash-clear! metadata-cache))

(define (metadata-tokens raw)
  (filter (lambda (s) (not (string=? s ""))) (regexp-split #rx"[ ,\t]+" (string-trim raw))))

(define (metadata-bool raw default)
  (define normalized (string-downcase (string-trim raw)))
  (cond
    [(string=? normalized "") default]
    [(member normalized '("true" "yes" "1" "on")) #t]
    [(member normalized '("false" "no" "0" "off")) #f]
    [else default]))

(define (metadata-line-match line tag)
  (define pattern (pregexp (format "@~a(?:[[:space:]]+([^;]*))?" (regexp-quote tag))))
  (define m (regexp-match pattern line))
  (and m (list line (string-trim (or (cadr m) "")))))

(define (get-file-metadata f)
  (hash-ref!
   metadata-cache
   f
   (lambda ()
     (define full-path
       (if (absolute-path? f)
           f
           (build-path base-dir f)))
     (cond
       [(not (file-exists? full-path)) (hash)]
       [else
        (define speed #f)
        (define suite #f)
        (define suites '())
        (define requires '())
        (define not-test? #f)
        (define mutates #f)
        (define boundary #f)
        (define isolation #f)
        (define timeout #f)
        (with-handlers ([exn:fail? (lambda (_) (void))])
          (call-with-input-file full-path
                                (lambda (port)
                                  (for ([_ (in-range 50)]
                                        #:break (eof-object? (peek-byte port)))
                                    (define line (read-line port))
                                    (when (string? line)
                                      (define speed-match (metadata-line-match line "speed"))
                                      (when speed-match
                                        (define toks (metadata-tokens (cadr speed-match)))
                                        (when (pair? toks)
                                          (set! speed (string->symbol (car toks)))))
                                      (define suite-match (metadata-line-match line "suite"))
                                      (when suite-match
                                        (set! suites (metadata-tokens (cadr suite-match)))
                                        (set! suite (and (pair? suites) (car suites))))
                                      (define requires-match (metadata-line-match line "requires"))
                                      (when requires-match
                                        (set! requires (metadata-tokens (cadr requires-match))))
                                      (define not-test-match (metadata-line-match line "not-test"))
                                      (when not-test-match
                                        (set! not-test? (metadata-bool (cadr not-test-match) #t)))
                                      (define mutates-match (metadata-line-match line "mutates"))
                                      (when mutates-match
                                        (set! mutates (string-trim (cadr mutates-match))))
                                      (define boundary-match (metadata-line-match line "boundary"))
                                      (when boundary-match
                                        (set! boundary (string-trim (cadr boundary-match))))
                                      (define isolation-match (metadata-line-match line "isolation"))
                                      (when isolation-match
                                        (set! isolation (string-trim (cadr isolation-match))))
                                      (define timeout-match
                                        (regexp-match #rx";+[ \\t]*@timeout[ \\t]+([0-9]+)" line))
                                      (when timeout-match
                                        (set! timeout (string->number (cadr timeout-match)))))))))
        (hash 'speed
              speed
              'suite
              suite
              'suites
              suites
              'requires
              requires
              'not-test?
              not-test?
              'mutates
              mutates
              'boundary
              boundary
              'isolation
              isolation
              'timeout
              timeout)]))))
