#lang racket/base

;; test-metadata.rkt - Parse structured metadata from test file headers.
;;
;; Report-only: this module does NOT change test selection.
;; It reads @suite/@boundary/@speed/@mutates/@isolation/@timeout annotations
;; from the first N lines of test files.
;;
;; Grammar:
;;   ;; @suite runtime|tui|security|arch|extensions|workflows|fixtures|all
;;   ;; @boundary unit|contract|integration|workflow|e2e|io|macro|serialization
;;   ;; @speed fast|slow|perf
;;   ;; @mutates none|env|cwd|home|repo|quarantine|process|network
;;   ;; @isolation none|temp-dir|sandboxed-home|repo-serial|process-tree
;;   ;; @timeout <positive-integer-seconds>

(require racket/string
         racket/port
         racket/match
         racket/file)

(provide parse-test-metadata
         metadata?
         metadata-suite
         metadata-boundary
         metadata-speed
         metadata-mutates
         metadata-isolation
         metadata-timeout
         metadata-file
         metadata-warnings
         scan-files-metadata
         metadata-report)

;; ---------------------------------------------------------------------------
;; Struct
;; ---------------------------------------------------------------------------

(struct metadata (file suite boundary speed mutates isolation timeout warnings) #:transparent)

;; ---------------------------------------------------------------------------
;; Parsing
;; ---------------------------------------------------------------------------

(define max-scan-lines 30)

(define annotation-pattern
  ;; Match lines like: ;; @suite runtime
  ;; Captures the key and value
  #rx"^;+[ \t]*@([a-z]+)[ \t]+(.+)$")

(define known-keys '(suite boundary speed mutates isolation timeout))

(define (parse-annotation line)
  (define m (regexp-match annotation-pattern line))
  (and m
       (let ([key (string->symbol (cadr m))]
             [val (string-trim (caddr m))])
         (if (memq key known-keys)
             (cons key val)
             #f))))

(define (parse-test-metadata file-path)
  "Parse metadata annotations from the first N lines of a test file.
   Returns a metadata struct. Unknown annotations produce warnings."
  (define lines
    (with-handlers ([exn:fail? (lambda (_) '())])
      (call-with-input-file file-path
                            (lambda (in)
                              (for/list ([line (in-lines in)]
                                         [i (in-naturals)]
                                         #:break (>= i max-scan-lines))
                                line))
                            #:mode 'text)))
  (define annotations (filter values (map parse-annotation lines)))
  ;; Collect into fields
  (define (get-field key)
    (cdr (or (assoc key annotations) (cons key #f))))
  (define (validate-enum key val valid-set)
    (if (and val (not (member val valid-set)))
        (list (format "~a: invalid value '~a' (valid: ~a)" key val (string-join valid-set ", ")))
        '()))
  (define suite-val (get-field 'suite))
  (define boundary-val (get-field 'boundary))
  (define speed-val (get-field 'speed))
  (define mutates-val (get-field 'mutates))
  (define isolation-val (get-field 'isolation))
  (define timeout-val (get-field 'timeout))
  (define warnings
    (append
     (validate-enum 'suite
                    suite-val
                    '("runtime" "tui" "security" "arch" "extensions" "workflows" "fixtures" "all"))
     (validate-enum 'boundary
                    boundary-val
                    '("unit" "contract" "integration" "workflow" "e2e" "io" "macro" "serialization"))
     (validate-enum 'speed speed-val '("fast" "slow" "perf"))
     (validate-enum 'mutates
                    mutates-val
                    '("none" "env" "cwd" "home" "repo" "quarantine" "process" "network"))
     (validate-enum 'isolation
                    isolation-val
                    '("none" "temp-dir" "sandboxed-home" "repo-serial" "process-tree"))
     (if (and timeout-val (not (regexp-match? #rx"^[1-9][0-9]*$" timeout-val)))
         (list (format "timeout: invalid value '~a' (expected positive integer)" timeout-val))
         '())))
  (metadata file-path
            suite-val
            boundary-val
            speed-val
            mutates-val
            isolation-val
            (and timeout-val (string->number timeout-val))
            warnings))

;; ---------------------------------------------------------------------------
;; Scanning
;; ---------------------------------------------------------------------------

(define (scan-files-metadata files)
  "Scan a list of file paths and return a list of metadata structs."
  (for/list ([f (in-list files)])
    (parse-test-metadata f)))

(define (metadata-report metas #:port [port (current-output-port)])
  "Print a report of metadata scan results."
  (define tagged
    (filter (lambda (m) (or (metadata-suite m) (metadata-boundary m) (metadata-speed m))) metas))
  (define untagged
    (filter (lambda (m) (not (or (metadata-suite m) (metadata-boundary m) (metadata-speed m))))
            metas))
  (define all-warnings (apply append (map metadata-warnings metas)))
  (fprintf port
           ";; metadata scan: ~a files scanned, ~a tagged, ~a untagged~n"
           (length metas)
           (length tagged)
           (length untagged))
  (when (pair? all-warnings)
    (fprintf port ";; warnings:~n")
    (for ([w (in-list all-warnings)])
      (fprintf port ";;   ~a~n" w)))
  (when (pair? tagged)
    (fprintf port "~n;; tagged files:~n")
    (for ([m (in-list tagged)])
      (fprintf port
               ";;   ~a @suite=~a @boundary=~a @speed=~a @mutates=~a @isolation=~a @timeout=~a~n"
               (metadata-file m)
               (or (metadata-suite m) "-")
               (or (metadata-boundary m) "-")
               (or (metadata-speed m) "-")
               (or (metadata-mutates m) "-")
               (or (metadata-isolation m) "-")
               (or (metadata-timeout m) "-")))))
