#lang racket/base

;; extensions/package-audit.rkt -- Install-time package auditing for risk indicators
;;
;; Originally in util/ but relocated to extensions/ because it imports
;; from extensions/manifest.rkt, violating util's leaf-only contract (ARCH-03).
;; util/package-audit.rkt is now a thin re-export facade for backward compat.

(require racket/contract
         racket/port
         racket/string
         racket/list
         racket/file
         racket/match
         racket/path
         "./manifest.rkt"
         "../util/checksum.rkt")

;; ============================================================
;; Structs
;; ============================================================

(struct finding (severity message) #:transparent)
;; severity : (or/c 'low 'medium 'high 'info)
;; message  : string?

(struct audit-result (risk-level findings checksum-verified?) #:transparent)
;; risk-level        : (or/c 'low 'medium 'high)
;; findings          : (listof finding?)
;; checksum-verified? : boolean?

;; ============================================================
;; Provides
;; ============================================================

(provide
 (struct-out finding)
 (struct-out audit-result)
 (contract-out
  [audit-package        (-> path-string? audit-result?)]
  [scan-rkt-files       (-> path-string? (listof finding?))]
  [classify-risk        (-> (listof finding?) (or/c 'extension 'skill 'bundle) (or/c 'low 'medium 'high))]
  [format-audit-report  (-> audit-result? string?)]
  [format-audit-warning (-> audit-result? string?)]))

;; ============================================================
;; Pattern definitions
;; ============================================================

(define network-pattern
  #rx"require.*net/(http|url|headless)")

(define subprocess-pattern
  #rx"(subprocess|system|shell-execute|process)")

(define file-write-pattern
  #rx"(call-with-output-file|write-bytes|delete-file|delete-directory)")

(define file-read-pattern
  #rx"(file->string|call-with-input-file|read-bytes)")

(define eval-pattern
  #px"\\beval\\b|\\bcompile\\b")

(define dynamic-require-pattern
  #rx"dynamic-require")

(define ffi-pattern
  #rx"require.*ffi/")

(define env-modification-pattern
  #rx"(putenv|setenv|current-environment-variables)")

(define unsafe-io-pattern
  #rx"(read-byte|read-char|read\\b)")  ;; unchecked deserialization

;; ============================================================
;; scan-rkt-files : path-string? -> (listof finding?)
;; ============================================================

(define (scan-rkt-files dir)
  (define rkt-files
    (find-files (lambda (p)
                  (let ([ext (filename-extension p)])
                    (and ext (or (bytes=? ext #"rkt")
                                 (bytes=? ext #"zo")))))
                dir))
  (append*
   (for/list ([fpath (in-list rkt-files)])
     (scan-one-file fpath dir))))

(define (scan-one-file fpath base-dir)
  (define rel (path->string (find-relative-path base-dir fpath)))
  (define findings '())
  (define (add! sev msg)
    (set! findings (cons (finding sev (format "~a in ~a" msg rel)) findings)))
  ;; Check for .zo files (compiled bytecode)
  (define ext (filename-extension fpath))
  (when (and ext (bytes=? ext #"zo"))
    (add! 'high "compiled bytecode (.zo) — cannot audit"))
  (define content
    (with-handlers ([exn:fail? (lambda (_) "")])
      (call-with-input-file fpath port->string #:mode 'text)))
  (when (regexp-match? network-pattern content)
    (add! 'high "network access"))
  (when (regexp-match? subprocess-pattern content)
    (add! 'high "subprocess execution"))
  (when (regexp-match? file-write-pattern content)
    (add! 'medium "file write operations"))
  (when (regexp-match? file-read-pattern content)
    (add! 'low "file read operations"))
  (when (regexp-match? eval-pattern content)
    (add! 'high "eval/compile usage"))
  (when (regexp-match? dynamic-require-pattern content)
    (add! 'high "dynamic-require usage"))
  (when (regexp-match? ffi-pattern content)
    (add! 'high "FFI usage"))
  (when (regexp-match? env-modification-pattern content)
    (add! 'high "environment modification"))
  (when (regexp-match? unsafe-io-pattern content)
    (add! 'high "unsafe deserialization (read)"))
  (reverse findings))

;; ============================================================
;; classify-risk : (listof finding?) symbol -> symbol
;; ============================================================

(define (classify-risk findings pkg-type)
  ;; Baseline from package type
  (define baseline
    (case pkg-type
      [(extension) 'medium]
      [(bundle)    'medium]
      [(skill)     'low]))
  ;; Count findings by severity
  (define high-count   (count (lambda (f) (eq? (finding-severity f) 'high))   findings))
  (define medium-count (count (lambda (f) (eq? (finding-severity f) 'medium)) findings))
  ;; Escalation logic
  (cond
    [(> high-count 0) 'high]
    [(>= medium-count 2) 'high]
    [(and (eq? baseline 'medium) (> medium-count 0)) 'high]
    [else baseline]))

;; ============================================================
;; audit-package : path-string? -> audit-result?
;; ============================================================

(define (audit-package dir)
  (define qpm-path (build-path dir "qpm.json"))
  (define manifest (read-qpm-manifest qpm-path))
  (cond
    [(not manifest)
     ;; No manifest found
     (audit-result 'medium
                   (list (finding 'medium "no manifest found"))
                   #f)]
    [else
     ;; Scan .rkt files
     (define code-findings (scan-rkt-files dir))
     ;; Check entry point
     (define entry-findings
       (if (qpm-manifest-entry manifest)
           (list (finding 'info
                          (format "entry point: ~a" (qpm-manifest-entry manifest))))
           '()))
     ;; Checksum verification
     (define expected-checksum (qpm-manifest-checksum manifest))
     (define checksum-ok
       (if expected-checksum
           (verify-file-checksum qpm-path expected-checksum)
           #f))
     ;; Combine findings
     (define all-findings (append code-findings entry-findings))
     ;; Classify
     (define risk (classify-risk all-findings (qpm-manifest-type manifest)))
     (audit-result risk all-findings checksum-ok)]))

;; ============================================================
;; format-audit-report : audit-result? -> string?
;; ============================================================

(define (format-audit-report result)
  (define level (audit-result-risk-level result))
  (define findings (audit-result-findings result))
  (define checksum-ok (audit-result-checksum-verified? result))
  (define lines
    (list*
     (format "Package Audit Report")
     (format "  Risk Level: ~a" level)
     (format "  Checksum Verified: ~a" (if checksum-ok "yes" "no"))
     (format "  Findings (~a):" (length findings))
     (for/list ([f (in-list findings)])
       (format "    [~a] ~a" (finding-severity f) (finding-message f)))))
  (string-join lines "\n"))

;; ============================================================
;; format-audit-warning : audit-result? -> string?
;; ============================================================

(define (format-audit-warning result)
  (define level (audit-result-risk-level result))
  (define count (length (audit-result-findings result)))
  (format "⚠ Package risk level: ~a (~a finding~a)"
          level count (if (= count 1) "" "s")))
