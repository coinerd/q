#lang racket/base

;; Bounded, side-effect-safe reader validation for proposed Racket source.
;; Reader extensions execute code while `read-syntax` runs, so validation uses
;; a dedicated custodian, memory/deadline limits, a fresh namespace, and a
;; security guard that denies writes, execution, links, and network access.

(require racket/async-channel
         racket/path
         racket/port
         syntax-color/racket-lexer
         (only-in "error/error-sanitizer.rkt" sanitize-error-message))

(provide racket-source-path?
         validate-proposed-racket-source
         current-racket-parse-timeout-ms
         current-racket-parse-memory-limit-mb
         racket-edit-balance-warning)

(define current-racket-parse-timeout-ms (make-parameter 1000))
(define current-racket-parse-memory-limit-mb (make-parameter 64))
(define racket-source-extension-rx #px"(?i:[.](?:rkt|rktl|scrbl|rktd))$")

(define (racket-source-path? path)
  (regexp-match? racket-source-extension-rx
                 (if (path? path)
                     (path->string path)
                     path)))

(define (validation-error path detail)
  (format
   "Edit rejected: proposed content would leave ~a unparseable (read-syntax: ~a). The original file was left unchanged."
   path
   (sanitize-error-message detail)))

(define trusted-languages
  '("racket" "racket/base"
             "typed/racket"
             "typed/racket/base"
             "scribble/manual"
             "scribble/base"
             "scribble/doc"
             "info"
             "setup/infotab"))

;; `read-syntax` must enable reader support for #lang, but arbitrary #reader
;; modules execute attacker-controlled code in the host process. Lex first and
;; reject custom reader directives and non-whitelisted languages before any
;; reader module runs. The lexer distinguishes directives from strings/comments.
(define (reader-policy-error content)
  (define in (open-input-string content))
  (let loop ()
    (define-values (lexeme type _paren _start _end) (racket-lexer in))
    (cond
      [(eq? type 'eof) #f]
      [(equal? lexeme "#reader") "custom reader directives are not allowed during edit validation"]
      [(and (string? lexeme) (regexp-match #px"^#lang\\s+([^\\s]+)" lexeme))
       =>
       (lambda (match-result)
         (define language (cadr match-result))
         (if (member language trusted-languages)
             (loop)
             (format "custom reader language ~a is not allowed during edit validation" language)))]
      [else (loop)])))

(define (reader-security-guard)
  (make-security-guard (current-security-guard)
                       (lambda (who path modes)
                         (when (ormap (lambda (mode) (memq mode '(write delete execute))) modes)
                           (error who "reader validation denied filesystem mutation: ~a" path)))
                       (lambda (who host port mode)
                         (error who "reader validation denied network access to ~a:~a" host port))
                       (lambda (who path target)
                         (error who "reader validation denied link creation: ~a" path))))

(define (read-all-syntax path content)
  (define in (open-input-string content))
  (port-count-lines! in)
  (parameterize ([read-accept-reader #t]
                 [current-directory (or (path-only (path->complete-path path)) (current-directory))]
                 [current-namespace (make-base-empty-namespace)]
                 [current-security-guard (reader-security-guard)]
                 [exit-handler
                  (lambda (status)
                    (error 'read-syntax "reader attempted to exit with status ~a" status))]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (let loop ()
      (define form (read-syntax path in))
      (unless (eof-object? form)
        (loop)))))

;; --------------------------------------------------
;; Balance guard: fast string-aware paren-depth heuristic.
;; Returns #f when old-text and new-text have the same S-expression
;; depth delta, or a warning string when the delta is nonzero for a
;; Racket-family file. The W0 parse check is the authoritative backstop;
;; this guard only provides routing guidance to prevent structural splits.
;; --------------------------------------------------

(define (count-paren-depth text)
  (define in (open-input-string text))
  (let loop ([depth 0])
    (define-values (lexeme type _paren _start _end) (racket-lexer in))
    (cond
      [(eq? type 'eof) depth]
      [(eq? type 'parenthesis)
       (loop (+ depth
                (for/fold ([delta 0]) ([ch (in-string lexeme)])
                  (cond
                    [(memv ch '(#\( #\[ #\{)) (add1 delta)]
                    [(memv ch '(#\) #\] #\})) (sub1 delta)]
                    [else delta]))))]
      [else (loop depth)])))

(define (racket-edit-balance-warning path old-text new-text)
  (and (racket-source-path? path)
       (let ([delta (- (count-paren-depth new-text) (count-paren-depth old-text))])
         (and (not (zero? delta))
              (format
               (string-append
                "Warning: this edit changes S-expression depth by ~a, "
                "which is a structural-split risk. "
                "For a whole-form replacement, replace the entire form in one edit "
                "(pass max-old-text-len explicitly if needed) or use the structural edit tool; "
                "do not split a nested form into partial edits.")
               (if (positive? delta)
                   (format "+~a" delta)
                   (number->string delta)))))))

;; Returns #f when the complete proposed content is readable, otherwise an
;; actionable error string. Non-Racket paths intentionally bypass validation.
(define (validate-proposed-racket-source path content)
  (and
   (racket-source-path? path)
   (let ([custodian (make-custodian)]
         [result-channel (make-async-channel)])
     (custodian-limit-memory custodian (* (current-racket-parse-memory-limit-mb) 1024 1024) custodian)
     (parameterize ([current-custodian custodian])
       (thread
        (lambda ()
          (define result
            (with-handlers ([(lambda (_) #t) (lambda (raised)
                                               (validation-error
                                                path
                                                (if (exn? raised)
                                                    (exn-message raised)
                                                    (format "reader raised non-exception value: ~e"
                                                            raised))))])
              (define policy-error (reader-policy-error content))
              (if policy-error
                  (validation-error path policy-error)
                  (begin
                    (read-all-syntax path content)
                    #f))))
          (async-channel-put result-channel (cons 'complete result)))))
     (define outcome (sync/timeout (/ (current-racket-parse-timeout-ms) 1000.0) result-channel))
     (custodian-shutdown-all custodian)
     (if outcome
         (cdr outcome)
         (validation-error path
                           (format "validation timed out after ~a ms"
                                   (current-racket-parse-timeout-ms)))))))
