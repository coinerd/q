#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; Cross-plane contract tests for local and worker edit execution.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool.rkt" tool-result-is-error? tool-result-details tool-result-content)
         (only-in "../tools/builtins/edit.rkt"
                  current-fuzzy-edit-enabled?
                  current-max-old-text-len
                  set-current-max-old-text-len!
                  tool-edit)
         (only-in "../tools/builtins/edit-contract.rkt" DEFAULT-MAX-OLD-TEXT-LEN)
         (only-in "../tools/exec-context.rkt" make-exec-context)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-response-status
                  ipc-response-content
                  ipc-response-details
                  ipc-response-error-message)
         (only-in "../sandbox/worker-tools.rkt" current-allowed-roots dispatch-tool execute-edit))

(define (local-result-text result)
  (define c (tool-result-content result))
  (if (and (pair? c) (hash? (car c)))
      (hash-ref (car c) 'text "")
      (format "~a" c)))

(define (worker-result-text result)
  (ipc-response-content result))

(define (run-parity-case content
                         old-text
                         new-text
                         #:fuzzy? [fuzzy? #f]
                         #:global-fuzzy? [global-fuzzy? #f]
                         #:max-old-text-len [max-old-text-len DEFAULT-MAX-OLD-TEXT-LEN]
                         #:expected-status expected-status
                         #:expected-content expected-content
                         #:expected-replacements [expected-replacements #f])
  (define dir (make-temporary-file "q-edit-parity-~a" 'directory))
  (define local-path (build-path dir "local.txt"))
  (define worker-path (build-path dir "worker.txt"))
  (dynamic-wind
   void
   (lambda ()
     (display-to-file content local-path #:exists 'replace)
     (display-to-file content worker-path #:exists 'replace)
     (define common-args (hasheq 'old-text old-text 'new-text new-text 'fuzzy? fuzzy?))
     (define previous-max-old-text-len (current-max-old-text-len))
     (dynamic-wind
      (lambda () (set-current-max-old-text-len! max-old-text-len))
      (lambda ()
        (define local-result
          (parameterize ([current-fuzzy-edit-enabled? global-fuzzy?])
            (tool-edit (hash-set common-args 'path (path->string local-path)))))
        (define worker-result
          (parameterize ([current-allowed-roots (list dir)])
            (execute-edit (hash-set common-args 'path (path->string worker-path))
                          #:max-old-text-len max-old-text-len
                          #:fuzzy-edit-enabled? global-fuzzy?)))
        (define local-status (if (tool-result-is-error? local-result) 'error 'ok))
        (define worker-status (ipc-response-status worker-result))
        (check-equal? local-status expected-status "unexpected local status")
        (check-equal? worker-status expected-status "unexpected worker status")
        (check-equal? worker-status local-status "local and worker statuses differ")
        (check-equal? (file->string local-path) expected-content "unexpected local content")
        (check-equal? (file->string worker-path) expected-content "unexpected worker content")
        (check-equal? (file->string worker-path)
                      (file->string local-path)
                      "local and worker content differs")
        (when expected-replacements
          (check-equal? (hash-ref (tool-result-details local-result) 'replacements #f)
                        expected-replacements
                        "unexpected local replacement count")
          (check-equal? (hash-ref (ipc-response-details worker-result) 'replacements #f)
                        expected-replacements
                        "unexpected worker replacement count")))
      (lambda () (set-current-max-old-text-len! previous-max-old-text-len))))
   (lambda () (delete-directory/files dir))))

(define parity-suite
  (test-suite "Local/worker edit execution parity"

    (test-case "unique exact match is replaced once"
      (run-parity-case "alpha beta gamma"
                       "beta"
                       "BETA"
                       #:expected-status 'ok
                       #:expected-content "alpha BETA gamma"
                       #:expected-replacements 1))

    (test-case "duplicate exact match is rejected without mutation"
      (run-parity-case "same / same"
                       "same"
                       "changed"
                       #:expected-status 'error
                       #:expected-content "same / same"))

    (test-case "empty old-text is rejected without mutation"
      (run-parity-case "unchanged"
                       ""
                       "prefix"
                       #:expected-status 'error
                       #:expected-content "unchanged"))

    (test-case "overlapping exact matches are rejected without mutation"
      (run-parity-case "aaa" "aa" "changed" #:expected-status 'error #:expected-content "aaa"))

    (test-case "string fuzzy flag is rejected instead of treated as truthy"
      (define dir (make-temporary-file "q-edit-fuzzy-type-~a" 'directory))
      (define local-path (build-path dir "local.txt"))
      (define worker-path (build-path dir "worker.txt"))
      (dynamic-wind void
                    (lambda ()
                      (display-to-file "alpha  \nbeta" local-path #:exists 'replace)
                      (display-to-file "alpha  \nbeta" worker-path #:exists 'replace)
                      (define args (hasheq 'old-text "alpha\n" 'new-text "gamma\n" 'fuzzy? "false"))
                      (define local-result
                        (tool-edit (hash-set args 'path (path->string local-path))))
                      (define worker-result
                        (parameterize ([current-allowed-roots (list dir)])
                          (execute-edit (hash-set args 'path (path->string worker-path)))))
                      (check-true (tool-result-is-error? local-result))
                      (check-equal? (ipc-response-status worker-result) 'error)
                      (check-equal? (file->string local-path) "alpha  \nbeta")
                      (check-equal? (file->string worker-path) "alpha  \nbeta"))
                    (lambda () (delete-directory/files dir))))

    (test-case "old-text over 2000 characters is rejected without mutation"
      (define content (make-string 2100 #\x))
      (run-parity-case content
                       (make-string 2001 #\x)
                       "short"
                       #:expected-status 'error
                       #:expected-content content))

    (test-case "nondefault max length above default is honored in both planes"
      (define old-text (make-string 501 #\x))
      (run-parity-case old-text
                       old-text
                       "short"
                       #:max-old-text-len 600
                       #:expected-status 'ok
                       #:expected-content "short"
                       #:expected-replacements 1))

    (test-case "worker dispatch honors max-old-text-len from public arguments"
      (define dir (make-temporary-file "q-edit-worker-limit-~a" 'directory))
      (define path (build-path dir "worker.txt"))
      (define old-text (make-string 501 #\x))
      (dynamic-wind void
                    (lambda ()
                      (display-to-file old-text path #:exists 'replace)
                      (define result
                        (parameterize ([current-allowed-roots (list dir)])
                          (dispatch-tool "edit"
                                         (hasheq 'path
                                                 (path->string path)
                                                 'old-text
                                                 old-text
                                                 'new-text
                                                 "short"
                                                 'max-old-text-len
                                                 600))))
                      (check-equal? (ipc-response-status result) 'ok)
                      (check-equal? (file->string path) "short"))
                    (lambda () (delete-directory/files dir))))

    (test-case "explicit false max-old-text-len is rejected in both planes"
      (define dir (make-temporary-file "q-edit-false-limit-~a" 'directory))
      (define local-path (build-path dir "local.txt"))
      (define worker-path (build-path dir "worker.txt"))
      (define content "unchanged")
      (dynamic-wind
       void
       (lambda ()
         (display-to-file content local-path #:exists 'replace)
         (display-to-file content worker-path #:exists 'replace)
         (define local-result
           (tool-edit (hasheq 'path
                              (path->string local-path)
                              'old-text
                              content
                              'new-text
                              "changed"
                              'max-old-text-len
                              #f)))
         (define worker-result
           (parameterize ([current-allowed-roots (list dir)])
             (dispatch-tool "edit"
                            (hasheq 'path
                                    (path->string worker-path)
                                    'old-text
                                    content
                                    'new-text
                                    "changed"
                                    'max-old-text-len
                                    #f))))
         (check-true (tool-result-is-error? local-result))
         (check-equal? (ipc-response-status worker-result) 'error)
         (check-true (string-contains? (local-result-text local-result) "max-old-text-len"))
         (check-true (string-contains? (ipc-response-error-message worker-result) "max-old-text-len"))
         (check-equal? (file->string local-path) content)
         (check-equal? (file->string worker-path) content))
       (lambda () (delete-directory/files dir))))

    (test-case "worker too-long error routes to a whole-form structural edit"
      (define dir (make-temporary-file "q-edit-worker-too-long-~a" 'directory))
      (define path (build-path dir "worker.txt"))
      (define content (make-string 2100 #\x))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file content path #:exists 'replace)
         (define result
           (parameterize ([current-allowed-roots (list dir)])
             (dispatch-tool
              "edit"
              (hasheq 'path (path->string path) 'old-text (make-string 2001 #\x) 'new-text "short"))))
         (define message (ipc-response-error-message result))
         (check-equal? (ipc-response-status result) 'error)
         (check-true (string-contains? message "max-old-text-len"))
         (check-true (string-contains? message "whole-form"))
         (check-true (string-contains? message "structural edit tool"))
         (check-equal? (file->string path) content))
       (lambda () (delete-directory/files dir))))

    (test-case "worker parse rejection includes lexer-aware balance guidance"
      (define dir (make-temporary-file "q-edit-worker-balance-~a" 'directory))
      (define path (build-path dir "worker.rkt"))
      (define content "#lang racket/base\n(define (f)\n  1)\n")
      (dynamic-wind
       void
       (lambda ()
         (display-to-file content path #:exists 'replace)
         (define result
           (parameterize ([current-allowed-roots (list dir)])
             (dispatch-tool
              "edit"
              (hasheq 'path (path->string path) 'old-text "  1" 'new-text "  (begin\n    1"))))
         (define message (ipc-response-error-message result))
         (check-equal? (ipc-response-status result) 'error)
         (check-true (string-contains? message "changes S-expression depth"))
         (check-true (string-contains? message "structural-split risk"))
         (check-true (string-contains? message "whole-form replacement"))
         (check-equal? (file->string path) content))
       (lambda () (delete-directory/files dir))))

    (test-case "literal U+2014 survives matching and replacement"
      (run-parity-case "before — after"
                       "—"
                       "— preserved —"
                       #:expected-status 'ok
                       #:expected-content "before — preserved — after"
                       #:expected-replacements 1))

    (test-case "fuzzy matching is disabled by default in both planes"
      (run-parity-case "alpha  \nbeta"
                       "alpha\n"
                       "gamma\n"
                       #:expected-status 'error
                       #:expected-content "alpha  \nbeta"))

    (test-case "fuzzy matching applies one normalized-equivalent replacement"
      (run-parity-case "alpha  \nbeta"
                       "alpha\n"
                       "gamma\n"
                       #:fuzzy? #t
                       #:expected-status 'ok
                       #:expected-content "gamma\nbeta"
                       #:expected-replacements 1))

    (test-case "ambiguous fuzzy matches are rejected without mutation"
      (define content "alpha  \nbeta\n---\nalpha\t\nbeta")
      (run-parity-case content
                       "alpha\nbeta"
                       "changed"
                       #:fuzzy? #t
                       #:expected-status 'error
                       #:expected-content content))

    (test-case "CRLF fuzzy match ending at newline consumes both bytes"
      (run-parity-case "alpha\r\nbeta"
                       "alpha\n"
                       "gamma\n"
                       #:fuzzy? #t
                       #:expected-status 'ok
                       #:expected-content "gamma\nbeta"
                       #:expected-replacements 1))

    (test-case "global fuzzy setting overrides an explicit false argument in both planes"
      (run-parity-case "alpha  \nbeta"
                       "alpha\n"
                       "gamma\n"
                       #:global-fuzzy? #t
                       #:expected-status 'ok
                       #:expected-content "gamma\nbeta"
                       #:expected-replacements 1))

    (test-case "line-count integrity rejection is shared by both planes"
      (define content "start\n\n\n\n\nend")
      (run-parity-case content
                       "start\n\nend"
                       "done"
                       #:fuzzy? #t
                       #:expected-status 'error
                       #:expected-content content))

    (test-case "relative path resolves from CWD in both planes"
      ;; Use separate files for local and worker to avoid local edit consuming the content
      (define dir-a (make-temporary-file "q-edit-cwd-a-~a" 'directory))
      (define dir-b (make-temporary-file "q-edit-cwd-b-~a" 'directory))
      (define local-path (build-path dir-a "data.txt"))
      (define worker-path (build-path dir-b "data.txt"))
      (define original-content "hello world\nold-line\ngoodbye")
      (display-to-file original-content local-path #:exists 'replace)
      (display-to-file original-content worker-path #:exists 'replace)
      (dynamic-wind
       void
       (lambda ()
         (define local-result
           (parameterize ([current-fuzzy-edit-enabled? #f])
             (tool-edit
              (hasheq 'old-text "old-line" 'new-text "new-line" 'path (path->string local-path))
              #f)))
         (define worker-result
           (parameterize ([current-allowed-roots (list dir-b)])
             (execute-edit
              (hasheq 'old-text "old-line" 'new-text "new-line" 'path (path->string worker-path)))))
         (check-false (tool-result-is-error? local-result) "local relative-path edit should succeed")
         (check-equal? (ipc-response-status worker-result)
                       'ok
                       "worker relative-path edit should succeed")
         (check-equal? (file->string local-path)
                       "hello world\nnew-line\ngoodbye"
                       "local content after edit should match")
         (check-equal? (file->string worker-path)
                       "hello world\nnew-line\ngoodbye"
                       "worker content after edit should match"))
       (lambda ()
         (delete-directory/files dir-a)
         (delete-directory/files dir-b))))

    (test-case "worker edit reports the same canonical path as direct edit"
      ;; The direct edit tool resolves its input to one absolute,
      ;; symlink-resolved spelling and reports THAT in details and diagnostics.
      ;; The worker used to report the lexical `path->complete-path` spelling,
      ;; so a relative input or a symlinked input produced two different
      ;; success details for the same bytes.
      (define direct-dir (make-temporary-file "q-edit-canonical-direct-~a" 'directory))
      (define worker-dir (make-temporary-file "q-edit-canonical-worker-~a" 'directory))
      (define direct-file (build-path direct-dir "real.txt"))
      (define worker-file (build-path worker-dir "real.txt"))
      (define worker-link (build-path worker-dir "link.txt"))
      (display-to-file "alpha\n" direct-file #:exists 'replace)
      (display-to-file "alpha\n" worker-file #:exists 'replace)
      (make-file-or-directory-link worker-file worker-link)
      (dynamic-wind
       void
       (lambda ()
         (define direct-result
           (parameterize ([current-fuzzy-edit-enabled? #f])
             (tool-edit (hasheq 'path "real.txt" 'old-text "alpha" 'new-text "beta")
                        (make-exec-context #:working-directory direct-dir))))
         (check-false (tool-result-is-error? direct-result) "direct relative edit succeeds")
         (define direct-path (hash-ref (tool-result-details direct-result) 'path))
         (check-equal? direct-path
                       (path->string (simplify-path (resolve-path direct-file) #f))
                       "direct details are the canonical spelling")
         (define worker-canonical (path->string (simplify-path (resolve-path worker-file) #f)))
         ;; Worker, relative input from the same kind of working directory.
         (define worker-relative
           (parameterize ([current-allowed-roots (list worker-dir)]
                          [current-directory worker-dir])
             (execute-edit (hasheq 'path "real.txt" 'old-text "alpha" 'new-text "beta"))))
         (check-equal? (ipc-response-status worker-relative) 'ok)
         (check-equal? (hash-ref (ipc-response-details worker-relative) 'path)
                       worker-canonical
                       "worker details report the canonical spelling of a relative input")
         ;; Worker, symlinked input: the reported path is the target, not the link.
         (display-to-file "alpha\n" worker-file #:exists 'replace)
         (define worker-linked
           (parameterize ([current-allowed-roots (list worker-dir)]
                          [current-directory worker-dir])
             (execute-edit (hasheq 'path "link.txt" 'old-text "alpha" 'new-text "gamma"))))
         (check-equal? (ipc-response-status worker-linked) 'ok)
         (check-equal? (hash-ref (ipc-response-details worker-linked) 'path)
                       worker-canonical
                       "worker details report the resolved target of a symlinked input")
         (check-false (string=? (hash-ref (ipc-response-details worker-linked) 'path)
                                (path->string worker-link))
                      "the link spelling is never reported")
         (check-equal? (file->string worker-file) "gamma\n")
         (check-true (link-exists? worker-link)
                     "the edit writes through the canonical path, not over the link")
         ;; Not-found diagnostics name an absolute canonical path in both planes.
         (define direct-missing
           (tool-edit (hasheq 'path "missing.txt" 'old-text "x" 'new-text "y")
                      (make-exec-context #:working-directory direct-dir)))
         (check-true (tool-result-is-error? direct-missing))
         (define direct-missing-text (local-result-text direct-missing))
         (define worker-missing
           (parameterize ([current-allowed-roots (list worker-dir)]
                          [current-directory worker-dir])
             (execute-edit (hasheq 'path "missing.txt" 'old-text "x" 'new-text "y"))))
         (check-equal? (ipc-response-status worker-missing) 'error)
         (define worker-missing-text (ipc-response-error-message worker-missing))
         (define missing-relative (path->string (build-path "missing.txt")))
         (for ([text (in-list (list direct-missing-text worker-missing-text))]
               [dir (in-list (list direct-dir worker-dir))])
           (check-false (string-contains? text (string-append " " missing-relative))
                        "a relative spelling is not what diagnostics report")
           (check-true (string-contains? text (path->string (build-path dir "missing.txt")))
                       "diagnostics name the absolute path")))
       (lambda ()
         (delete-directory/files direct-dir #:must-exist? #f)
         (delete-directory/files worker-dir #:must-exist? #f))))

    (test-case "not-found diagnostics include first-differing-offset, escaped code points, whitespace count"
      (define content "def foo(em — dash):\n    pass\n")
      (define old-text-with-em-dash "def foo(em \u2014 dash):\n    pass\n")
      (define old-text-with-different-indent "def foo(em \u2014 dash):\n   pass\n")
      (define p (make-temporary-file "q-edit-diagnostic-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file content p #:exists 'replace)
         ;; Test 1: exact match should succeed
         (define ok-result
           (tool-edit (hasheq 'path p 'old-text old-text-with-em-dash 'new-text "replaced") #f))
         (check-false (tool-result-is-error? ok-result) "exact match with em dash should succeed")
         ;; Recreate file
         (display-to-file content p #:exists 'replace)
         ;; Test 2: a one-space leading-whitespace mismatch now succeeds via the
         ;; leading-ws auto-fallback (#9366) and reports the indentation note
         (define ws-result
           (tool-edit (hasheq 'path p 'old-text old-text-with-different-indent 'new-text "replaced")
                      #f))
         (check-false (tool-result-is-error? ws-result)
                      "leading-whitespace-only mismatch should auto-fallback")
         (define ws-text (local-result-text ws-result))
         (check-true (string-contains? ws-text "Note:")
                     "auto-fallback should surface the indentation note")
         ;; Recreate file
         (display-to-file content p #:exists 'replace)
         ;; Test 3: a genuine content mismatch still reports detailed diagnostics
         (define old-text-different-body "def foo(em \u2014 dash):\n    fail\n")
         (define fail-result
           (tool-edit (hasheq 'path p 'old-text old-text-different-body 'new-text "replaced") #f))
         (check-true (tool-result-is-error? fail-result) "content mismatch should fail")
         (define err-text (local-result-text fail-result))
         (check-true (string-contains? err-text "First differing offset:")
                     "diagnostics should mention the first difference")
         (check-true (string-contains? err-text "line 2, column 5")
                     "diagnostics should give line and column")
         (check-true (string-contains? err-text "  file:")
                     "diagnostics should show a readable file line")
         (check-true (string-contains? err-text "  old-text:")
                     "diagnostics should show the readable old-text line")
         (check-true (string-contains? err-text "(U+70)")
                     "the differing printable character keeps its code point")
         (check-false (string-contains? err-text "Context around mismatch in file:  [U+")
                      "raw code-point lists are gone"))
       (lambda () (delete-file p))))

    (test-case "not-found diagnostics show escaped code points for U+2014 mismatch"
      (define content "line with em \u2014 dash\n")
      (define old-text-wrong-dash
        "line with em - dash\n") ;; uses regular hyphen-minus U+002D instead of U+2014
      (define p (make-temporary-file "q-edit-emdash-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file content p #:exists 'replace)
         (define result
           (tool-edit (hasheq 'path p 'old-text old-text-wrong-dash 'new-text "replaced") #f))
         (check-true (tool-result-is-error? result) "wrong dash should fail")
         (define err-text (local-result-text result))
         (check-true (string-contains? err-text "U+2014") "diagnostics should show expected U+2014")
         (check-true (string-contains? err-text "offset") "diagnostics should mention offset"))
       (lambda () (delete-file p))))))

(module+ test
  (run-tests parity-suite))

(module+ main
  (run-tests parity-suite))
