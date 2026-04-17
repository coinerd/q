#lang racket

;; tests/test-wave12.rkt — Wave 12 tests
;;
;; Covers:
;;   (A) Image rendering — Kitty/iTerm2 escape sequence generation
;;   (B) Skills frontmatter — parsing, validation, name checks
;;   (C) Graceful shutdown — shutdown flags on agent-session
;;   (D) Iteration loop shutdown — shutdown-check/force-shutdown-check thunks

(require rackunit
         rackunit/text-ui
         racket/file
         "../tui/image.rkt"
         "../skills/frontmatter.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt"
         "../runtime/agent-session.rkt"
         (only-in "helpers/mock-provider.rkt" make-simple-mock-provider))

;; ============================================================
;; (A) Image rendering tests
;; ============================================================

(define image-tests
  (test-suite "Wave 12 — Image Rendering"

    (test-case "string-chunks splits correctly"
      (check-equal? (string-chunks "abcdef" 3) '("abc" "def"))
      (check-equal? (string-chunks "abcdefg" 3) '("abc" "def" "g"))
      (check-equal? (string-chunks "" 3) '())
      (check-equal? (string-chunks "ab" 5) '("ab")))

    (test-case "kitty-image-escape produces escape sequences"
      (define result (kitty-image-escape "dGVzdA==" #:width 10 #:height 5))
      (check-true (string-contains? result "\x1b_G"))
      (check-true (string-contains? result "w=10"))
      (check-true (string-contains? result "h=5"))
      (check-true (string-contains? result "\x1b\\")))

    (test-case "kitty-image-escape works without dimensions"
      (define result (kitty-image-escape "dGVzdA=="))
      (check-true (string-contains? result "\x1b_G"))
      (check-false (string-contains? result "w="))
      (check-false (string-contains? result "h=")))

    (test-case "iterm2-image-escape produces 1337 escape"
      (define result (iterm2-image-escape "dGVzdA==" #:width 20))
      (check-true (string-contains? result "\x1b]1337;File=inline=1"))
      (check-true (string-contains? result "width=20px"))
      (check-true (string-contains? result "dGVzdA=="))
      (check-true (string-contains? result "\x07")))

    (test-case "render-image-escape with kitty protocol"
      (define result (render-image-escape "dGVzdA==" #:width 10 #:protocol 'kitty))
      (check-true (string-contains? result "\x1b_G")))

    (test-case "render-image-escape with iterm2 protocol"
      (define result (render-image-escape "dGVzdA==" #:width 10 #:protocol 'iterm2))
      (check-true (string-contains? result "\x1b]1337;File=inline=1")))

    (test-case "render-image-escape with wezterm falls back to iterm2"
      (define result (render-image-escape "dGVzdA==" #:protocol 'wezterm))
      (check-true (string-contains? result "\x1b]1337;File=inline=1")))

    (test-case "render-image-escape returns #f for no protocol"
      (check-false (render-image-escape "dGVzdA==" #:protocol #f)))

    (test-case "image-placeholder generates box"
      (define result (image-placeholder "hello" 20))
      (check-true (string-contains? result "hello"))
      (check-true (string-contains? result "┌"))
      (check-true (string-contains? result "┘")))

    (test-case "image-placeholder truncates long text"
      (define result (image-placeholder "abcdefghijklmnopqrstuvwxyz" 10))
      (check-true (string-contains? result "...")))))

;; ============================================================
;; (B) Skills frontmatter tests
;; ============================================================

(define frontmatter-tests
  (test-suite "Wave 12 — Skills Frontmatter"

    (test-case "valid-skill-name? accepts good names"
      (check-true (valid-skill-name? "my-skill"))
      (check-true (valid-skill-name? "a"))
      (check-true (valid-skill-name? "skill-123"))
      (check-true (valid-skill-name? "abc")))

    (test-case "valid-skill-name? rejects bad names"
      (check-false (valid-skill-name? ""))
      (check-false (valid-skill-name? "-starts-dash"))
      (check-false (valid-skill-name? "ends-dash-"))
      (check-false (valid-skill-name? "UPPER"))
      (check-false (valid-skill-name? "has space"))
      (check-false (valid-skill-name? 42))
      (check-false (valid-skill-name? (make-string 65 #\a))))

    (test-case "parse-skill-frontmatter extracts fields"
      (define content "---\nname: my-skill\ndescription: A test skill\n---\nBody here")
      (define fm (parse-skill-frontmatter content))
      (check-equal? (hash-ref fm 'name) "my-skill")
      (check-equal? (hash-ref fm 'description) "A test skill"))

    (test-case "parse-skill-frontmatter returns #f for no markers"
      (check-false (parse-skill-frontmatter "No frontmatter here"))
      (check-false (parse-skill-frontmatter ""))
      (check-false (parse-skill-frontmatter "---\nonly opener"))
      (check-false (parse-skill-frontmatter "a\nb")))

    (test-case "parse-skill-frontmatter handles quoted values"
      (define content "---\nname: \"quoted-name\"\n---\n")
      (define fm (parse-skill-frontmatter content))
      (check-equal? (hash-ref fm 'name) "quoted-name"))

    (test-case "validate-frontmatter OK for valid input"
      (define fm (hasheq 'name "my-skill" 'description "A test"))
      (define-values (status msg) (validate-frontmatter fm "my-skill"))
      (check-equal? status 'ok)
      (check-equal? msg "Valid"))

    (test-case "validate-frontmatter errors on missing name"
      (define fm (hasheq 'description "A test"))
      (define-values (status msg) (validate-frontmatter fm "my-skill"))
      (check-equal? status 'error)
      (check-true (string-contains? msg "name")))

    (test-case "validate-frontmatter errors on invalid name"
      (define fm (hasheq 'name "BAD-NAME" 'description "desc"))
      (define-values (status msg) (validate-frontmatter fm "BAD-NAME"))
      (check-equal? status 'error))

    (test-case "validate-frontmatter warns on name/directory mismatch"
      (define fm (hasheq 'name "my-skill" 'description "A test"))
      (define-values (status msg) (validate-frontmatter fm "other-name"))
      (check-equal? status 'warning)
      (check-true (string-contains? msg "doesn't match")))

    (test-case "validate-frontmatter errors on missing description"
      (define fm (hasheq 'name "my-skill"))
      (define-values (status msg) (validate-frontmatter fm "my-skill"))
      (check-equal? status 'error)
      (check-true (string-contains? msg "description")))))

;; ============================================================
;; (C) Graceful shutdown tests
;; ============================================================

(define shutdown-tests
  (test-suite "Wave 12 — Graceful Shutdown"

    (test-case "new session has shutdown flags as #f"
      (define tmpdir (make-temporary-file "q-shutdown-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "hello"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (check-false (shutdown-requested? sess))
                      (check-false (force-shutdown-requested? sess)))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    (test-case "request-shutdown! sets flag"
      (define tmpdir (make-temporary-file "q-shutdown-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "hello"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (request-shutdown! sess)
                      (check-true (shutdown-requested? sess))
                      (check-false (force-shutdown-requested? sess)))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    (test-case "force-shutdown! sets flag"
      (define tmpdir (make-temporary-file "q-shutdown-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "hello"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (force-shutdown! sess)
                      (check-false (shutdown-requested? sess))
                      (check-true (force-shutdown-requested? sess)))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    (test-case "reset-shutdown-flags! clears both"
      (define tmpdir (make-temporary-file "q-shutdown-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "hello"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (request-shutdown! sess)
                      (force-shutdown! sess)
                      (check-true (shutdown-requested? sess))
                      (check-true (force-shutdown-requested? sess))
                      (reset-shutdown-flags! sess)
                      (check-false (shutdown-requested? sess))
                      (check-false (force-shutdown-requested? sess)))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    (test-case "resumed session has shutdown flags as #f"
      (define tmpdir (make-temporary-file "q-shutdown-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "hello"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (request-shutdown! sess)
                      (force-shutdown! sess)
                      (close-session! sess)
                      (define resumed
                        (resume-agent-session (session-id sess)
                                              (hasheq 'provider
                                                      prov
                                                      'tool-registry
                                                      reg
                                                      'event-bus
                                                      bus
                                                      'session-dir
                                                      (path->string tmpdir)
                                                      'model-name
                                                      "test")))
                      ;; Resumed session should start with clean shutdown flags
                      (check-false (shutdown-requested? resumed))
                      (check-false (force-shutdown-requested? resumed)))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))))

;; ============================================================
;; Run all
;; ============================================================

(run-tests image-tests)
(run-tests frontmatter-tests)
(run-tests shutdown-tests)
