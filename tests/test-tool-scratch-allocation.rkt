#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-tool-scratch-allocation.rkt — W2 trusted scratch is PER SESSION.
;;
;; Every session owns one unique restrictive root below
;; <platform-temp>/q-agent-scratch. Two sessions in the SAME process cannot
;; reach each other's root through any plane (structured write/edit, bash
;; redirection, worker allowed roots, prompt guidance), and there is no
;; fallback that sanctions the scratch BASE directory when a session has no
;; allocation.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../tools/scratch.rkt"
         (only-in "../tools/builtins/bash-safety.rkt"
                  sanctioned-scratch-root
                  current-sanctioned-scratch-root
                  destructive-command?
                  destructive-reason)
         (only-in "../tools/tool.rkt"
                  tool-result-is-error?
                  tool-result-content
                  tool-result-details
                  make-exec-context)
         (only-in "../tools/builtins/write.rkt" tool-write)
         (only-in "../tools/builtins/edit.rkt" tool-edit)
         (only-in "../tools/builtins/read.rkt" tool-read)
         (only-in "../tools/builtins/bash.rkt"
                  tool-bash
                  make-bash-execution-config
                  current-bash-execution-config)
         (only-in "../util/safe-mode/safe-mode-state.rkt"
                  current-safe-mode-config
                  make-safe-mode-config)
         (only-in "../util/safe-mode/safe-mode-predicates.rkt" allowed-tool?))

(define (fresh-base-dir)
  (make-temporary-file "q-scratch-base-~a" 'directory))

(define (cleanup dir)
  (delete-directory/files dir #:must-exist? #f))

(define (same-path? a b)
  (equal? (path->string a) (path->string b)))

;; The first text block of a tool result (error or success).
(define (result-text result)
  (define content (tool-result-content result))
  (cond
    [(and (list? content) (pair? content) (hash? (car content))) (hash-ref (car content) 'text "")]
    [else (format "~a" content)]))

(define scratch-suite
  (test-suite "tools/scratch.rkt"

    (test-case "valid tokens are opaque lowercase hex of a bounded length"
      (check-true (valid-scratch-token? (fresh-scratch-token)))
      (check-true (valid-scratch-token? "0123456789abcdef"))
      (check-false (valid-scratch-token? "0123456789ABCDEF"))
      (check-false (valid-scratch-token? "short"))
      (check-false (valid-scratch-token? "../escape"))
      (check-false (valid-scratch-token? "/tmp/q-agent-scratch/x"))
      (check-false (valid-scratch-token? "abc/def"))
      (check-false (valid-scratch-token? #f)))

    (test-case "root shape is exactly one opaque component below the base"
      (define base (fresh-base-dir))
      (dynamic-wind void
                    (lambda ()
                      (parameterize ([current-scratch-base-dir base])
                        (define token (fresh-scratch-token))
                        (define root (scratch-root-for-token token))
                        (check-equal? (path->string root) (path->string (build-path base token)))
                        (check-true (trusted-session-scratch-path? root))
                        (check-false (trusted-session-scratch-path? base)
                                     "the base directory itself is never a session root")
                        (check-false (trusted-session-scratch-path? (build-path base token "child"))
                                     "a nested path is not a session allocation")
                        (check-false (trusted-session-scratch-path? (build-path base ".." token))
                                     "an up component is not a session allocation")
                        (check-false (trusted-session-scratch-path? (build-path base token "-suffix"))
                                     "the token boundary is exact")
                        (check-false (scratch-root-for-token "../escape"))))
                    (lambda () (cleanup base))))

    (test-case "allocation is unique per session, stable for that session, and restrictive"
      (define base (fresh-base-dir))
      (dynamic-wind void
                    (lambda ()
                      (parameterize ([current-scratch-base-dir base])
                        (reset-session-scratch-registry!)
                        (define root-a (allocate-session-scratch-root! "session-a"))
                        (define root-b (allocate-session-scratch-root! "session-b"))
                        (check-not-equal? (path->string root-a)
                                          (path->string root-b)
                                          "two sessions never share one scratch root")
                        (check-true (same-path? root-a (allocate-session-scratch-root! "session-a"))
                                    "one allocation per session key")
                        (check-true (same-path? root-b (allocated-session-scratch-root "session-b")))
                        (check-false (allocated-session-scratch-root "session-unknown")
                                     "lookup never allocates and never falls back")
                        (for ([root (in-list (list root-a root-b))])
                          (check-true (directory-exists? root))
                          (check-true (trusted-session-scratch-path? root))
                          (check-equal? (file-or-directory-permissions root 'bits)
                                        #o700
                                        "session scratch is owner-only"))
                        (check-equal? (file-or-directory-permissions base 'bits)
                                      #o700
                                      "base scratch directory is owner-only")
                        ;; Release forgets the binding for exactly one session.
                        (release-session-scratch-root! "session-a")
                        (check-false (allocated-session-scratch-root "session-a"))
                        (check-true (same-path? root-b (allocated-session-scratch-root "session-b")))
                        (reset-session-scratch-registry!)))
                    (lambda () (cleanup base))))

    (test-case "RED-FIRST: bash redirection follows the per-session root"
      ;; This is the W2 blocker: a process-global allocation let any session --
      ;; and any fallback -- reach every other session's scratch. Each session
      ;; now owns one root and the destructive-redirection policy enforces that
      ;; boundary. (Safe mode itself refuses bash/write/edit outright; the
      ;; following case pins that read-only behavior instead of pretending a
      ;; session scratch root is a safe-mode write destination.)
      (define base (fresh-base-dir))
      (define repo (make-temporary-file "q-scratch-two-repo-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base])
           (reset-session-scratch-registry!)
           (define root-a (allocate-session-scratch-root! "session-a"))
           (define root-b (allocate-session-scratch-root! "session-b"))
           (define ctx-a (make-exec-context #:working-directory repo #:scratch-root root-a))
           (define ctx-b (make-exec-context #:working-directory repo #:scratch-root root-b))
           ;; Each session may redirect into its OWN root ...
           (parameterize ([current-bash-execution-config (make-bash-execution-config #:block? #t)])
             (check-false
              (tool-result-is-error?
               (tool-bash (hasheq 'command
                                  (format "echo ok >~a" (path->string (build-path root-a "out.log"))))
                          ctx-a))
              (format "own scratch redirection must pass: ~a"
                      (tool-result-details
                       (tool-bash (hasheq 'command
                                          (format "echo ok >~a"
                                                  (path->string (build-path root-a "out.log"))))
                                  ctx-a))))
             (check-true (file-exists? (build-path root-a "out.log")))
             ;; ... and NOT into another session's root.
             (check-true (tool-result-is-error?
                          (tool-bash (hasheq 'command
                                             (format "echo no >~a"
                                                     (path->string (build-path root-b "out.log"))))
                                     ctx-a))
                         "session A cannot redirect into session B's root")
             (check-false (file-exists? (build-path root-b "out.log")))
             (check-false (tool-result-is-error?
                           (tool-bash (hasheq 'command
                                              (format "echo ok >~a"
                                                      (path->string (build-path root-b "own.log"))))
                                      ctx-b))
                          "session B writes its own root")
             (check-true (file-exists? (build-path root-b "own.log")))
             ;; Neither session may redirect into the BASE directory itself or
             ;; into an arbitrary /tmp path: the base is a container, not a
             ;; session root.
             (check-true (tool-result-is-error? (tool-bash (hasheq 'command
                                                                   (format "echo no >~a/base.log"
                                                                           (path->string base)))
                                                           ctx-a)))
             (check-false (file-exists? (build-path base "base.log"))))
           (reset-session-scratch-registry!)))
       (lambda ()
         (delete-directory/files base #:must-exist? #f)
         (delete-directory/files repo #:must-exist? #f))))

    (test-case "RED-FIRST: direct bash revalidates an exec-context scratch root"
      ;; A direct caller can stamp any path onto an exec-context. Bash must not
      ;; treat that value as authority: it reuses the same read-only
      ;; existing-trusted-session-scratch-root? predicate as the worker, so the
      ;; base container, a missing token directory, a lax directory, a symlink
      ;; and an unrelated directory all arm NOTHING. Redirection is then
      ;; refused under blocking policy (the worker always blocks).
      (define base (fresh-base-dir))
      (define repo (make-temporary-file "q-scratch-revalidate-repo-~a" 'directory))
      (define foreign (make-temporary-file "q-scratch-revalidate-foreign-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base])
           (reset-session-scratch-registry!)
           (define root (allocate-session-scratch-root! "session-revalidate"))
           (define ctx (make-exec-context #:working-directory repo #:scratch-root root))
           (define (refused? scratch-root target)
             (define ctx* (make-exec-context #:working-directory repo #:scratch-root scratch-root))
             (tool-result-is-error?
              (tool-bash (hasheq 'command (format "echo no >~a" (path->string target))) ctx*)))
           (parameterize ([current-bash-execution-config (make-bash-execution-config #:block? #t)])
             ;; The real allocation is accepted end to end.
             (check-false
              (tool-result-is-error?
               (tool-bash (hasheq 'command
                                  (format "echo ok >~a" (path->string (build-path root "ok.log"))))
                          ctx)))
             (check-true (file-exists? (build-path root "ok.log")))
             ;; The container itself is not a session root.
             (check-true (refused? base (build-path base "base.log")))
             (check-false (file-exists? (build-path base "base.log")))
             ;; A well-shaped but non-existent token directory is not trusted.
             (define missing-root (build-path base "fedcba9876543210"))
             (check-true (refused? missing-root (build-path missing-root "missing.log")))
             (check-false (file-exists? (build-path missing-root "missing.log")))
             ;; A lax (group/other-accessible) directory is not trusted.
             (define lax-root (build-path base "1111111111111111"))
             (make-directory* lax-root)
             (file-or-directory-permissions lax-root #o755)
             (check-true (refused? lax-root (build-path lax-root "lax.log")))
             (check-false (file-exists? (build-path lax-root "lax.log")))
             ;; A symlinked session directory is refused, never followed.
             (define link-root (build-path base "2222222222222222"))
             (make-file-or-directory-link foreign link-root)
             (check-true (refused? link-root (build-path link-root "link.log")))
             (check-false (file-exists? (build-path foreign "link.log")))
             ;; An unrelated directory is not a session allocation.
             (check-true (refused? repo (build-path repo "foreign.log")))
             (check-false (file-exists? (build-path repo "foreign.log"))))
           (reset-session-scratch-registry!)))
       (lambda ()
         (delete-directory/files base #:must-exist? #f)
         (delete-directory/files repo #:must-exist? #f)
         (delete-directory/files foreign #:must-exist? #f))))

    (test-case "safe mode stays read-only: the session scratch root is unavailable"
      ;; W2 review: the safe-mode allowlist blocks bash/edit/write, and a
      ;; session scratch allocation is NOT a safe-mode write exception. Write,
      ;; edit and read all refuse the scratch root consistently (no "writable
      ;; scratch" guidance), so safe mode keeps exactly its documented
      ;; read-only, project-root-bounded policy. Scratch is a trusted
      ;; destination only when safe mode is off.
      (define base (fresh-base-dir))
      (define repo (make-temporary-file "q-scratch-safe-repo-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base]
                        [current-safe-mode-config (make-safe-mode-config #:active #t
                                                                         #:project-root repo)])
           (check-false (allowed-tool? "write") "write must stay blocked by the safe-mode allowlist")
           (check-false (allowed-tool? "edit") "edit must stay blocked by the safe-mode allowlist")
           (check-false (allowed-tool? "bash") "bash must stay blocked by the safe-mode allowlist")
           (reset-session-scratch-registry!)
           (define root (allocate-session-scratch-root! "session-safe"))
           (define ctx (make-exec-context #:working-directory repo #:scratch-root root))
           (define target (build-path root "probe.rkt"))
           (display-to-file "original\n" target #:exists 'replace)
           (define write-result
             (tool-write (hasheq 'path (path->string target) 'content "changed\n") ctx))
           (check-true (tool-result-is-error? write-result)
                       "write into scratch is refused in safe mode")
           (check-true (string-contains? (result-text write-result) "not allowed (safe mode)"))
           (check-equal? (file->string target)
                         "original\n"
                         "the refused write never touched the file")
           (check-true
            (tool-result-is-error?
             (tool-edit (hasheq 'path (path->string target) 'old-text "original" 'new-text "changed")
                        ctx))
            "edit into scratch is refused in safe mode")
           (check-equal? (file->string target) "original\n")
           ;; read refuses the same path, so the policy is consistent rather
           ;; than "writes blocked, reads of the scratch tree silently allowed".
           (check-true (tool-result-is-error? (tool-read (hasheq 'path (path->string target)) ctx)))
           (reset-session-scratch-registry!)))
       (lambda ()
         (delete-directory/files base #:must-exist? #f)
         (delete-directory/files repo #:must-exist? #f))))

    (test-case "no allocation means no scratch: the base directory is never a fallback"
      ;; The former process-wide default sanctioned the whole
      ;; /tmp/q-agent-scratch container. That is gone: a session without an
      ;; allocated or explicitly adopted root has no trusted scratch.
      (check-false sanctioned-scratch-root "there is no process-wide sanctioned scratch root")
      (define base (fresh-base-dir))
      (define repo (make-temporary-file "q-scratch-noalloc-repo-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base]
                        [current-safe-mode-config (make-safe-mode-config #:active #t
                                                                         #:project-root repo)])
           (reset-session-scratch-registry!)
           (define token "0123456789abcdef")
           (define session-shaped (scratch-root-for-token token))
           ;; Nothing is sanctioned while no session has allocated a root.
           (check-false (current-sanctioned-scratch-root)
                        "no armed redirection root without a session allocation")
           (check-true (destructive-command? (format "raco fmt x.rkt >~a"
                                                     (path->string (build-path session-shaped
                                                                               "p.log")))))
           (check-eq? (destructive-reason (format "raco fmt x.rkt >~a"
                                                  (path->string (build-path session-shaped "p.log"))))
                      'redirection)
           (check-true (destructive-command? (format "raco fmt x.rkt >~a"
                                                     (path->string (build-path base "p.log")))))
           ;; Direct structured tools deny the same paths without a session root.
           (define ctx (make-exec-context #:working-directory repo))
           (check-true
            (tool-result-is-error?
             (tool-write
              (hasheq 'path (path->string (build-path session-shaped "p.rkt")) 'content "x\n")
              ctx)))
           (check-false (file-exists? (build-path session-shaped "p.rkt")))
           (parameterize ([current-bash-execution-config (make-bash-execution-config #:block? #t)])
             (check-true (tool-result-is-error?
                          (tool-bash (hasheq 'command
                                             (format "echo no >~a"
                                                     (path->string (build-path session-shaped
                                                                               "p.log"))))
                                     ctx))))
           (check-false (file-exists? (build-path session-shaped "p.log")))
           ;; An allocated root arms exactly that root.
           (define root (allocate-session-scratch-root! "session-allocated" token))
           (parameterize ([current-sanctioned-scratch-root root])
             (check-false (destructive-command? (format "raco fmt x.rkt >~a"
                                                        (path->string (build-path root "p.log")))))
             (check-true (destructive-command?
                          (format "raco fmt x.rkt >~a"
                                  (path->string (build-path base "other-token" "p.log")))))
             (check-true (destructive-command? (format "raco fmt x.rkt >~a"
                                                       (path->string (build-path base "p.log"))))))
           (reset-session-scratch-registry!)))
       (lambda ()
         (delete-directory/files base #:must-exist? #f)
         (delete-directory/files repo #:must-exist? #f))))

    (test-case "adoption is explicit, shape-checked and single-shot"
      (define base (fresh-base-dir))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base])
           (reset-session-scratch-registry!)
           (define advertised (allocate-session-scratch-root! "campaign-token"))
           ;; With no adoption in effect nothing is adopted.
           (check-false (take-adopted-session-scratch-root!))
           ;; Inside the adoption extent exactly one session adopts the
           ;; already advertised root; a second creation sees nothing.
           (define adopted
             (call-with-adopted-session-scratch-root advertised
                                                     (lambda ()
                                                       (define first
                                                         (begin0 (take-adopted-session-scratch-root!)
                                                           (void)))
                                                       (define second
                                                         (take-adopted-session-scratch-root!))
                                                       (list first second))))
           (check-true (same-path? (car adopted) advertised))
           (check-false (cadr adopted) "adoption is single-shot")
           ;; After the extent the box is gone.
           (check-false (take-adopted-session-scratch-root!))
           ;; An adopted root binds to a session key only when it is a real
           ;; trusted scratch path; a broader path is refused.
           (define bound (bind-session-scratch-root! "session-adopted" advertised))
           (check-true (same-path? bound advertised))
           (check-true (same-path? (allocated-session-scratch-root "session-adopted") advertised))
           (check-exn exn:fail?
                      (lambda () (bind-session-scratch-root! "session-wide" base))
                      "the base directory can never be adopted as a session root")
           (check-exn
            exn:fail?
            (lambda () (call-with-adopted-session-scratch-root (build-path base ".." "escape") void)))
           (reset-session-scratch-registry!)))
       (lambda () (cleanup base))))

    (test-case "a symlinked base or session root is refused, never followed"
      ;; A pre-created symlink where the allocation should be is a foreign
      ;; object: following it would make "one opaque component below the base"
      ;; describe a directory the model does not actually own.
      (define target (make-temporary-file "q-scratch-target-~a" 'directory))
      (define link-parent (make-temporary-file "q-scratch-link-parent-~a" 'directory))
      (define base-link (build-path link-parent "q-agent-scratch"))
      (make-file-or-directory-link target base-link)
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base-link])
           (reset-session-scratch-registry!)
           (check-exn exn:fail?
                      (lambda () (allocate-session-scratch-root! "session-link"))
                      "a symlinked allocation container must fail closed")
           (check-false (allocated-session-scratch-root "session-link")
                        "a refused allocation must not become a session root")
           (check-false (directory-exists? (build-path target "0123456789abcdef"))
                        "the refused allocation must not be created behind the link"))
         ;; A symlinked SESSION directory is refused the same way.
         (define real-base (make-temporary-file "q-scratch-real-base-~a" 'directory))
         (define other (make-temporary-file "q-scratch-other-target-~a" 'directory))
         (define root-link (build-path real-base "0123456789abcdef"))
         (make-file-or-directory-link other root-link)
         (parameterize ([current-scratch-base-dir real-base])
           (reset-session-scratch-registry!)
           (check-exn exn:fail?
                      (lambda () (allocate-session-scratch-root! "session-link" "0123456789abcdef"))
                      "a symlinked session root must fail closed")
           (check-false (allocated-session-scratch-root "session-link")))
         (reset-session-scratch-registry!)
         (delete-directory/files real-base #:must-exist? #f)
         (delete-directory/files other #:must-exist? #f))
       (lambda ()
         (delete-directory/files target #:must-exist? #f)
         (delete-directory/files link-parent #:must-exist? #f))))

    (test-case "a pre-existing lax base is forced back to owner-only"
      ;; An older installation, a lax umask, or a foreign actor widening the
      ;; container must not survive: the enforced mode is re-read, and only
      ;; owner-only bits are accepted.
      (define base (fresh-base-dir))
      (dynamic-wind void
                    (lambda ()
                      (parameterize ([current-scratch-base-dir base])
                        (file-or-directory-permissions base #o777)
                        (reset-session-scratch-registry!)
                        (define root
                          (allocate-session-scratch-root! "session-lax" "0123456789abcdef"))
                        (check-equal? (bitwise-and (file-or-directory-permissions base 'bits) #o077)
                                      0
                                      "the container loses group/other access")
                        (check-equal? (bitwise-and (file-or-directory-permissions root 'bits) #o077)
                                      0
                                      "the session root loses group/other access")
                        (reset-session-scratch-registry!)))
                    (lambda () (cleanup base))))

    (test-case "containment helper respects component boundaries"
      (define root (build-path (find-system-path 'temp-dir) "q-agent-scratch" "0123456789abcdef"))
      (check-true (scratch-path-within? (build-path root "x.log") root))
      (check-true (scratch-path-within? root root))
      (check-false (scratch-path-within? (build-path root ".." "escape.log") root))
      (check-false
       (scratch-path-within?
        (build-path (find-system-path 'temp-dir) "q-agent-scratch" "0123456789abcdef-suffix" "x.log")
        root)))

    (test-case "RED-FIRST: containment resolves symlinks instead of comparing strings"
      ;; A lexical prefix check sanctions `>root/escape-dir/pwned` even when
      ;; `escape-dir` is a symlink to a foreign directory, because bash follows
      ;; the link. Containment is therefore decided on canonical spellings.
      (define base (fresh-base-dir))
      (define foreign (make-temporary-file "q-scratch-sym-foreign-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base])
           (reset-session-scratch-registry!)
           (define root (allocate-session-scratch-root! "session-sym" "0123456789abcdef"))
           (define victim (build-path foreign "victim.txt"))
           (display-to-file "original\n" victim #:exists 'replace)
           (make-file-or-directory-link foreign (build-path root "escape-dir"))
           (make-file-or-directory-link victim (build-path root "victim-link"))
           ;; Plain paths below the root stay inside.
           (check-true (scratch-path-within? (build-path root "plain.log") root))
           ;; A directory symlink inside the root is outside it.
           (check-false (scratch-path-within? (build-path root "escape-dir" "pwned.txt") root)
                        "a symlinked subdirectory must not extend the root")
           ;; A file symlink at the target itself is outside it.
           (check-false (scratch-path-within? (build-path root "victim-link") root)
                        "a symlinked target must not extend the root")
           ;; A symlink to a sibling allocation path is outside it too.
           (check-false (scratch-path-within? (build-path root ".." "fedcba9876543210" "x") root))
           ;; Canonical spellings of the SAME directory remain inside both ways.
           (define canonical-root (canonical-scratch-path root))
           (check-true (scratch-path-within? (build-path canonical-root "plain.log") root))
           (check-true (scratch-path-within? (build-path root "plain.log") canonical-root))
           (reset-session-scratch-registry!)))
       (lambda ()
         (cleanup base)
         (delete-directory/files foreign #:must-exist? #f))))

    (test-case "RED-FIRST: worker scratch validation demands an existing private allocation"
      ;; The worker used to accept the session root's SHAPE. That is not enough
      ;; on the plane that actually writes: the base and the session directory
      ;; must already exist as real (non-symlink) owner-only directories, and
      ;; the canonical root must still sit below the canonical base.
      (define real-parent (make-temporary-file "q-scratch-validate-parent-~a" 'directory))
      (define alias-parent (make-temporary-file "q-scratch-validate-alias-~a" 'directory))
      (define foreign (make-temporary-file "q-scratch-validate-foreign-~a" 'directory))
      (define token "0123456789abcdef")
      (define real-base (build-path real-parent "q-agent-scratch"))
      (define real-root (build-path real-base token))
      (define alias (build-path alias-parent "alias"))
      (make-file-or-directory-link real-parent alias)
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir real-base])
           ;; Missing session root: refuse (the worker never creates it).
           (check-false (existing-trusted-session-scratch-root? real-root))
           (make-directory* real-base)
           (file-or-directory-permissions real-base #o700)
           (make-directory* real-root)
           (file-or-directory-permissions real-root #o700)
           (check-true (existing-trusted-session-scratch-root? real-root))
           ;; Group/other access on the session root: refuse.
           (file-or-directory-permissions real-root #o755)
           (check-false (existing-trusted-session-scratch-root? real-root))
           (file-or-directory-permissions real-root #o700)
           (check-true (existing-trusted-session-scratch-root? real-root))
           ;; Group/other access on the container: refuse.
           (file-or-directory-permissions real-base #o755)
           (check-false (existing-trusted-session-scratch-root? real-root))
           (file-or-directory-permissions real-base #o700)
           ;; The base itself, a nested path and a sibling token are refused.
           (check-false (existing-trusted-session-scratch-root? real-base))
           (check-false (existing-trusted-session-scratch-root? (build-path real-root "child")))
           (check-false (existing-trusted-session-scratch-root? (build-path real-base
                                                                            "fedcba9876543210")))
           (check-false (existing-trusted-session-scratch-root? #f))
           ;; A non-symlink container REACHED through a symlinked ancestor is
           ;; the same directory: the alias and the physical spelling agree.
           (define alias-base (build-path alias "q-agent-scratch"))
           (parameterize ([current-scratch-base-dir alias-base])
             (check-true (existing-trusted-session-scratch-root? (build-path alias-base token)))
             (check-true (existing-trusted-session-scratch-root? real-root)))
           ;; A symlinked allocation CONTAINER is refused, never followed.
           (parameterize ([current-scratch-base-dir alias])
             (check-false (existing-trusted-session-scratch-root? (build-path alias token))))
           ;; A symlinked session directory is refused even when it points at
           ;; a real private directory.
           (define link-root (build-path real-base "fedcba9876543210"))
           (make-file-or-directory-link foreign link-root)
           (check-false (existing-trusted-session-scratch-root? link-root))))
       (lambda ()
         (delete-directory/files real-parent #:must-exist? #f)
         (delete-directory/files alias-parent #:must-exist? #f)
         (delete-directory/files foreign #:must-exist? #f))))

    (test-case "canonical-scratch-path resolves the existing prefix only"
      (define dir (make-temporary-file "q-scratch-canonical-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define real (build-path dir "real"))
         (make-directory* real)
         (define link (build-path dir "link"))
         (make-file-or-directory-link real link)
         (check-equal? (path->string (canonical-scratch-path (build-path link "new.log")))
                       (path->string (build-path (canonical-scratch-path real) "new.log"))
                       "an existing symlinked prefix resolves; a missing tail stays lexical")
         ;; A dangling link is followed to the lexical target the OS would
         ;; create: outside the root stays outside, inside stays inside.
         (define outside-target (build-path dir "outside.txt"))
         (define inside-target (build-path real "inside.txt"))
         (make-file-or-directory-link outside-target (build-path real "dangling-out"))
         (make-file-or-directory-link inside-target (build-path real "dangling-in"))
         (check-false (scratch-path-within? (build-path real "dangling-out") real))
         (check-true (scratch-path-within? (build-path real "dangling-in") real))
         ;; A self-referential link must fail closed, never spin: `resolve-path`
         ;; makes no progress on a cycle, so the walk remembers resolved links.
         (define loop (build-path real "loop"))
         (make-file-or-directory-link loop loop)
         (check-false (canonical-scratch-path loop) "a link cycle has no canonical spelling")
         (check-false (scratch-path-within? (build-path loop "x.log") real)
                      "a link cycle is outside every root"))
       (lambda () (delete-directory/files dir #:must-exist? #f))))))

(module+ test
  (run-tests scratch-suite))

(module+ main
  (run-tests scratch-suite))
