#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @isolation subprocess
;; @covers extensions/gsd/review-tools.rkt extensions/gsd/review-session.rkt
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         "../tools/tool.rkt"
         "../sandbox/subprocess.rkt"
         (submod "../extensions/gsd/review-session.rkt" test-support))

(define (scratch f)
  (define root (make-temporary-file "review-tools-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))

(define (invoke registry command [ctx #f])
  ((tool-execute (lookup-tool registry "bash")) (hasheq 'command command) ctx))
(define (result-text result) (format "~a" (tool-result-content result)))
(define (put path text)
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))
(define (git! root . args)
  (define result
    (run-subprocess "/usr/bin/git" #:directory root #:timeout 5
                    #:environment (make-environment-variables #"PATH" #"/usr/bin:/bin" #"HOME" #"/dev/null"
                                                              #"GIT_CONFIG_NOSYSTEM" #"1" #"GIT_CONFIG_GLOBAL" #"/dev/null")
                    #:args (append '("-c" "user.name=Review fixture" "-c" "user.email=review@example.invalid") args)))
  (unless (equal? 0 (subprocess-result-exit-code result))
    (error 'git-fixture "~a: ~a" args (subprocess-result-stderr result)))
  (subprocess-result-stdout result))
(define (repository root)
  (git! root "init" "-q")
  (put (build-path root "source.txt") "source needle\nsecond line\n")
  (put (build-path root ".gitattributes") "*.txt diff=reviewtrap\n")
  (git! root "add" "source.txt" ".gitattributes")
  (git! root "commit" "-qm" "baseline")
  (put (build-path root "source.txt") "changed needle\nsecond line\n"))

(module+ test
  (test-case "review registry cannot redirect output into a file (behavioral RED)"
    (scratch
     (lambda (root)
       (define registry (make-review-tool-registry root))
       (define target (build-path root "must-not-exist"))
       (define result
         (parameterize ([current-directory root])
           ((tool-execute (lookup-tool registry "bash"))
            (hasheq 'command (format "printf reviewer-write > ~a" target))
            (make-exec-context #:working-directory root))))
       (check-false (file-exists? target) "reviewer tool executed a filesystem write")
       (check-true (tool-result-is-error? result)))))

  (test-case "registry exposes only non-externalizable read/bash inspectors"
    (scratch
     (lambda (root)
       (define registry (make-review-tool-registry root))
       (check-equal? (sort (map tool-name (list-tools registry)) string<?) '("bash" "read"))
       (for ([tool (in-list (list-tools registry))])
         (check-false (tool-externalizable? tool))
         (check-false (tool-mutates-filesystem? tool))
         (check-equal? (tool-required-capability tool) 'read-only)))))

  (test-case "actual write, shell, interpreter, PTY, alias and output attempts are blocked without effects"
    (scratch
     (lambda (root)
       (repository root)
       (define registry (make-review-tool-registry root))
       (define marker (build-path root "must-not-exist"))
       (define commands
         (list (format "touch ~a" marker)
               (format "python -c \"import pathlib; pathlib.Path('~a').touch()\"" marker)
               "python -c 'import pty; pty.spawn(\"/bin/sh\")'"
               "python3 -c 'import pty; pty.spawn(\"/bin/sh\")'"
               "python3 -c \"import pty; pty.spawn(['sh','-c','touch must-not-exist'])\""
               "racket -e '(system \"touch must-not-exist\")'"
               "sh -c 'touch must-not-exist'" "bash -c 'touch must-not-exist'"
               "env touch must-not-exist" "exec touch must-not-exist" "q /go approve" "gh pr review --approve"
               "git -c 'alias.approve=!touch must-not-exist' approve" "git --git-dir=/tmp status --short"
               "git config core.fsmonitor helper" "git checkout HEAD" "git update-ref refs/heads/forged HEAD"
               "git show --output=must-not-exist HEAD" "git diff --output=must-not-exist"
               "git diff --ext-diff" "git diff --textconv" "git show --textconv HEAD:source.txt"
               "git log --output must-not-exist" "git log --format=%G?" "git log --show-signature"
               "git diff HEAD -- ../outside" "git diff HEAD -- --output=must-not-exist"
               "git diff --no-ext-diff --no-textconv --output=must-not-exist"
               "git show HEAD:../outside" "git show HEAD:--output=must-not-exist"
               "ls --dired" "cat --output=must-not-exist source.txt" "wc --files0-from=source.txt"
               "tail -f source.txt" "head -n 100000 source.txt"
               "grep --include=source.txt needle ." "rg --pre=touch needle source.txt"
               "rg --pre touch needle source.txt" "rg --files --hidden"
               "pwd; touch must-not-exist" "pwd && touch must-not-exist" "pwd | sh"
               "pwd\ntouch must-not-exist" "pwd\rtouch must-not-exist"
               "cat < source.txt" "cat source.txt > must-not-exist" "cat source.txt >> must-not-exist"
               "ls `touch must-not-exist`" "ls $(touch must-not-exist)" "ls \\; touch must-not-exist"
               "ls *.txt" "cat 'unclosed"))
       (for ([command (in-list commands)])
         (check-true (tool-result-is-error? (invoke registry command)) command)
         (check-false (file-exists? marker) command))
       (check-equal? (file->string (build-path root "source.txt")) "changed needle\nsecond line\n")
       (check-equal? (string-trim (git! root "branch" "--list" "forged")) "")
       (check-equal? (string-trim (git! root "status" "--short")) "M source.txt"))))

  (test-case "cwd is pinned, argument overrides fail closed, and relative read uses checkout"
    (scratch
     (lambda (root)
       (define checkout (build-path root "checkout"))
       (make-directory checkout)
       (put (build-path checkout "source.txt") "pinned source")
       (put (build-path root "source.txt") "wrong source")
       (define registry (make-review-tool-registry checkout))
       (define hostile-ctx (make-exec-context #:working-directory root))
       (parameterize ([current-directory root])
         (check-equal? (string-trim (result-text (invoke registry "pwd" hostile-ctx)))
                       (path->string (simplify-path checkout)))
         (check-true (string-contains? (result-text (invoke registry "cat source.txt" hostile-ctx)) "pinned source"))
         (define read-result
           ((tool-execute (lookup-tool registry "read")) (hasheq 'path "source.txt") hostile-ctx))
         (check-false (tool-result-is-error? read-result))
         (check-true (string-contains? (result-text read-result) "pinned source")))
       (for ([key '(cwd working-directory directory environment env args)])
         (check-true
          (tool-result-is-error?
           ((tool-execute (lookup-tool registry "bash")) (hasheq 'command "pwd" key (path->string root)) hostile-ctx))))
       (for ([timeout '(0 -1 11 +inf.0 +nan.0 "1")])
         (check-true (tool-result-is-error?
                      ((tool-execute (lookup-tool registry "bash")) (hasheq 'command "pwd" 'timeout timeout))))))))

  (test-case "read inspects absolute private retained proof, RED manifest and raw outputs outside checkout"
    (scratch
     (lambda (root)
       (define checkout (build-path root "checkout"))
       (define proof-root (build-path root ".q" "agent" "owned-review-proofs" (make-string 64 #\a)))
       (make-directory checkout)
       (make-directory* proof-root)
       (define registry (make-review-tool-registry checkout))
       (for ([name (list (string-append (make-string 64 #\b) ".json")
                        (string-append (make-string 64 #\c) ".log")
                        (string-append (make-string 64 #\d) ".json"))]
             [text '("{\"owned-proof\":true}" "actual raw RED/gate failure output" "{\"owned-red\":true}")])
         (define path (build-path proof-root name))
         (put path text)
         (file-or-directory-permissions path #o400)
         (define result ((tool-execute (lookup-tool registry "read")) (hasheq 'path (path->string path))))
         (check-false (tool-result-is-error? result))
         (check-true (string-contains? (result-text result) text))))))

  (test-case "fixed Git facts and literal source inspection remain available"
    (scratch
     (lambda (root)
       (repository root)
       (git! root "commit" "--allow-empty" "-qm" "second baseline")
       (put (build-path root "space name.txt") "literal needle\nlast line\n")
       (define registry (make-review-tool-registry root))
       (for ([command '("pwd" "git status --short" "git status --short --branch"
                        "git rev-parse HEAD" "git rev-parse HEAD^{tree}" "git rev-parse --show-toplevel"
                        "git diff" "git diff --stat HEAD" "git diff --name-only HEAD -- source.txt"
                        "git diff --no-ext-diff --no-textconv HEAD^ HEAD -- source.txt"
                        "git show HEAD" "git show HEAD:source.txt" "git log" "git log --oneline" "git log -n 1"
                        "ls -la" "cat -- 'space name.txt'" "wc -l source.txt"
                        "head -n 1 source.txt" "tail -n 1 source.txt" "grep -n -F needle source.txt")])
         (define result (invoke registry command))
         (check-false (tool-result-is-error? result) (format "~a: ~a" command (result-text result))))
       (when (file-exists? "/usr/bin/rg")
         (check-false (tool-result-is-error? (invoke registry "rg -n needle source.txt"))))
       (check-true (string-contains? (result-text (invoke registry "git show HEAD:source.txt")) "source needle"))
       (check-true (string-contains? (result-text (invoke registry "git diff HEAD -- source.txt")) "+changed needle"))
       (define missing (invoke registry "cat missing.txt"))
       (check-true (tool-result-is-error? missing))
       (check-not-equal? (hash-ref (tool-result-details missing) 'exit-code) 0))))

  (test-case "subprocess output and blocking reads are bounded with honest result metadata"
    (scratch
     (lambda (root)
       (define registry (make-review-tool-registry root))
       (put (build-path root "large.txt") (make-string (* 128 1024) #\x))
       (define large (invoke registry "cat large.txt"))
       (check-false (tool-result-is-error? large))
       (check-true (hash-ref (tool-result-details large) 'truncated))
       (check-true (< (string-length (result-text large)) 70000))
       (define fifo (build-path root "blocked-read"))
       (define setup (run-subprocess "/usr/bin/mkfifo" #:args (list (path->string fifo)) #:timeout 1))
       (check-equal? (subprocess-result-exit-code setup) 0)
       (define started (current-inexact-monotonic-milliseconds))
       (define blocked
         ((tool-execute (lookup-tool registry "bash")) (hasheq 'command "cat blocked-read" 'timeout 0.05)))
       (check-true (tool-result-is-error? blocked))
       (check-true (hash-ref (tool-result-details blocked) 'timed-out))
       ;; run-subprocess also has a bounded TERM/KILL and reader cleanup grace.
       (check-true (< (- (current-inexact-monotonic-milliseconds) started) 6000)))))

  (test-case "unsafe Git filter names fail closed while read remains available"
    (scratch
     (lambda (root)
       (repository root)
       (git! root "config" "filter.unusual/name.clean" "touch must-not-exist")
       (define registry (make-review-tool-registry root))
       (check-true (tool-result-is-error? (invoke registry "git diff HEAD")))
       (check-false (file-exists? (build-path root "must-not-exist")))
       (check-false (tool-result-is-error?
                     ((tool-execute (lookup-tool registry "read")) (hasheq 'path "source.txt")))))))

  (test-case "missing promisor objects cannot trigger fetching or remote helpers"
    (scratch
     (lambda (root)
       (repository root)
       (define blob (string-trim (git! root "rev-parse" "HEAD:source.txt")))
       (delete-file (build-path root ".git" "objects" (substring blob 0 2) (substring blob 2)))
       (define marker (build-path root "remote-helper-ran"))
       (define helper (build-path root "remote-helper"))
       (put helper (format "#!/bin/sh\nprintf remote > '~a'\n" marker))
       (file-or-directory-permissions helper #o700)
       (git! root "config" "remote.review.promisor" "true")
       (git! root "config" "remote.review.url" (string-append "ext::" (path->string helper)))
       (define registry (make-review-tool-registry root))
       (check-true (tool-result-is-error? (invoke registry "git show HEAD:source.txt")))
       (check-false (file-exists? marker))
       (check-false (file-exists? (build-path root ".git" "FETCH_HEAD"))))))

  (test-case "repository config and inherited Git/search environment cannot launch hooks or redirect writes"
    (scratch
     (lambda (root)
       (repository root)
       (define registry (make-review-tool-registry root))
       (define marker (build-path root "hook-ran"))
       (define helper (build-path root "helper"))
       (put helper (format "#!/bin/sh\nprintf hook > '~a'\n" marker))
       (file-or-directory-permissions helper #o700)
       (for ([setting '("core.fsmonitor" "core.pager" "diff.reviewtrap.command" "diff.reviewtrap.textconv" "diff.external")])
         (git! root "config" setting (path->string helper)))
       (put (build-path root ".gitattributes") "*.txt diff=reviewtrap filter=reviewtrap\n")
       (git! root "config" "filter.reviewtrap.clean" (path->string helper))
       (git! root "config" "filter.reviewtrap.process" (path->string helper))
       (git! root "config" "filter.reviewtrap.required" "true")
       (git! root "config" "log.showSignature" "true")
       (git! root "config" "alias.approve" (string-append "!" (path->string helper)))
       (define config (build-path root "rg-config"))
       (put config (format "--pre=~a\n" helper))
       (define trace (build-path root "trace-must-not-exist"))
       (define index (build-path root "index-must-not-exist"))
       (define env (environment-variables-copy (current-environment-variables)))
       (for ([key '(#"GIT_DIR" #"GIT_WORK_TREE" #"GIT_CONFIG_COUNT" #"GIT_CONFIG_KEY_0" #"GIT_CONFIG_VALUE_0"
                              #"GIT_EXTERNAL_DIFF" #"GIT_PAGER" #"GIT_TRACE" #"GIT_TRACE2_EVENT" #"GIT_INDEX_FILE"
                              #"RIPGREP_CONFIG_PATH" #"PATH")]
             [value (list #"/missing-repo" #"/missing-worktree" #"1" #"core.fsmonitor" (path->bytes helper)
                          (path->bytes helper) (path->bytes helper) (path->bytes trace) (path->bytes trace) (path->bytes index)
                          (path->bytes config) (path->bytes root))])
         (environment-variables-set! env key value))
       (parameterize ([current-environment-variables env])
         (for ([command '("git status --short" "git diff HEAD -- source.txt" "git show HEAD" "git show HEAD:source.txt"
                          "git log --oneline" "git rev-parse HEAD")])
           (define result (invoke registry command))
           (check-false (tool-result-is-error? result) (format "~a: ~a" command (result-text result))))
         (check-true (tool-result-is-error? (invoke registry "git -c alias.approve=!helper approve")))
         (check-true (tool-result-is-error? (invoke registry "git approve")))
         (when (file-exists? "/usr/bin/rg")
           (check-false (tool-result-is-error? (invoke registry "rg needle source.txt")))))
       (check-false (file-exists? marker))
       (check-false (file-exists? trace))
       (check-false (file-exists? index))))))
