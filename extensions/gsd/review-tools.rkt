#lang racket/base
;; STABILITY: internal
;; Reviewer authority is read-only by construction, not by prompt or auto-approval.
;; "bash" is a compatibility name, NOT a shell. Keep this grammar deliberately small;
;; unsupported inspection should use read instead of growing a shell/option interpreter.
(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         "../../tools/tool.rkt"
         "../../tools/registry-defaults.rkt"
         "../../sandbox/subprocess.rkt"
         (only-in "../../sandbox/limits.rkt" strict-exec-limits))
(provide make-review-tool-registry)

(define (refuse detail)
  (error 'review-tools "Blocked read-only reviewer command: ~a. Use read for source/proof/output files." detail))

;; No expansion, escaping, operators, substitutions, globbing, or multiline commands.
;; Quotes only group literal arguments (including paths with spaces).
(define (command->argv command)
  (unless (and (string? command) (<= 1 (string-length command) 8192))
    (refuse "command must be a nonempty string of at most 8192 characters"))
  (when (for/or ([c (in-string command)])
          (or (memv c '(#\nul #\newline #\return #\; #\| #\& #\< #\> #\` #\$ #\\ #\( #\)))
              (and (char-iso-control? c) (not (char=? c #\tab)))))
    (refuse "shell syntax/control characters are not supported"))
  (define words '())
  (define out (open-output-string))
  (define quote-char #f)
  (define started? #f)
  (define (flush!)
    (when started?
      (set! words (cons (get-output-string out) words))
      (set! out (open-output-string))
      (set! started? #f)))
  (for ([c (in-string command)])
    (cond
      [quote-char
       (if (char=? c quote-char) (set! quote-char #f) (write-char c out))]
      [(memv c '(#\' #\")) (set! quote-char c) (set! started? #t)]
      [(char-whitespace? c) (flush!)]
      [else (write-char c out) (set! started? #t)]))
  (when quote-char (refuse "unclosed quote"))
  (flush!)
  (when (null? words) (refuse "empty command"))
  (reverse words))

(define (literal-path? s)
  (and (string? s) (non-empty-string? s)
       (not (string-prefix? s "-"))
       (not (regexp-match? #px"[*?\\[\\]{}]" s))))
(define (paths! paths)
  (unless (and (pair? paths) (andmap literal-path? paths))
    (refuse "expected literal paths, not flags or glob patterns"))
  paths)
(define (strip-divider args)
  (if (and (pair? args) (equal? (car args) "--")) (cdr args) args))
(define (absolute-paths cwd paths)
  (map (lambda (p) (path->string (path->complete-path p cwd))) (paths! paths)))
(define (count! n)
  (unless (and (regexp-match? #px"^[0-9]{1,4}$" n) (<= 1 (string->number n) 1000))
    (refuse "count must be between 1 and 1000"))
  n)

;; Revisions cannot introduce flags, arbitrary revision expressions, reflog lookups, or paths.
(define (revision? s)
  (regexp-match? #px"^(?:HEAD(?:[~^][0-9]{0,3})*|[0-9a-fA-F]{7,64})$" s))
(define (git-paths! paths)
  (paths! paths)
  (unless (for/and ([p (in-list paths)])
            (and (relative-path? p)
                 (not (string-prefix? p ":"))
                 (not (member ".." (string-split p "/")))))
    (refuse "Git paths must be literal checkout-relative paths without .."))
  paths)
(define diff-flags '("--no-ext-diff" "--no-textconv" "--no-color" "--no-renames" "--ignore-submodules=all"))
;; Fixed trusted options override executable hooks/helpers in repository config.
;; No user-supplied global options or arbitrary log/diff/show flags are forwarded.
(define git-prefix
  '("--no-pager" "--no-optional-locks"
    "-c" "core.fsmonitor=false" "-c" "core.hooksPath=/dev/null"
    "-c" "core.attributesFile=/dev/null" "-c" "core.pager=cat"
    "-c" "diff.external=" "-c" "log.showSignature=false"
    "-c" "protocol.allow=never" "-c" "maintenance.auto=false" "-c" "gc.auto=0"))
(define (git-argv args)
  (define payload
    (match args
      [(list "status") '("status" "--short" "--untracked-files=no" "--ignore-submodules=all")]
      [(list "status" "--short") '("status" "--short" "--untracked-files=no" "--ignore-submodules=all")]
      [(list "status" "--short" "--branch")
       '("status" "--short" "--branch" "--untracked-files=no" "--ignore-submodules=all")]
      [(list "rev-parse" (and fact (or "HEAD" "HEAD^{tree}" "--show-toplevel"))) (list "rev-parse" fact)]
      [(list "log") '("log" "-10" "--format=fuller" "--no-show-signature" "--no-decorate")]
      [(list "log" "--oneline") '("log" "-10" "--oneline" "--no-show-signature" "--no-decorate")]
      [(list "log" "-n" n)
       (count! n)
       (list "log" "-n" n "--format=fuller" "--no-show-signature" "--no-decorate")]
      [(list "show" spec)
       (define parts (string-split spec ":" #:trim? #f))
       (unless (and (<= 1 (length parts) 2) (revision? (car parts))) (refuse "unsupported Git show revision"))
       (when (= (length parts) 2) (git-paths! (cdr parts)))
       (append '("show" "--format=fuller" "--no-show-signature" "--no-decorate") diff-flags (list spec "--"))]
      [(list* "diff" rest)
       ;; Accept these compatibility spellings, but always inject the protection ourselves.
       (define without-safety
         (if (and (>= (length rest) 2) (equal? (take rest 2) '("--no-ext-diff" "--no-textconv")))
             (drop rest 2) rest))
       (define style
         (and (pair? without-safety) (member (car without-safety) '("--stat" "--name-only")) (car without-safety)))
       (define body (if style (cdr without-safety) without-safety))
       (define split-at-path (index-of body "--"))
       (define revisions (if split-at-path (take body split-at-path) body))
       (define paths (if split-at-path (git-paths! (drop body (add1 split-at-path))) '()))
       (unless (and (<= (length revisions) 2) (andmap revision? revisions)) (refuse "unsupported Git diff shape"))
       (append '("diff") diff-flags (if style (list style) '()) revisions '("--") paths)]
      [_ (refuse "unsupported Git fact; use status --short, diff [revisions] [-- paths], show REV[:path], or log")]))
  (append git-prefix payload))

(define (inspection-argv cwd program args)
  (match program
    ["git" (git-argv args)]
    ["ls"
     (define flag (and (pair? args) (member (car args) '("-l" "-a" "-la" "-al" "-1")) (car args)))
     (define paths (strip-divider (if flag (cdr args) args)))
     (append (if flag (list flag) '()) '("--") (absolute-paths cwd (if (null? paths) '(".") paths)))]
    ["cat" (append '("--") (absolute-paths cwd (strip-divider args)))]
    ["wc"
     (define flag (and (pair? args) (member (car args) '("-l" "-c" "-w")) (car args)))
     (append (if flag (list flag) '()) '("--") (absolute-paths cwd (strip-divider (if flag (cdr args) args))))]
    [(or "head" "tail")
     (define counted? (and (pair? args) (equal? (car args) "-n")))
     (when (and counted? (< (length args) 3)) (refuse "expected -n COUNT and literal paths"))
     (define n (if counted? (count! (cadr args)) "20"))
     (append (list "-n" n "--") (absolute-paths cwd (strip-divider (if counted? (cddr args) args))))]
    [(or "grep" "rg")
     (define body (let loop ([xs args])
                    (if (and (pair? xs) (member (car xs) '("-n" "-F" "-nF" "-Fn"))) (loop (cdr xs)) xs)))
     (define pattern-and-paths (strip-divider body))
     (unless (and (>= (length pattern-and-paths) 2) (literal-path? (car pattern-and-paths)))
       (refuse "search expects a literal pattern and explicit paths; only -n/-F are supported"))
     (append (if (equal? program "rg") '("--no-config" "--color=never" "--no-heading") '())
             '("-n" "-F" "--") (list (car pattern-and-paths)) (absolute-paths cwd (cdr pattern-and-paths)))]
    [_ (refuse "only pwd, fixed Git facts, ls, cat, wc, head, tail, grep, and rg are supported")]))

;; Start empty: inherited GIT_*, loaders, shell startup, credential agents, PATH,
;; ripgrep config, and locale hooks never reach the child. No secrets are authorized
;; merely because read can inspect controller-provided absolute private-proof paths.
(define (inspection-environment)
  (make-environment-variables
   #"PATH" #"/usr/bin:/bin" #"HOME" #"/dev/null" #"XDG_CONFIG_HOME" #"/dev/null" #"LC_ALL" #"C"
   #"GIT_CONFIG_NOSYSTEM" #"1" #"GIT_CONFIG_SYSTEM" #"/dev/null" #"GIT_CONFIG_GLOBAL" #"/dev/null"
   #"GIT_ATTR_NOSYSTEM" #"1" #"GIT_TERMINAL_PROMPT" #"0" #"GIT_OPTIONAL_LOCKS" #"0"
   #"GIT_NO_REPLACE_OBJECTS" #"1" #"GIT_NO_LAZY_FETCH" #"1" #"GIT_LITERAL_PATHSPECS" #"1"))
;; --no-ext-diff/--no-textconv do NOT disable clean/process filters used while
;; comparing worktree files. Read config without executing helpers, then override
;; every filter. Unsupported subsection names fail closed instead of becoming argv.
;; The pinned checkout/config must remain controller-owned during inspection, as
;; for checkout authentication itself; this is not isolation from a concurrent OS user.
(define (git-filter-overrides cwd timeout)
  (define result
    (run-subprocess (trusted-executable "git") #:directory cwd #:environment (inspection-environment)
                    #:limits (strict-exec-limits) #:timeout (min timeout 1)
                    #:args (append git-prefix '("config" "--null" "--name-only" "--get-regexp" "^filter\\."))))
  (unless (and (not (subprocess-result-timed-out? result)) (not (subprocess-result-truncated? result))
               (member (subprocess-result-exit-code result) '(0 1)))
    (refuse "could not safely inspect Git filter configuration"))
  (define names
    (for/list ([key (in-list (string-split (subprocess-result-stdout result) (string #\nul)))])
      (define m (regexp-match #px"^filter\\.([a-zA-Z0-9_.-]+)\\.[a-zA-Z][a-zA-Z0-9-]*$" key))
      (unless m (refuse "unsupported Git filter configuration name"))
      (cadr m)))
  (append-map (lambda (name)
                (append-map (lambda (setting) (list "-c" (string-append "filter." name "." setting)))
                            '("clean=" "smudge=" "process=" "required=false")))
              (remove-duplicates names)))
(define (trusted-executable program)
  (or (for/first ([dir '("/usr/bin" "/bin")]
                 #:when (file-exists? (build-path dir program)))
        (path->string (build-path dir program)))
      (refuse "inspection executable is unavailable")))
(define (validate-keys! args allowed)
  (unless (and (hash? args) (for/and ([key (in-hash-keys args)]) (memq key allowed)))
    (refuse "unsupported arguments (cwd/environment overrides are forbidden)")))
(define (make-review-tool-registry checkout)
  (define cwd (simplify-path (path->complete-path checkout) #t))
  (unless (directory-exists? cwd) (raise-argument-error 'make-review-tool-registry "existing checkout directory" checkout))
  (define registry (make-tool-registry))
  (register-default-tools! registry #:only '("read"))
  (define builtin-read (lookup-tool registry "read"))
  (register-tool!
   registry
   (make-tool
    "read" (tool-description builtin-read) (tool-schema builtin-read)
    (lambda (args [ctx #f])
      (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
        (validate-keys! args '(path offset limit))
        ;; Builtin read intentionally allows absolute retained-proof paths outside
        ;; the checkout. Pin only relative resolution; do not claim a secret ACL.
        (parameterize ([current-directory cwd]) ((tool-execute builtin-read) args ctx))))
    #:required-capability 'read-only))
  (register-tool!
   registry
   (make-tool
    "bash"
    "Read-only inspection, not a shell: pwd; fixed git status/diff/show/log/rev-parse; ls/cat/wc/head/tail; literal grep/rg. Use read otherwise."
    (hasheq 'type "object" 'required '("command")
            'properties (hasheq 'command (hasheq 'type "string" 'description "One supported read-only inspection command")
                                'timeout (hasheq 'type "number" 'description "Seconds, positive and at most 10")))
    (lambda (args [ctx #f])
      (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
        (validate-keys! args '(command timeout))
        (define timeout (hash-ref args 'timeout 5))
        (unless (and (real? timeout) (< 0 timeout) (<= timeout 10)) (refuse "timeout must be positive and at most 10 seconds"))
        (define argv (command->argv (hash-ref args 'command #f)))
        (define program (car argv))
        (cond
          [(equal? argv '("pwd")) (make-success-result (string-append (path->string cwd) "\n"))]
          [else
           (define safe-args (inspection-argv cwd program (cdr argv)))
           (define started (current-inexact-monotonic-milliseconds))
           (define overrides (if (equal? program "git") (git-filter-overrides cwd timeout) '()))
           (define remaining (- timeout (/ (- (current-inexact-monotonic-milliseconds) started) 1000)))
           (unless (> remaining 0) (refuse "inspection deadline exceeded during Git configuration check"))
           (define result
             (run-subprocess (trusted-executable program) #:args (append overrides safe-args) #:directory cwd
                             #:environment (inspection-environment) #:limits (strict-exec-limits) #:timeout remaining
                             #:process-group? #f)) ; DIRECT execution, never the process-group shell supervisor.
           (make-tool-result
            (string-append (subprocess-result-stdout result) (subprocess-result-stderr result))
            (hasheq 'exit-code (subprocess-result-exit-code result)
                    'timed-out (subprocess-result-timed-out? result) 'truncated (subprocess-result-truncated? result))
            (or (subprocess-result-timed-out? result) (not (equal? 0 (subprocess-result-exit-code result)))))])))
    #:required-capability 'read-only
    #:timeout-seconds 10))
  registry)
