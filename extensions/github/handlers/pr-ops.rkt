#lang racket/base

;; extensions/github/handlers/pr-ops.rkt — GitHub PR tool handler
;;
;; Extracted from tool-handlers.rkt. Pure handler logic for gh-pr actions.

(require racket/format
         racket/string
         racket/port
         racket/file
         json
         (only-in "../helpers.rkt"
                  with-error-result
                  gh-binary
                  gh-unavailable-error
                  gh-exec-result
                  git-exec-result
                  valid-number?
                  valid-method?
                  valid-state?
                  gh-success-json)
         (only-in "../../tool-api.rkt" make-error-result))

(provide handle-gh-pr
         pr-lookup-command
         pr-exists-for-head?
         find-open-pr-for-head
         open-pr-from-lookup
         unrelated-staged-paths
         wave-already-committed?
         wave-checkpoint-section
         read-state-content
         read-wave-checkpoints
         wave-step-completed?
         write-wave-checkpoint!)

;; W6 (BUG-0011): lookup-first idempotence for `gh-pr create`.
;; `pr-lookup-command` builds the gh args that find an existing OPEN PR
;; for a head branch; `open-pr-from-lookup` parses the lookup stdout.
;; Both are pure functions so idempotence is unit-testable without a
;; live `gh` binary; `pr-exists-for-head?` runs the lookup for real.
(define (pr-lookup-command head)
  (list "pr" "list" "--state" "open" "--head" head "--limit" "1" "--json" "number,title,url,state"))

(define (open-pr-from-lookup stdout)
  ;; "[]" (or whitespace/empty) means no open PR; any non-empty JSON
  ;; array with at least one element means one exists.
  (define raw (string-trim (or stdout "")))
  (and (>= (string-length raw) 2)
       (string-prefix? raw "[")
       (not (string=? raw "[]"))
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define parsed (string->jsexpr raw))
         (and (list? parsed) (pair? parsed) (car parsed)))))

(define (checked-open-pr-from-lookup stdout)
  (define raw (string-trim (or stdout "")))
  (define parsed
    (with-handlers ([exn:fail? (lambda (_)
                                 (raise-user-error 'github-pr "PR lookup returned invalid JSON"))])
      (string->jsexpr raw)))
  (cond
    [(and (list? parsed) (null? parsed)) #f]
    [(and (list? parsed) (pair? parsed) (hash? (car parsed))) (car parsed)]
    [else (raise-user-error 'github-pr "PR lookup returned an unexpected response")]))

(define (find-open-pr-for-head head #:gh [gh-fn gh-exec-result])
  (define-values (ec out err) (apply gh-fn (pr-lookup-command head)))
  (unless (= ec 0)
    (raise-user-error 'github-pr "PR lookup failed: ~a" (string-trim err)))
  (checked-open-pr-from-lookup out))

(define (pr-exists-for-head? head)
  (find-open-pr-for-head head))

(define (handle-gh-pr args [exec-ctx #f])
  (with-error-result
   "github operation"
   (cond
     [(not (gh-binary)) (gh-unavailable-error)]
     [else
      (define action (hash-ref args 'action ""))
      (cond
        [(string=? action "") (make-error-result "Missing required argument: action")]
        [(string=? action "create")
         (define title (hash-ref args 'title #f))
         (cond
           [(not title) (make-error-result "create requires 'title'")]
           [else
            (define body (hash-ref args 'body ""))
            (define head (hash-ref args 'head #f))
            (define base (hash-ref args 'base "main"))
            ;; W6 (BUG-0011) idempotence: lookup-first — if an open PR
            ;; already exists for the head branch, return it instead of
            ;; erroring on a duplicate create. Re-executing `create` for
            ;; the same head yields the same single PR, success both times.
            (cond
              [(and head (pr-exists-for-head? head))
               (gh-success-json "pr"
                                "list"
                                "--state"
                                "open"
                                "--head"
                                head
                                "--limit"
                                "1"
                                "--json"
                                "number,title,url,state")]
              [else
               (apply gh-success-json
                      (append (list "pr"
                                    "create"
                                    "--title"
                                    title
                                    "--body"
                                    body
                                    "--base"
                                    base
                                    "--json"
                                    "number,title,url")
                              (if head
                                  (list "--head" head)
                                  '())))])])]
        [(string=? action "merge")
         (define num (hash-ref args 'number #f))
         (cond
           [(not num) (make-error-result "merge requires 'number'")]
           [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
           [else
            (define method (hash-ref args 'method "squash"))
            (unless (valid-method? method)
              (raise-user-error 'github-pr "invalid merge method: ~a" method))
            (define commit-title (hash-ref args 'commit_title #f))
            (apply gh-success-json
                   (append (list "pr" "merge" (~a num) (string-append "--" method))
                           (if commit-title
                               (list "--subject" commit-title)
                               '())))])]
        [(string=? action "list")
         (define raw-state (hash-ref args 'state "open"))
         (unless (valid-state? raw-state)
           (raise-user-error 'github-pr "invalid state: ~a" raw-state))
         (gh-success-json "pr"
                          "list"
                          "--state"
                          raw-state
                          "--limit"
                          "100"
                          "--json"
                          "number,title,state,headRefName")]
        [(string=? action "get")
         (define num (hash-ref args 'number #f))
         (cond
           [(not num) (make-error-result "get requires 'number'")]
           [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
           [else
            (gh-success-json "pr"
                             "view"
                             (~a num)
                             "--json"
                             "number,title,state,headRefName,baseRefName,url")])]
        [else
         (make-error-result (format "Unknown action: ~a. Valid: create, merge, list, get"
                                    action))])])))

;; ============================================================
;; W6 (BUG-0011): gh-wave-finish idempotence + durable checkpoints
;; ============================================================

;; Tree/content check: the wave's change is already applied iff every
;; listed file is clean in the working tree — `git status --porcelain
;; -- <files>` reports nothing. Status failures are errors, never evidence
;; that a mutation is required.
(define (wave-already-committed? files #:git [git-fn #f])
  (define exec (or git-fn git-exec-result))
  (define-values (ec out err) (apply exec (append (list "status" "--porcelain" "--") files)))
  (unless (= ec 0)
    (raise-user-error 'gh-wave-finish "git status failed: ~a" (string-trim err)))
  (string=? (string-trim out) ""))

;; Return staged paths that are not in the explicit wave allowlist. Failure to
;; inspect the index is fail-closed because continuing could commit unrelated
;; work. File validation in the handler excludes embedded newlines.
(define (unrelated-staged-paths files #:git [git-fn #f])
  (define exec (or git-fn git-exec-result))
  (define-values (ec out err) (exec "diff" "--cached" "--name-only" "-z" "--"))
  (unless (= ec 0)
    (raise-user-error 'gh-wave-finish "staged-path lookup failed: ~a" (string-trim err)))
  ;; NUL-delimited output disables Git's quoting/escaping and preserves path
  ;; bytes represented by the process string, including whitespace and tabs.
  (define staged (filter (lambda (path) (not (string=? path ""))) (string-split out "\0" #:trim? #f)))
  (filter (lambda (path) (not (member path files))) staged))

;; Durable checkpoints: a structured per-wave done-steps checklist
;; stored in .planning/STATE.md. Format:
;;
;;   ## Wave checkpoints
;;
;;   ### W6
;;   - [x] step-id-1
;;   - [x] step-id-2
;;
;; Pure parsers (read-wave-checkpoints / wave-step-completed?) and a
;; single idempotent writer (write-wave-checkpoint!) so a resumed run
;; can skip already-completed milestone steps instead of re-executing.

(define wave-checkpoint-header "## Wave checkpoints")

(define (wave-checkpoint-section wave-id steps)
  (string-append wave-checkpoint-header
                 "\n\n### "
                 wave-id
                 "\n"
                 (string-join (for/list ([s steps])
                                (string-append "- [x] " s))
                              "\n")
                 "\n"))

;; Parse STATE.md content into (listof (cons wave-id step)).
;; Only lines under "## Wave checkpoints" count; duplicate step lines
;; collapse to the first occurrence.
(define (read-wave-checkpoints state-content)
  (define step-rx #px"^- \\[x\\] (.+)$")
  (define wave-rx #px"^### (.+)$")
  (let loop ([lines (string-split state-content "\n")]
             [in-section? #f]
             [wave #f]
             [acc '()])
    (cond
      [(null? lines) (reverse acc)]
      [else
       (define t (string-trim (car lines)))
       (cond
         [(string=? t wave-checkpoint-header) (loop (cdr lines) #t wave acc)]
         [(and in-section? (regexp-match? wave-rx t))
          (loop (cdr lines) in-section? (string-trim (cadr (regexp-match wave-rx t))) acc)]
         [(and in-section? wave (regexp-match? step-rx t))
          (define pair (cons wave (string-trim (cadr (regexp-match step-rx t)))))
          (if (member pair acc)
              (loop (cdr lines) in-section? wave acc)
              (loop (cdr lines) in-section? wave (cons pair acc)))]
         [else (loop (cdr lines) in-section? wave acc)])])))

;; Flat predicate: has this step of this wave been recorded?
(define (wave-step-completed? state-content wave-id step)
  (and (member (cons wave-id step) (read-wave-checkpoints state-content)) #t))

;; Read STATE.md content safely (empty string if missing/unreadable) so
;; callers can test step completion before re-executing milestone steps.
(define (read-state-content state-path)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (if (file-exists? state-path)
        (file->string state-path)
        "")))

;; Append "- [x] step" at the END of the wave's existing step block so
;; the file order mirrors execution order (oldest step first) and
;; read-wave-checkpoints returns checkpoints chronologically.
;; Returns #f when no header line exists.
(define (wave-step-run-length rest)
  ;; rest begins right after the "### <wave>" header line; return the
  ;; char length of the maximal run of consecutive "- [x] step" lines
  ;; (each line prefixed by the newline that precedes it).
  (let loop ([ls (string-split rest "\n")]
             [len 0])
    (cond
      [(null? ls) len]
      [(regexp-match? #px"^- \\[x\\] " (car ls))
       (loop (cdr ls) (+ len (add1 (string-length (car ls)))))]
      [else len])))

(define (insert-step-at-wave-block-end content wave-id step)
  (define m (regexp-match-positions (format "(?m:^### ~a$)" (regexp-quote wave-id)) content))
  (and m
       (let* ([end (cdar m)]
              [insert-at (+ end (wave-step-run-length (substring content end)))])
         (string-append (substring content 0 insert-at)
                        (format "\n- [x] ~a" step)
                        (substring content insert-at)))))

;; Record completion durably into STATE.md. Idempotent — recording an
;; already-recorded step is a no-op returning 'no-op.
(define (write-wave-checkpoint! state-path wave-id step)
  (define content (read-state-content state-path))
  (cond
    [(wave-step-completed? content wave-id step) 'no-op]
    [else
     (define new-content
       (cond
         ;; wave block already exists: append at the block's end
         [(insert-step-at-wave-block-end content wave-id step)]
         ;; section header exists but not this wave: append the wave block
         [(string-contains? content wave-checkpoint-header)
          (string-append (string-trim content) "\n\n" (wave-checkpoint-section wave-id (list step)))]
         ;; no section at all: append one
         [else
          (string-append (string-trim content)
                         "\n\n"
                         (wave-checkpoint-section wave-id (list step)))]))
     (call-with-output-file state-path (lambda (out) (display new-content out)) #:exists 'truncate)
     'recorded]))
