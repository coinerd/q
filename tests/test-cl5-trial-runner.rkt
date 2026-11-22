#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-trial-runner.rkt — CL5 W1 (#9982): trial command construction,
;; series identity/path, strict session discovery and durable checkpointing.
;;
;; Covers scripts/cl5/trial-runner.rkt without executing any pi session:
;;   1. The command is derived from the shared experiment config (both arms)
;;   2. The series path names the arm and pins the revision
;;   3. Session discovery never falls back to a stale transcript
;;   4. write-series! checkpoints metadata + completed trials as JSON
;;   5. resets are claimed only for disposable trial worktrees

(require json
         rackunit
         racket/file
         racket/list
         racket/string
         "../scripts/cl5/experiment-config.rkt"
         "../scripts/cl5/task-spec.rkt"
         "../scripts/cl5/trial-runner.rkt")

(define config (experiment-config #:pi-version "1.6.0"))

(define (arg-after args flag)
  (define tail (member flag args))
  (and tail (pair? (cdr tail)) (second tail)))

(module+ test
  (test-case "both arms build commands from the same experiment config"
    (define baseline (trial-command 'T1 (arm-config config 'baseline) "/tmp/s" "/tmp/o"))
    (define intervention (trial-command 'T1 (arm-config config 'intervention) "/tmp/s" "/tmp/o"))
    (for ([command (in-list (list baseline intervention))])
      (check-equal? (arg-after command "--provider") "deepseek")
      (check-equal? (arg-after command "--model") "deepseek-flash")
      (check-equal? (arg-after command "--thinking") "high")
      (check-equal? (arg-after command "--tools") "read,bash,edit,write")
      (check-equal? (arg-after command "--session-dir") "/tmp/s")
      (check-not-false (member "--no-extensions" command))
      (check-not-false (member "--no-mcp" command)))
    ;; The arm is in the session name so a transcript is traceable.
    (check-true (string-contains? (arg-after baseline "--name") "baseline"))
    (check-true (string-contains? (arg-after intervention "--name") "intervention"))
    (check-true (string-contains? (arg-after baseline "--name") "003e425d"))
    (check-true (string-contains? (arg-after intervention "--name") "ec88d463"))
    ;; The frozen prompt is the last argument, verbatim.
    (check-equal? (last baseline) (hash-ref (task-spec 'T1) 'prompt)))

  (test-case "series path names the arm and the pinned revision"
    (define path (series-path-for 'baseline baseline-revision "docs/reports"))
    (check-true (string-contains? (path->string path) "CL5-TRIALS-baseline-003e425d.json"))
    (define other (series-path-for 'intervention intervention-revision "docs/reports"))
    (check-not-equal? (path->string path) (path->string other)))

  (test-case "first-correct-edit is claimed only when a single edit is attributable"
    (define-values (ms basis) (first-correct-edit #t 1 4200))
    (check-equal? ms 4200)
    (check-equal? basis "single-in-scope-edit")
    ;; Multiple edits: the final state passing does not prove the first was correct.
    (check-equal? (call-with-values (lambda () (first-correct-edit #t 3 4200)) list)
                  (list #f "unobserved-multiple-edits"))
    (check-equal? (call-with-values (lambda () (first-correct-edit #t 0 #f)) list)
                  (list #f "unobserved-no-edit"))
    (check-equal? (call-with-values (lambda () (first-correct-edit #f 1 4200)) list)
                  (list #f "not-correct")))

  (test-case "session discovery never falls back to a stale transcript"
    (define dir (make-temporary-file "cl5-sessions-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define stale (build-path dir "stale.jsonl"))
       (call-with-output-file stale (lambda (out) (display "{}\n" out)) #:exists 'truncate/replace)
       ;; Nothing new since the pre-run snapshot: return #f, never the stale file.
       (check-false (find-newest-session-file dir (list stale)))
       ;; A genuinely new file is found.
       (define fresh (build-path dir "fresh.jsonl"))
       (call-with-output-file fresh (lambda (out) (display "{}\n" out)) #:exists 'truncate/replace)
       (check-equal? (find-newest-session-file dir (list stale)) fresh)
       (check-false (find-newest-session-file dir (list stale fresh))))
     (lambda () (delete-directory/files dir))))

  (test-case "write-series! publishes atomically and leaves no temp file"
    (define dir (make-temporary-file "cl5-atomic-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define path (build-path dir "series.json"))
       (define metadata (series-metadata config 'baseline #:host "host-a" #:n 1 #:rotation '(T1)))
       (write-series! path metadata (list (hash 'task "T1" 'round 0)))
       (check-true (file-exists? path))
       (check-equal? (filter (lambda (name) (string-prefix? "." name))
                             (map path->string (directory-list dir)))
                     '()))
     (lambda () (delete-directory/files dir))))

  (test-case "an existing series refuses silent restart and resumes only on request"
    (define dir (make-temporary-file "cl5-resume-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define path (series-path-for 'baseline baseline-revision dir))
       (define metadata (series-metadata config 'baseline #:host "host-a" #:n 1 #:rotation '(T1)))
       (write-series! path metadata (list (hash 'task "T1" 'round 0 'arm "baseline")))
       ;; Without #:resume? the runner refuses instead of duplicating trials.
       (check-exn exn:fail?
                  (lambda ()
                    (run-series '(T1)
                                dir
                                #:config config
                                #:arm 'baseline
                                #:n 1
                                #:session-dir dir
                                #:out-dir dir
                                #:models (list (cons "deepseek" "deepseek-flash"))
                                #:cli-version "1.6.0" #:host "host-a")))
       ;; With #:resume? #t the completed (round, task) pair is skipped, so no
       ;; session is launched for the same workload.
       (check-equal? (hash-ref (run-series '(T1)
                                           dir
                                           #:config config
                                           #:arm 'baseline
                                           #:n 1
                                           #:session-dir dir
                                           #:out-dir dir
                                           #:models (list (cons "deepseek" "deepseek-flash"))
                                           #:cli-version "1.6.0"
                                           #:host "host-a"
                                           #:resume? #t)
                               'trial-count)
                     1))
     (lambda () (delete-directory/files dir))))

  (test-case "write-series! checkpoints metadata and completed trials"
    (define dir (make-temporary-file "cl5-series-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define path (build-path dir "series.json"))
       (define metadata (series-metadata config 'baseline #:host "host-a" #:n 2 #:rotation '(T1 T3)))
       (define trials
         (list (hash 'task "T1" 'arm "baseline" 'oracle (hash 'verdict "pass"))
               (hash 'task "T3" 'arm "baseline" 'oracle (hash 'verdict "fail"))))
       (write-series! path metadata trials)
       (define written (string->jsexpr (file->string path)))
       (check-equal? (hash-ref written 'model) "deepseek-flash")
       (check-equal? (hash-ref written 'arm) "baseline")
       (check-equal? (hash-ref written 'pi-version) "1.6.0")
       (check-equal? (length (hash-ref written 'trials)) 2)
       ;; A second checkpoint replaces the file with the full list (no loss, no dup).
       (write-series! path metadata (append trials (list (hash 'task "T2" 'arm "baseline"))))
       (define rewritten (string->jsexpr (file->string path)))
       (check-equal? (length (hash-ref rewritten 'trials)) 3)
       (check-equal? (hash-ref (last (hash-ref rewritten 'trials)) 'task) "T2"))
     (lambda () (delete-directory/files dir)))))
