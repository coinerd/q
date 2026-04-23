#lang racket/base

;; extensions/q-sync.rkt — Sync Extension
;;
;; Wave C2: Registers q-sync tool for multi-domain sync
;; (planning, config, scripts, git, all) with push/pull/status/handoff.

(require racket/contract
         racket/string
         racket/port
         racket/path
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide the-extension
         q-sync-extension
         handle-q-sync
         sync-planning
         sync-pi-config
         sync-scripts
         sync-git
         sync-status)

;; ============================================================
;; Sync domain implementations
;; ============================================================

;; Run a shell command, return (values exit-code stdout stderr)
(define (run-cmd cmd args)
  (define-values (sp out-in in-out err-in)
    (apply subprocess #f #f #f (find-executable-path cmd) args))
  (subprocess-wait sp)
  (define stdout-str (port->string out-in))
  (define stderr-str (port->string err-in))
  (close-input-port out-in)
  (close-output-port in-out)
  (close-input-port err-in)
  (values (subprocess-status sp) stdout-str stderr-str))

;; Get remote host from args or default
(define (get-remote-host args)
  (hash-ref args 'remote_host "user@vps2402959.fastwebserver.de"))

;; Get project dir
(define (get-project-dir args)
  (hash-ref args 'project_dir (path->string (current-directory))))

;; Sync planning state via rsync
(define (sync-planning remote-host local-dir direction)
  (define planning-dir (build-path local-dir ".planning"))
  (define remote-path (string-append remote-host ":~/src/q-agent/.planning/"))
  (define-values (ec out err)
    (case (string->symbol direction)
      [(push) (run-cmd "rsync" (list "-avz" "--delete" (path->string planning-dir) "/" remote-path))]
      [(pull) (run-cmd "rsync" (list "-avz" "--delete" remote-path (path->string planning-dir) "/"))]
      [else (values 1 "" (format "Unknown direction: ~a" direction))]))
  (values ec (string-trim out) (string-trim err)))

;; Sync pi config
(define (sync-pi-config remote-host local-dir direction)
  (define pi-dir (build-path local-dir ".pi"))
  (define remote-path (string-append remote-host ":~/src/q-agent/.pi/"))
  (define-values (ec out err)
    (case (string->symbol direction)
      [(push) (run-cmd "rsync" (list "-avz" (path->string pi-dir) "/" remote-path))]
      [(pull) (run-cmd "rsync" (list "-avz" remote-path (path->string pi-dir) "/"))]
      [else (values 1 "" (format "Unknown direction: ~a" direction))]))
  (values ec (string-trim out) (string-trim err)))

;; Sync scripts
(define (sync-scripts remote-host local-dir direction)
  (define scripts-dir (build-path local-dir "scripts"))
  (define remote-path (string-append remote-host ":~/src/q-agent/scripts/"))
  (define-values (ec out err)
    (case (string->symbol direction)
      [(push) (run-cmd "rsync" (list "-avz" (path->string scripts-dir) "/" remote-path))]
      [(pull) (run-cmd "rsync" (list "-avz" remote-path (path->string scripts-dir) "/"))]
      [else (values 1 "" (format "Unknown direction: ~a" direction))]))
  (values ec (string-trim out) (string-trim err)))

;; Git sync
(define (sync-git local-dir direction)
  (define-values (ec out err)
    (case (string->symbol direction)
      [(push) (run-cmd "git" '("push" "origin" "main"))]
      [(pull) (run-cmd "git" '("pull" "origin" "main"))]
      [else (values 1 "" (format "Unknown direction: ~a" direction))]))
  (values ec (string-trim out) (string-trim err)))

;; Status check — compare local and remote
(define (sync-status remote-host local-dir)
  (define parts '())
  ;; Check planning dir exists
  (set! parts
        (cons (if (directory-exists? (build-path local-dir ".planning"))
                  "✓ .planning/ exists locally"
                  "✗ .planning/ missing locally")
              parts))
  ;; Check pi dir exists
  (set! parts
        (cons (if (directory-exists? (build-path local-dir ".pi"))
                  "✓ .pi/ exists locally"
                  "✗ .pi/ missing locally")
              parts))
  ;; Check git status
  (define-values (git-ec git-out _) (run-cmd "git" '("status" "--short")))
  (set! parts
        (cons (if (non-empty-string? (string-trim git-out))
                  (format "⚠ git: uncommitted changes\n~a" (string-trim git-out))
                  "✓ git: clean")
              parts))
  (string-join (reverse parts) "\n"))

;; ============================================================
;; Tool handler
;; ============================================================

(define (handle-q-sync args [exec-ctx #f])
  (define direction (hash-ref args 'direction "status"))
  (define domain (hash-ref args 'domain "all"))
  (define remote-host (get-remote-host args))
  (define local-dir (get-project-dir args))

  (define results '())

  (case (string->symbol direction)
    [(status)
     (define status-text (sync-status remote-host local-dir))
     (make-success-result (list (hasheq 'type "text" 'text (format "Sync Status:\n~a" status-text))))]
    [(push pull)
     ;; Sync requested domains
     (when (member domain '("planning" "all"))
       (define-values (ec out err) (sync-planning remote-host local-dir direction))
       (set! results (cons (format "planning: ~a" (if (zero? ec) "OK" err)) results)))
     (when (member domain '("pi" "config" "all"))
       (define-values (ec out err) (sync-pi-config remote-host local-dir direction))
       (set! results (cons (format "pi-config: ~a" (if (zero? ec) "OK" err)) results)))
     (when (member domain '("scripts" "all"))
       (define-values (ec out err) (sync-scripts remote-host local-dir direction))
       (set! results (cons (format "scripts: ~a" (if (zero? ec) "OK" err)) results)))
     (when (member domain '("git" "all"))
       (define-values (ec out err) (sync-git local-dir direction))
       (set! results (cons (format "git: ~a" (if (zero? ec) "OK" err)) results)))
     (make-success-result
      (list (hasheq
             'type
             "text"
             'text
             (format "Sync ~a (~a):\n~a" direction domain (string-join (reverse results) "\n")))))]
    [(handoff)
     ;; Push everything then update HANDOFF.json
     (for ([d '("planning" "pi" "scripts" "git")])
       (define-values (ec out err)
         (case (string->symbol d)
           [(planning) (sync-planning remote-host local-dir "push")]
           [(pi) (sync-pi-config remote-host local-dir "push")]
           [(scripts) (sync-scripts remote-host local-dir "push")]
           [(git) (sync-git local-dir "push")]))
       (set! results (cons (format "~a: ~a" d (if (zero? ec) "OK" err)) results)))
     (make-success-result (list (hasheq 'type
                                        "text"
                                        'text
                                        (format "Handoff complete:\n~a"
                                                (string-join (reverse results) "\n")))))]
    [else (make-error-result (format "Unknown direction: ~a" direction))]))

;; ============================================================
;; Extension definition
;; ============================================================

(define (register-sync-tools ctx)
  (ext-register-tool!
   ctx
   "q-sync"
   (string-append "Sync project state between local and remote machines. "
                  "Domains: planning, pi, scripts, git, all. "
                  "Directions: push, pull, status, handoff. "
                  "Uses rsync for file sync, git for version control.")
   (hasheq 'type
           "object"
           'required
           '()
           'properties
           (hasheq 'direction
                   (hasheq 'type "string" 'description "push|pull|status|handoff (default: status)")
                   'domain
                   (hasheq 'type "string" 'description "planning|pi|scripts|git|all (default: all)")
                   'remote_host
                   (hasheq 'type "string" 'description "SSH remote host (default: from config)")
                   'project_dir
                   (hasheq 'type "string" 'description "Project root directory (default: cwd)")))
   handle-q-sync)
  (hook-pass ctx))

(define-q-extension q-sync-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-sync-tools)

(define the-extension q-sync-extension)
