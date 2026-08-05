#lang racket/base

;; wiring/run-modes/base.rkt — Mode resolution + MCP governed execution
;;
;; v0.99.43 W0: Extracted from wiring/run-modes.rkt (monolithic 658-line file).
;; Contains the foundational, side-effect-light pieces:
;;   - mode-for-config: cli-config → interface mode symbol
;;   - make-mcp-event-publisher: bus event adapter
;;   - make-mcp-governed-execute-fn: governed tool execution for MCP tools/call
;;   - filter-workflow-skills: mas-workflow skill detection
;;
;; STABILITY: stable — re-exported through wiring/run-modes.rkt facade.

(require racket/string
         "../../interfaces/cli.rkt"
         (only-in "../../skills/frontmatter.rkt" parse-skill-frontmatter-extended)
         "../../tools/tool.rkt"
         (only-in "../../tools/permission-gate.rkt" make-strict-permission-config)
         (only-in "../../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         "../../util/event/event-bus.rkt"
         (only-in "../../util/event/event.rkt" make-event))

(provide mode-for-config
         make-mcp-event-publisher
         make-mcp-governed-execute-fn
         filter-workflow-skills)

;; ============================================================
;; mode-for-config
;; ============================================================

;; Determine which interface mode to run based on cli-config.
;; Returns a symbol: 'interactive | 'single | 'json | 'rpc | 'tui | 'help | 'version

(define (mode-for-config cfg)
  (define cmd (cli-config-command cfg))
  (case cmd
    [(help) 'help]
    [(version) 'version]
    [(doctor) 'doctor]
    [(init) 'init]
    [(sessions) 'sessions]
    [else (cli-config-mode cfg)]))

;; ============================================================
;; MCP governed tool execution
;; ============================================================

(define (make-mcp-event-publisher bus session-id)
  (and bus
       (lambda (event-type payload)
         (publish! bus
                   (make-event event-type (current-inexact-milliseconds) session-id #f payload)))))

(define (make-mcp-governed-execute-fn registry
                                      #:working-directory [working-directory #f]
                                      #:event-publisher [event-publisher #f]
                                      #:runtime-settings [runtime-settings #f]
                                      #:session-metadata [session-metadata #f]
                                      #:permission-config
                                      [permission-config (make-strict-permission-config)]
                                      #:hook-dispatcher [hook-dispatcher #f])
  (lambda (tool-name args)
    (define call-id (format "mcp-~a" (current-inexact-milliseconds)))
    (define sched-result
      (run-tool-batch (list (make-tool-call call-id tool-name args))
                      registry
                      #:hook-dispatcher hook-dispatcher
                      #:exec-context (make-exec-context #:working-directory working-directory
                                                        #:event-publisher event-publisher
                                                        #:runtime-settings runtime-settings
                                                        #:call-id call-id
                                                        #:session-metadata session-metadata
                                                        #:permission-config permission-config)
                      #:parallel? #f))
    (define results (scheduler-result-results sched-result))
    (if (pair? results)
        (car results)
        (make-error-result (format "tool '~a' produced no result" tool-name)))))

;; ============================================================
;; filter-workflow-skills
;; ============================================================

;; v0.99.26 §5.2: Filter skills that have type: mas-workflow in their frontmatter.
;; Returns a list of skill hashes (same format as resource-set-skills).
(define (filter-workflow-skills skills)
  (filter (lambda (s)
            (define raw-content (hash-ref s 'raw-content (hash-ref s 'content "")))
            (define fm (parse-skill-frontmatter-extended raw-content))
            (and fm (equal? (hash-ref fm 'type #f) "mas-workflow")))
          skills))
