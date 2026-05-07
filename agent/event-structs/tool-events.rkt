#lang racket/base

;; agent/event-structs/tool-events.rkt — tool execution and per-tool events
;;
;; Contains tool execution lifecycle, generic tool call/result,
;; and all per-tool typed events (bash, edit, write, read, grep, find, custom).
;;
;; Simple events (typed-event parent) use define-typed-event macro.
;; Per-tool events (tool-call-event parent with complex constructors) remain manual.

(require "base.rkt"
         "../../util/event-macro.rkt")

;; ============================================================
;; Tool execution lifecycle (typed-event parent)
;; ============================================================

(define-typed-event tool-execution-start-event "tool.execution.started"
  (tool-name tool-call-id))

(define-typed-event tool-execution-update-event "tool.execution.updated"
  (tool-name progress))

(define-typed-event tool-execution-end-event "tool.execution.completed"
  (tool-name duration-ms result-summary))

;; ============================================================
;; Generic tool call/result (typed-event parent)
;; ============================================================

(define-typed-event tool-call-event "tool.called"
  (tool-name arguments tool-call-id))

(define-typed-event tool-result-event "tool.result"
  (tool-call-id content is-error?))

;; ============================================================
;; Per-tool typed events (tool-call-event parent — manual)
;; These inherit from tool-call-event and have complex constructors
;; that compute arguments from specific fields.
;; ============================================================

(provide (struct-out bash-tool-call-event)
         (struct-out edit-tool-call-event)
         (struct-out write-tool-call-event)
         (struct-out read-tool-call-event)
         (struct-out grep-tool-call-event)
         (struct-out find-tool-call-event)
         (struct-out custom-tool-call-event)
         make-bash-tool-call-event
         make-edit-tool-call-event
         make-write-tool-call-event
         make-read-tool-call-event
         make-grep-tool-call-event
         make-find-tool-call-event
         make-custom-tool-call-event
         bash-tool-call-event?
         edit-tool-call-event?
         write-tool-call-event?
         read-tool-call-event?
         grep-tool-call-event?
         find-tool-call-event?
         custom-tool-call-event?
         ;; Field lists for per-tool events (for event serialization)
         bash-tool-call-event-fields
         edit-tool-call-event-fields
         write-tool-call-event-fields
         read-tool-call-event-fields
         grep-tool-call-event-fields
         find-tool-call-event-fields
         custom-tool-call-event-fields)

;; bash-tool-call-event
(struct bash-tool-call-event tool-call-event (command timeout cwd) #:transparent)

(define (make-bash-tool-call-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:command command
                                   #:timeout timeout
                                   #:cwd cwd
                                   #:tool-call-id tool-call-id)
  (bash-tool-call-event "tool.bash.called"
                        timestamp
                        session-id
                        turn-id
                        "bash"
                        (hasheq 'command command 'timeout timeout 'cwd cwd)
                        tool-call-id
                        command
                        timeout
                        cwd))

(define bash-tool-call-event-fields
  (append tool-call-event-fields '(command timeout cwd)))

;; edit-tool-call-event
(struct edit-tool-call-event tool-call-event (path edits) #:transparent)

(define (make-edit-tool-call-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:path path
                                   #:edits edits
                                   #:tool-call-id tool-call-id)
  (edit-tool-call-event "tool.edit.called"
                        timestamp
                        session-id
                        turn-id
                        "edit"
                        (hasheq 'path path 'edits edits)
                        tool-call-id
                        path
                        edits))

(define edit-tool-call-event-fields
  (append tool-call-event-fields '(path edits)))

;; write-tool-call-event
(struct write-tool-call-event tool-call-event (path content) #:transparent)

(define (make-write-tool-call-event #:session-id session-id
                                    #:turn-id turn-id
                                    #:timestamp timestamp
                                    #:path path
                                    #:content content
                                    #:tool-call-id tool-call-id)
  (write-tool-call-event "tool.write.called"
                         timestamp
                         session-id
                         turn-id
                         "write"
                         (hasheq 'path path 'content content)
                         tool-call-id
                         path
                         content))

(define write-tool-call-event-fields
  (append tool-call-event-fields '(path content)))

;; read-tool-call-event
(struct read-tool-call-event tool-call-event (path offset limit) #:transparent)

(define (make-read-tool-call-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:path path
                                   #:offset offset
                                   #:limit limit
                                   #:tool-call-id tool-call-id)
  (read-tool-call-event "tool.read.called"
                        timestamp
                        session-id
                        turn-id
                        "read"
                        (hasheq 'path path 'offset offset 'limit limit)
                        tool-call-id
                        path
                        offset
                        limit))

(define read-tool-call-event-fields
  (append tool-call-event-fields '(path offset limit)))

;; grep-tool-call-event
(struct grep-tool-call-event tool-call-event (pattern path glob) #:transparent)

(define (make-grep-tool-call-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:pattern pattern
                                   #:path path
                                   #:glob glob
                                   #:tool-call-id tool-call-id)
  (grep-tool-call-event "tool.grep.called"
                        timestamp
                        session-id
                        turn-id
                        "grep"
                        (hasheq 'pattern pattern 'path path 'glob glob)
                        tool-call-id
                        pattern
                        path
                        glob))

(define grep-tool-call-event-fields
  (append tool-call-event-fields '(pattern path glob)))

;; find-tool-call-event
(struct find-tool-call-event tool-call-event (pattern path) #:transparent)

(define (make-find-tool-call-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:pattern pattern
                                   #:path path
                                   #:tool-call-id tool-call-id)
  (find-tool-call-event "tool.find.called"
                        timestamp
                        session-id
                        turn-id
                        "find"
                        (hasheq 'pattern pattern 'path path)
                        tool-call-id
                        pattern
                        path))

(define find-tool-call-event-fields
  (append tool-call-event-fields '(pattern path)))

;; custom-tool-call-event — fallback for unknown tools
(struct custom-tool-call-event tool-call-event () #:transparent)

(define (make-custom-tool-call-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:timestamp timestamp
                                     #:tool-name tool-name
                                     #:arguments arguments
                                     #:tool-call-id tool-call-id)
  (custom-tool-call-event "tool.custom.called"
                          timestamp
                          session-id
                          turn-id
                          tool-name
                          arguments
                          tool-call-id))

(define custom-tool-call-event-fields tool-call-event-fields)
