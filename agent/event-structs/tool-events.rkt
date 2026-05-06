#lang racket/base

;; agent/event-structs/tool-events.rkt — tool execution and per-tool events
;;
;; Contains tool execution lifecycle, generic tool call/result,
;; and all per-tool typed events (bash, edit, write, read, grep, find, custom).

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Tool execution lifecycle
(provide (struct-out tool-execution-start-event)
         (struct-out tool-execution-update-event)
         (struct-out tool-execution-end-event)
         make-tool-execution-start-event
         make-tool-execution-update-event
         make-tool-execution-end-event
         tool-execution-start-event?
         tool-execution-update-event?
         tool-execution-end-event?

         ;; Generic tool call/result
         (struct-out tool-call-event)
         (struct-out tool-result-event)
         make-tool-call-event
         make-tool-result-event
         tool-call-event?
         tool-result-event?

         ;; Per-tool typed events
         (struct-out bash-tool-call-event)
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
         custom-tool-call-event?)

;; ============================================================
;; Tool execution lifecycle
;; ============================================================

(struct tool-execution-start-event typed-event (tool-name tool-call-id) #:transparent)
(struct tool-execution-update-event typed-event (tool-name progress) #:transparent)
(struct tool-execution-end-event typed-event (tool-name duration-ms result-summary) #:transparent)

(define (make-tool-execution-start-event #:session-id session-id
                                         #:turn-id turn-id
                                         #:timestamp timestamp
                                         #:tool-name tool-name
                                         #:tool-call-id tool-call-id)
  (tool-execution-start-event "tool.execution.started"
                              timestamp
                              session-id
                              turn-id
                              tool-name
                              tool-call-id))

(define (make-tool-execution-update-event #:session-id session-id
                                          #:turn-id turn-id
                                          #:timestamp timestamp
                                          #:tool-name tool-name
                                          #:progress progress)
  (tool-execution-update-event "tool.execution.updated"
                               timestamp
                               session-id
                               turn-id
                               tool-name
                               progress))

(define (make-tool-execution-end-event #:session-id session-id
                                       #:turn-id turn-id
                                       #:timestamp timestamp
                                       #:tool-name tool-name
                                       #:duration-ms duration-ms
                                       #:result-summary result-summary)
  (tool-execution-end-event "tool.execution.completed"
                            timestamp
                            session-id
                            turn-id
                            tool-name
                            duration-ms
                            result-summary))

;; ============================================================
;; Generic tool call/result
;; ============================================================

(struct tool-call-event typed-event (tool-name arguments tool-call-id) #:transparent)
(struct tool-result-event typed-event (tool-call-id content is-error?) #:transparent)

(define (make-tool-call-event #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp timestamp
                              #:tool-name tool-name
                              #:arguments arguments
                              #:tool-call-id tool-call-id)
  (tool-call-event "tool.called" timestamp session-id turn-id tool-name arguments tool-call-id))

(define (make-tool-result-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:tool-call-id tool-call-id
                                #:content content
                                #:is-error? is-error?)
  (tool-result-event "tool.result" timestamp session-id turn-id tool-call-id content is-error?))

;; ============================================================
;; Per-tool typed events (Issue #1311)
;; ============================================================

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

;; grep-tool-call-event
(struct grep-tool-call-event
        tool-call-event
        (pattern path
          glob)
  #:transparent)

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
