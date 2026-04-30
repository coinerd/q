#lang racket/base

;; agent/event-structs.rkt — typed event struct definitions and constructors
;;
;; Extracted from event-types.rkt. Contains all struct definitions and
;; make- constructors for typed events. No serialization logic.
;;
;; All events inherit from typed-event (type, timestamp, session-id, turn-id).
;; Category-specific structs add typed fields.
;; Per-tool events inherit from tool-call-event.

(provide (struct-out typed-event)
         typed-event?

         ;; Turn lifecycle
         (struct-out turn-start-event)
         (struct-out turn-end-event)
         make-turn-start-event
         make-turn-end-event
         turn-start-event?
         turn-end-event?

         ;; Message lifecycle
         (struct-out message-start-event)
         (struct-out message-update-event)
         (struct-out message-end-event)
         make-message-start-event
         make-message-update-event
         make-message-end-event
         message-start-event?
         message-update-event?
         message-end-event?

         ;; Tool execution lifecycle
         (struct-out tool-execution-start-event)
         (struct-out tool-execution-update-event)
         (struct-out tool-execution-end-event)
         make-tool-execution-start-event
         make-tool-execution-update-event
         make-tool-execution-end-event
         tool-execution-start-event?
         tool-execution-update-event?
         tool-execution-end-event?

         ;; Tool call/result (generic)
         (struct-out tool-call-event)
         (struct-out tool-result-event)
         make-tool-call-event
         make-tool-result-event
         tool-call-event?
         tool-result-event?

         ;; Per-tool typed events (inherit from tool-call-event)
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
         custom-tool-call-event?

         ;; Provider events
         (struct-out provider-request-event)
         (struct-out provider-response-event)
         make-provider-request-event
         make-provider-response-event
         provider-request-event?
         provider-response-event?

         ;; Session events
         (struct-out session-start-event)
         (struct-out session-shutdown-event)
         make-session-start-event
         make-session-shutdown-event
         session-start-event?
         session-shutdown-event?

         ;; Input events
         (struct-out input-event)
         make-input-event
         input-event?

         ;; Model events
         (struct-out model-select-event)
         make-model-select-event
         model-select-event?

         ;; Agent events
         (struct-out agent-start-event)
         (struct-out agent-end-event)
         make-agent-start-event
         make-agent-end-event
         agent-start-event?
         agent-end-event?

         ;; Context events
         (struct-out context-event)
         make-context-event
         context-event?)

;; ============================================================
;; Base struct
;; ============================================================

(struct typed-event (type timestamp session-id turn-id) #:transparent)

;; ============================================================
;; Turn lifecycle
;; ============================================================

(struct turn-start-event typed-event (model provider) #:transparent)
(struct turn-end-event typed-event (reason duration-ms) #:transparent)

(define (make-turn-start-event #:session-id session-id
                               #:turn-id turn-id
                               #:timestamp timestamp
                               #:model model
                               #:provider provider)
  (turn-start-event "turn-start" timestamp session-id turn-id model provider))

(define (make-turn-end-event #:session-id session-id
                             #:turn-id turn-id
                             #:timestamp timestamp
                             #:reason reason
                             #:duration-ms duration-ms)
  (turn-end-event "turn-end" timestamp session-id turn-id reason duration-ms))

;; ============================================================
;; Message lifecycle
;; ============================================================

(struct message-start-event typed-event (role model) #:transparent)
(struct message-update-event typed-event (content delta) #:transparent)
(struct message-end-event typed-event (role content-length) #:transparent)

(define (make-message-start-event #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp timestamp
                                  #:role role
                                  #:model model)
  (message-start-event "message-start" timestamp session-id turn-id role model))

(define (make-message-update-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:content content
                                   #:delta delta)
  (message-update-event "message-update" timestamp session-id turn-id content delta))

(define (make-message-end-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:role role
                                #:content-length content-length)
  (message-end-event "message-end" timestamp session-id turn-id role content-length))

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
  (tool-execution-start-event "tool-execution-start"
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
  (tool-execution-update-event "tool-execution-update"
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
  (tool-execution-end-event "tool-execution-end"
                            timestamp
                            session-id
                            turn-id
                            tool-name
                            duration-ms
                            result-summary))

;; ============================================================
;; Tool call/result (generic)
;; ============================================================

(struct tool-call-event typed-event (tool-name arguments tool-call-id) #:transparent)
(struct tool-result-event typed-event (tool-call-id content is-error?) #:transparent)

(define (make-tool-call-event #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp timestamp
                              #:tool-name tool-name
                              #:arguments arguments
                              #:tool-call-id tool-call-id)
  (tool-call-event "tool-call" timestamp session-id turn-id tool-name arguments tool-call-id))

(define (make-tool-result-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:tool-call-id tool-call-id
                                #:content content
                                #:is-error? is-error?)
  (tool-result-event "tool-result" timestamp session-id turn-id tool-call-id content is-error?))

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
  (bash-tool-call-event "bash-tool-call"
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
  (edit-tool-call-event "edit-tool-call"
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
  (write-tool-call-event "write-tool-call"
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
  (read-tool-call-event "read-tool-call"
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
  (grep-tool-call-event "grep-tool-call"
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
  (find-tool-call-event "find-tool-call"
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
  (custom-tool-call-event "custom-tool-call"
                          timestamp
                          session-id
                          turn-id
                          tool-name
                          arguments
                          tool-call-id))

;; ============================================================
;; Provider events
;; ============================================================

(struct provider-request-event typed-event (model provider) #:transparent)
(struct provider-response-event typed-event (model provider latency-ms) #:transparent)

(define (make-provider-request-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:timestamp timestamp
                                     #:model model
                                     #:provider provider)
  (provider-request-event "provider-request" timestamp session-id turn-id model provider))

(define (make-provider-response-event #:session-id session-id
                                      #:turn-id turn-id
                                      #:timestamp timestamp
                                      #:model model
                                      #:provider provider
                                      #:latency-ms latency-ms)
  (provider-response-event "provider-response"
                           timestamp
                           session-id
                           turn-id
                           model
                           provider
                           latency-ms))

;; ============================================================
;; Session events
;; ============================================================

(struct session-start-event typed-event (model) #:transparent)
(struct session-shutdown-event typed-event (reason) #:transparent)

(define (make-session-start-event #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp timestamp
                                  #:model model)
  (session-start-event "session-start" timestamp session-id turn-id model))

(define (make-session-shutdown-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:timestamp timestamp
                                     #:reason reason)
  (session-shutdown-event "session-shutdown" timestamp session-id turn-id reason))

;; ============================================================
;; Input events
;; ============================================================

(struct input-event typed-event (input-type content) #:transparent)

(define (make-input-event #:session-id session-id
                          #:turn-id turn-id
                          #:timestamp timestamp
                          #:input-type input-type
                          #:content content)
  (input-event "input" timestamp session-id turn-id input-type content))

;; ============================================================
;; Model events
;; ============================================================

(struct model-select-event typed-event (model provider) #:transparent)

(define (make-model-select-event #:session-id session-id
                                 #:turn-id turn-id
                                 #:timestamp timestamp
                                 #:model model
                                 #:provider provider)
  (model-select-event "model-select" timestamp session-id turn-id model provider))

;; ============================================================
;; Agent events
;; ============================================================

(struct agent-start-event typed-event (model) #:transparent)
(struct agent-end-event typed-event (reason duration-ms) #:transparent)

(define (make-agent-start-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:model model)
  (agent-start-event "agent-start" timestamp session-id turn-id model))

(define (make-agent-end-event #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp timestamp
                              #:reason reason
                              #:duration-ms duration-ms)
  (agent-end-event "agent-end" timestamp session-id turn-id reason duration-ms))

;; ============================================================
;; Context events
;; ============================================================

(struct context-event typed-event (token-count window-size) #:transparent)

(define (make-context-event #:session-id session-id
                            #:turn-id turn-id
                            #:timestamp timestamp
                            #:token-count token-count
                            #:window-size window-size)
  (context-event "context" timestamp session-id turn-id token-count window-size))
