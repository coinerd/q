#lang racket/base

;; agent/event-types.rkt — typed event structs for all hook points
;;
;; Wave 0: Issues #1308 + #1311
;;
;; Defines typed Racket structs for each hook point event category,
;; replacing raw hash payloads with structured, contract-validated types.
;;
;; Design:
;;   - All events inherit from typed-event (type, timestamp, session-id, turn-id)
;;   - Category-specific structs add typed fields
;;   - Per-tool events inherit from tool-call-event
;;   - JSON serialization/deserialization with type-tagged dispatch
;;   - Unknown types deserialize as generic typed-event

(require racket/match)

;; Base struct
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
         context-event?

         ;; Serialization
         typed-event->jsexpr
         jsexpr->typed-event

         ;; Registry
         all-known-event-types
         event-name->tool-name)

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

;; ============================================================
;; JSON Serialization
;; ============================================================

;; typed-event->jsexpr : typed-event? -> hash?
(define (typed-event->jsexpr evt)
  (define base
    `#hasheq((type . ,(typed-event-type evt))
             (timestamp . ,(typed-event-timestamp evt))
             (sessionId . ,(typed-event-session-id evt))
             (turnId . ,(typed-event-turn-id evt))))
  (define extra (event-extra-fields evt))
  (for/fold ([h base]) ([(k v) (in-hash extra)])
    (hash-set h k v)))

;; jsexpr->typed-event : hash? -> typed-event?
(define (jsexpr->typed-event h)
  (define type (hash-ref h 'type #f))
  (define ts (hash-ref h 'timestamp 0))
  (define sid (hash-ref h 'sessionId ""))
  (define tid (hash-ref h 'turnId #f))
  (dispatch-deserialize type ts sid tid h))

;; ============================================================
;; Serialization helpers
;; ============================================================

;; Collect extra fields for each concrete event type
(define (event-extra-fields evt)
  (cond
    [(turn-start-event? evt)
     (hasheq 'model (turn-start-event-model evt) 'provider (turn-start-event-provider evt))]
    [(turn-end-event? evt)
     (hasheq 'reason (turn-end-event-reason evt) 'durationMs (turn-end-event-duration-ms evt))]
    [(message-start-event? evt)
     (hasheq 'role (message-start-event-role evt) 'model (message-start-event-model evt))]
    [(message-update-event? evt)
     (hasheq 'content (message-update-event-content evt) 'delta (message-update-event-delta evt))]
    [(message-end-event? evt)
     (hasheq 'role
             (message-end-event-role evt)
             'contentLength
             (message-end-event-content-length evt))]
    [(tool-execution-start-event? evt)
     (hasheq 'toolName
             (tool-execution-start-event-tool-name evt)
             'toolCallId
             (tool-execution-start-event-tool-call-id evt))]
    [(tool-execution-update-event? evt)
     (hasheq 'toolName
             (tool-execution-update-event-tool-name evt)
             'progress
             (tool-execution-update-event-progress evt))]
    [(tool-execution-end-event? evt)
     (hasheq 'toolName
             (tool-execution-end-event-tool-name evt)
             'durationMs
             (tool-execution-end-event-duration-ms evt)
             'resultSummary
             (tool-execution-end-event-result-summary evt))]
    [(bash-tool-call-event? evt)
     (hasheq 'toolName
             "bash"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'command
             (bash-tool-call-event-command evt)
             'timeout
             (bash-tool-call-event-timeout evt)
             'cwd
             (bash-tool-call-event-cwd evt))]
    [(edit-tool-call-event? evt)
     (hasheq 'toolName
             "edit"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (edit-tool-call-event-path evt)
             'edits
             (edit-tool-call-event-edits evt))]
    [(write-tool-call-event? evt)
     (hasheq 'toolName
             "write"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (write-tool-call-event-path evt)
             'content
             (write-tool-call-event-content evt))]
    [(read-tool-call-event? evt)
     (hasheq 'toolName
             "read"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (read-tool-call-event-path evt)
             'offset
             (read-tool-call-event-offset evt)
             'limit
             (read-tool-call-event-limit evt))]
    [(grep-tool-call-event? evt)
     (hasheq 'toolName
             "grep"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'pattern
             (grep-tool-call-event-pattern evt)
             'path
             (grep-tool-call-event-path evt)
             'glob
             (grep-tool-call-event-glob evt))]
    [(find-tool-call-event? evt)
     (hasheq 'toolName
             "find"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'pattern
             (find-tool-call-event-pattern evt)
             'path
             (find-tool-call-event-path evt))]
    [(custom-tool-call-event? evt)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'arguments
             (tool-call-event-arguments evt))]
    [(tool-call-event? evt)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'arguments
             (tool-call-event-arguments evt)
             'toolCallId
             (tool-call-event-tool-call-id evt))]
    [(tool-result-event? evt)
     (hasheq 'toolCallId
             (tool-result-event-tool-call-id evt)
             'content
             (tool-result-event-content evt)
             'isError
             (tool-result-event-is-error? evt))]
    [(provider-request-event? evt)
     (hasheq 'model
             (provider-request-event-model evt)
             'provider
             (provider-request-event-provider evt))]
    [(provider-response-event? evt)
     (hasheq 'model
             (provider-response-event-model evt)
             'provider
             (provider-response-event-provider evt)
             'latencyMs
             (provider-response-event-latency-ms evt))]
    [(session-start-event? evt) (hasheq 'model (session-start-event-model evt))]
    [(session-shutdown-event? evt) (hasheq 'reason (session-shutdown-event-reason evt))]
    [(input-event? evt)
     (hasheq 'inputType (input-event-input-type evt) 'content (input-event-content evt))]
    [(model-select-event? evt)
     (hasheq 'model (model-select-event-model evt) 'provider (model-select-event-provider evt))]
    [(agent-start-event? evt) (hasheq 'model (agent-start-event-model evt))]
    [(agent-end-event? evt)
     (hasheq 'reason (agent-end-event-reason evt) 'durationMs (agent-end-event-duration-ms evt))]
    [(context-event? evt)
     (hasheq 'tokenCount (context-event-token-count evt) 'windowSize (context-event-window-size evt))]
    [else (hasheq)]))

;; Dispatch deserialization by type string
(define (dispatch-deserialize type ts sid tid h)
  (match type
    ["turn-start" (turn-start-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["turn-end" (turn-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["message-start"
     (message-start-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'model ""))]
    ["message-update"
     (message-update-event type ts sid tid (hash-ref h 'content "") (hash-ref h 'delta ""))]
    ["message-end"
     (message-end-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'contentLength 0))]
    ["tool-execution-start"
     (tool-execution-start-event type
                                 ts
                                 sid
                                 tid
                                 (hash-ref h 'toolName "")
                                 (hash-ref h 'toolCallId ""))]
    ["tool-execution-update"
     (tool-execution-update-event type
                                  ts
                                  sid
                                  tid
                                  (hash-ref h 'toolName "")
                                  (hash-ref h 'progress ""))]
    ["tool-execution-end"
     (tool-execution-end-event type
                               ts
                               sid
                               tid
                               (hash-ref h 'toolName "")
                               (hash-ref h 'durationMs 0)
                               (hash-ref h 'resultSummary ""))]
    ["bash-tool-call"
     (bash-tool-call-event type
                           ts
                           sid
                           tid
                           "bash"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'command "")
                           (hash-ref h 'timeout 30)
                           (hash-ref h 'cwd ""))]
    ["edit-tool-call"
     (edit-tool-call-event type
                           ts
                           sid
                           tid
                           "edit"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'path "")
                           (hash-ref h 'edits '()))]
    ["write-tool-call"
     (write-tool-call-event type
                            ts
                            sid
                            tid
                            "write"
                            (hasheq)
                            (hash-ref h 'toolCallId "")
                            (hash-ref h 'path "")
                            (hash-ref h 'content ""))]
    ["read-tool-call"
     (read-tool-call-event type
                           ts
                           sid
                           tid
                           "read"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'path "")
                           (hash-ref h 'offset #f)
                           (hash-ref h 'limit #f))]
    ["grep-tool-call"
     (grep-tool-call-event type
                           ts
                           sid
                           tid
                           "grep"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'pattern "")
                           (hash-ref h 'path "")
                           (hash-ref h 'glob ""))]
    ["find-tool-call"
     (find-tool-call-event type
                           ts
                           sid
                           tid
                           "find"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'pattern "")
                           (hash-ref h 'path ""))]
    ["custom-tool-call"
     (custom-tool-call-event type
                             ts
                             sid
                             tid
                             (hash-ref h 'toolName "unknown")
                             (hash-ref h 'arguments (hasheq))
                             (hash-ref h 'toolCallId ""))]
    ["tool-call"
     (tool-call-event type
                      ts
                      sid
                      tid
                      (hash-ref h 'toolName "")
                      (hash-ref h 'arguments (hasheq))
                      (hash-ref h 'toolCallId ""))]
    ["tool-result"
     (tool-result-event type
                        ts
                        sid
                        tid
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'content "")
                        (hash-ref h 'isError #f))]
    ["provider-request"
     (provider-request-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["provider-response"
     (provider-response-event type
                              ts
                              sid
                              tid
                              (hash-ref h 'model "")
                              (hash-ref h 'provider "")
                              (hash-ref h 'latencyMs 0))]
    ["session-start" (session-start-event type ts sid tid (hash-ref h 'model ""))]
    ["session-shutdown" (session-shutdown-event type ts sid tid (hash-ref h 'reason ""))]
    ["input" (input-event type ts sid tid (hash-ref h 'inputType "") (hash-ref h 'content ""))]
    ["model-select"
     (model-select-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["agent-start" (agent-start-event type ts sid tid (hash-ref h 'model ""))]
    ["agent-end" (agent-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["context" (context-event type ts sid tid (hash-ref h 'tokenCount 0) (hash-ref h 'windowSize 0))]
    [_ (typed-event type ts sid tid)]))

;; ============================================================
;; Registry
;; ============================================================

(define (all-known-event-types)
  '("turn-start" "turn-end"
                 "message-start"
                 "message-update"
                 "message-end"
                 "tool-execution-start"
                 "tool-execution-update"
                 "tool-execution-end"
                 "tool-call"
                 "tool-result"
                 "bash-tool-call"
                 "edit-tool-call"
                 "write-tool-call"
                 "read-tool-call"
                 "grep-tool-call"
                 "find-tool-call"
                 "custom-tool-call"
                 "provider-request"
                 "provider-response"
                 "session-start"
                 "session-shutdown"
                 "input"
                 "model-select"
                 "agent-start"
                 "agent-end"
                 "context"))

(define (event-name->tool-name type)
  (match type
    ["bash-tool-call" "bash"]
    ["edit-tool-call" "edit"]
    ["write-tool-call" "write"]
    ["read-tool-call" "read"]
    ["grep-tool-call" "grep"]
    ["find-tool-call" "find"]
    [_ #f]))
