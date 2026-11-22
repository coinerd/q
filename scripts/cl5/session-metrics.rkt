#lang racket/base

;; scripts/cl5/session-metrics.rkt — derive CL0 §1 step-4 measurements from a
;; pi session transcript (CL5 W1 harness #9982).
;;
;; A pi session file is JSONL, one event per line. Observed event shapes
;; (verified against ~/.pi/agent/sessions/*​/*.jsonl, 2026-11):
;;   {"type":"session","timestamp":"2026-11-20T19:08:41.670Z", ...}
;;   {"type":"model_change","timestamp":...,"provider":"...","modelId":"..."}
;;   {"type":"thinking_level_change","timestamp":...,"thinkingLevel":"high"}
;;   {"type":"message","timestamp":...,
;;    "message":{"role":"assistant",
;;               "content":[{"type":"text","text":"..."},
;;                          {"type":"toolCall","id":"...","name":"bash",
;;                           "arguments":{"command":"..."}}],
;;               "usage":{"input":852,"output":34,"cacheRead":0,
;;                        "cacheWrite":0,"totalTokens":886}}}
;;   {"type":"message","timestamp":...,"message":{"role":"toolResult", ...}}
;;
;; The core, session-metrics, is PURE: it consumes an already-parsed list of
;; event hashes. read-session-events / session-metrics-file are the thin I/O
;; shell. Timestamps are ISO-8601 with a Z suffix; parsed here with
;; racket/date (no third-party dependencies).

(require json
         racket/date
         racket/list
         racket/set
         racket/string)

(provide iso8601->ms
         normalize-session-path
         read-session-events
         session-metrics
         session-metrics-file)

;; ---- timestamps ------------------------------------------------------------

(define iso8601-rx
  #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})(?:\\.([0-9]+))?Z$")

;; ISO-8601 Zulu string -> milliseconds since epoch, or #f when the shape is
;; not the pi session timestamp shape. Only Z timestamps are accepted: pi
;; session files always write Z, and silently mixing offsets would corrupt
;; the durations this harness exists to measure.
(define (iso8601->ms text)
  (and (string? text)
       (let ([m (regexp-match iso8601-rx text)])
         (and m
              (let ([second (string->number (list-ref m 6))]
                    [minute (string->number (list-ref m 5))]
                    [hour (string->number (list-ref m 4))]
                    [day (string->number (list-ref m 3))]
                    [month (string->number (list-ref m 2))]
                    [year (string->number (list-ref m 1))]
                    [fraction (list-ref m 7)])
                (+ (* 1000 (find-seconds second minute hour day month year))
                   (if fraction
                       (string->number (substring (string-append fraction "000") 0 3))
                       0)))))))

;; ---- path normalization ------------------------------------------------------

;; Normalize a repo-root string: absolute, no trailing slash.
(define (normalize-root repo-root)
  (and repo-root
       (let ([text (if (path? repo-root)
                       (path->string repo-root)
                       repo-root)])
         (string-trim text "/" #:left? #f #:repeat? #t))))

(define (absolute-unix-path? text)
  (and (string? text) (> (string-length text) 0) (char=? (string-ref text 0) #\/)))

(define (strip-dot-slash text)
  (if (string-prefix? text "./")
      (strip-dot-slash (substring text 2))
      text))

;; Tool-call path -> repo-relative path string (or the raw string when it
;; cannot be normalized). Absolute paths are relativized only under the
;; caller-supplied repo root; anything else is returned unchanged so it is
;; still counted as a distinct, recognizable path.
(define (normalize-session-path raw repo-root)
  (define root (normalize-root repo-root))
  (cond
    [(not (string? raw)) #f]
    [(absolute-unix-path? raw)
     (if (and root (string-prefix? raw (string-append root "/")))
         (substring raw (add1 (string-length root)))
         raw)]
    [else (strip-dot-slash raw)]))

;; Write-target classification: (values path in-repo?). A write outside the
;; repo root (absolute, not under root, or no root supplied) is in-repo? = #f
;; and therefore always unrelated (CL0: writes are graded repo-relative).
(define (normalize-write-target raw repo-root)
  (define root (normalize-root repo-root))
  (cond
    [(not (string? raw)) (values #f #f)]
    [(absolute-unix-path? raw)
     (if (and root (string-prefix? raw (string-append root "/")))
         (values (substring raw (add1 (string-length root))) #t)
         (values raw #f))]
    [else (values (strip-dot-slash raw) #t)]))

;; ---- event access --------------------------------------------------------------

(define (event-type event)
  (hash-ref event 'type #f))

(define (event-ms event)
  (iso8601->ms (hash-ref event 'timestamp #f)))

(define (message-events events)
  (filter (lambda (event) (equal? (event-type event) "message")) events))

(define (assistant-usage-total message)
  (define usage (hash-ref message 'usage (hash)))
  (define (num key)
    (let ([value (hash-ref usage key 0)]) (if (number? value) value 0)))
  (+ (num 'input) (num 'cacheRead) (num 'cacheWrite) (num 'output)))

;; Context size at a turn: the prompt the model had to re-read (input plus
;; cache reads/writes), excluding generated output. Summing this over turns
;; would double-count the transcript, so only the last value before the edit
;; is reported as the context-consumption measurement.
(define (assistant-context-total message)
  (define usage (hash-ref message 'usage (hash)))
  (define (num key)
    (let ([value (hash-ref usage key 0)]) (if (number? value) value 0)))
  (+ (num 'input) (num 'cacheRead) (num 'cacheWrite)))

;; An assistant message with a text block is answer material. The LAST such
;; message is the navigation answer point (CL0 §1: "before the final answer").
(define (message-has-text? message)
  (define content (hash-ref message 'content '()))
  (and (list? content)
       (for/or ([item (in-list content)])
         (and (hash? item) (equal? (hash-ref item 'type #f) "text")))))

(define (message-tool-calls message)
  (filter (lambda (item) (and (hash? item) (equal? (hash-ref item 'type #f) "toolCall")))
          (let ([content (hash-ref message 'content '())])
            (if (list? content)
                content
                '()))))

;; Does any string inside the serialized tool-call arguments name one of the
;; owner paths? Serialized once per call so nested argument shapes (edit
;; batches, bash commands) are all covered.
(define (arguments-mention-paths? arguments owner-paths)
  (define serialized
    (with-handlers ([exn:fail? (lambda (_) "")])
      (jsexpr->string arguments)))
  (for/or ([owner (in-list owner-paths)])
    (string-contains? serialized owner)))

;; ---- the metric core -------------------------------------------------------------

;; session-metrics : (listof hash) (listof string) [#:repo-root string/path/#f]
;;                   [#:owner-paths (listof string)] -> hash
;;
;; ownership-set: the task's repo-relative ownership set (task-spec
;; 'ownership-set); the first in-scope edit is the first write/edit tool call
;; whose normalized target is a member. owner-paths (default: ownership-set)
;; are the authoritative-implementation paths used for owner-location time —
;; for navigation tasks they come from the spec's 'owner-location-paths
;; because the ownership set itself is empty there.
;;
;; Boundary conventions (CL0 §1 step 4): context tokens are summed over
;; assistant messages up to AND INCLUDING the message that carries the first
;; in-scope edit; files-read and investigation calls are counted STRICTLY
;; BEFORE that edit tool call (so reads in the same message before the edit
;; count, reads after it do not). With no in-scope edit, the four
;; edit-relative fields are #f.
(define (session-metrics events
                         ownership-set
                         #:repo-root [repo-root #f]
                         #:owner-paths [owner-paths ownership-set])
  (define start-ms
    (or (for/first ([event (in-list events)]
                    #:when (and (equal? (event-type event) "session") (event-ms event)))
          (event-ms event))
        (for/first ([event (in-list events)]
                    #:when (event-ms event))
          (event-ms event))))
  (define last-ms
    (for/last ([event (in-list events)]
               #:when (event-ms event))
      (event-ms event)))
  (define token-sum 0)
  (define context-at-edit #f)
  (define last-context #f)
  ;; Answer-point accumulators: snapshot at every assistant text message so the
  ;; LAST snapshot is "before the final answer" for navigation tasks.
  (define answer-tokens 0)
  (define answer-context #f)
  (define answer-files 0)
  (define answer-calls 0)
  (define answer-ms #f)
  (define files-read (mutable-set))
  (define investigation-calls 0)
  (define first-edit-found? #f)
  (define first-edit-ms #f)
  (define owner-location-ms #f)
  (define in-scope-writes (mutable-set))
  (define in-scope-edit-count 0)
  (define unrelated-writes (mutable-set))
  (define tool-call-count 0)
  (define assistant-turn-count 0)
  (define model #f)
  (define thinking-level #f)
  (for ([event (in-list events)])
    (define type (event-type event))
    (define ms (event-ms event))
    (cond
      [(equal? type "model_change")
       (set! model (string-append (hash-ref event 'provider "?") "/" (hash-ref event 'modelId "?")))]
      [(equal? type "thinking_level_change") (set! thinking-level (hash-ref event 'thinkingLevel #f))]
      [(equal? type "message")
       (define message (hash-ref event 'message (hash)))
       (when (equal? (hash-ref message 'role #f) "assistant")
         (set! assistant-turn-count (add1 assistant-turn-count))
         ;; Inclusive token accounting: this message is counted when the first
         ;; in-scope edit has not happened in any EARLIER message.
         (set! last-context (assistant-context-total message))
         (unless first-edit-found?
           (set! token-sum (+ token-sum (assistant-usage-total message)))
           (set! context-at-edit (assistant-context-total message)))
         (for ([call (in-list (message-tool-calls message))])
           (set! tool-call-count (add1 tool-call-count))
           (define name (hash-ref call 'name #f))
           (define arguments (hash-ref call 'arguments (hash)))
           (unless owner-location-ms
             (when (and start-ms ms (arguments-mention-paths? arguments owner-paths))
               (set! owner-location-ms (- ms start-ms))))
           (define write-target? (and (member name '("write" "edit")) (hash-ref arguments 'path #f)))
           (define-values (write-path write-in-repo?)
             (if write-target?
                 (normalize-write-target (hash-ref arguments 'path) repo-root)
                 (values #f #f)))
           (define first-edit-here?
             (and write-target?
                  (not first-edit-found?)
                  write-in-repo?
                  write-path
                  (member write-path ownership-set)
                  #t))
           ;; Strictly-before accounting for the investigation metrics.
           (unless (or first-edit-found? first-edit-here?)
             (set! investigation-calls (add1 investigation-calls)))
           (when (and (equal? name "read") (not first-edit-found?) (not first-edit-here?))
             (define read-path (normalize-session-path (hash-ref arguments 'path #f) repo-root))
             (when read-path
               (set-add! files-read read-path)))
           (when write-target?
             (cond
               [(and write-in-repo? (member write-path ownership-set))
                (set-add! in-scope-writes write-path)
                (set! in-scope-edit-count (add1 in-scope-edit-count))
                (unless first-edit-found?
                  (set! first-edit-found? #t)
                  (set! first-edit-ms (and start-ms ms (- ms start-ms))))]
               [write-path (set-add! unrelated-writes write-path)])))
         ;; Snapshot AFTER this message's calls are processed: the answer point
         ;; is the end of the last assistant message that carried text.
         (when (message-has-text? message)
           (set! answer-tokens token-sum)
           (set! answer-context last-context)
           (set! answer-files (set-count files-read))
           (set! answer-calls investigation-calls)
           (set! answer-ms (and start-ms ms (- ms start-ms)))))]))
  (hash 'tokens-before-first-in-scope-edit
        (and first-edit-found? token-sum)
        'context-tokens-at-first-in-scope-edit
        (and first-edit-found? context-at-edit)
        'files-read-before-first-edit
        (and first-edit-found? (set-count files-read))
        'investigation-calls-before-first-edit
        (and first-edit-found? investigation-calls)
        'tokens-before-answer
        (and answer-ms answer-tokens)
        'context-tokens-at-answer
        answer-context
        'files-read-before-answer
        (and answer-ms answer-files)
        'investigation-calls-before-answer
        (and answer-ms answer-calls)
        'answer-ms
        answer-ms
        'owner-location-ms
        owner-location-ms
        'first-edit-ms
        first-edit-ms
        'elapsed-ms
        (and start-ms last-ms (- last-ms start-ms))
        'in-scope-edit-count
        in-scope-edit-count
        'written-subsystems
        (sort (map subsystem-of (set->list in-scope-writes)) string<?)
        'unrelated-writes
        (sort (set->list unrelated-writes) string<?)
        'tool-call-count
        tool-call-count
        'assistant-turn-count
        assistant-turn-count
        'model
        model
        'thinking-level
        thinking-level))

;; First path segment of a repo-relative path; "root" for a top-level file.
(define (subsystem-of path)
  (define parts (string-split path "/"))
  (if (= (length parts) 1)
      "root"
      (first parts)))

;; ---- I/O shell -----------------------------------------------------------------

;; path -> (values events skipped-line-count). Unparsable lines (including
;; blank lines) are skipped and counted under 'skipped-lines; they are never
;; silently dropped from the record.
(define (read-session-events path)
  (define events '())
  (define skipped 0)
  (call-with-input-file path
                        (lambda (in)
                          (for ([line (in-lines in)])
                            (with-handlers ([exn:fail? (lambda (_) (set! skipped (add1 skipped)))])
                              (define value (string->jsexpr line))
                              (if (hash? value)
                                  (set! events (cons value events))
                                  (set! skipped (add1 skipped)))))))
  (values (reverse events) skipped))

;; Read a session file and compute metrics; adds 'skipped-lines and
;; 'session-file to the metric hash.
(define (session-metrics-file path
                              ownership-set
                              #:repo-root [repo-root #f]
                              #:owner-paths [owner-paths ownership-set])
  (define-values (events skipped) (read-session-events path))
  (hash-set* (session-metrics events ownership-set #:repo-root repo-root #:owner-paths owner-paths)
             'skipped-lines
             skipped
             'session-file
             (path->string path)))
