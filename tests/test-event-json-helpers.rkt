#lang racket/base

;; @speed fast
;; @suite fast

;; W4 v0.99.35: Tests for event-json-helpers.rkt
;; Pure serialization boundary functions extracted from event-json.rkt:
;; - event-name->tool-name: pure event type to tool name mapping
;; - all-known-event-types: pure registry list
;; - serialize-*-tool-call: pure evt -> hasheq extraction
;; - deserialize-*-tool-call: pure type ts sid tid h -> event construction

(require rackunit
         rackunit/text-ui
         "../agent/event-json-helpers.rkt"
         (only-in "../agent/event-structs/tool-events.rkt"
                  bash-tool-call-event
                  bash-tool-call-event?
                  bash-tool-call-event-command
                  bash-tool-call-event-timeout
                  bash-tool-call-event-cwd
                  edit-tool-call-event
                  edit-tool-call-event?
                  edit-tool-call-event-path
                  edit-tool-call-event-edits
                  write-tool-call-event
                  write-tool-call-event?
                  write-tool-call-event-path
                  write-tool-call-event-content
                  read-tool-call-event
                  read-tool-call-event?
                  read-tool-call-event-path
                  read-tool-call-event-offset
                  read-tool-call-event-limit
                  grep-tool-call-event
                  grep-tool-call-event?
                  grep-tool-call-event-pattern
                  grep-tool-call-event-path
                  grep-tool-call-event-glob
                  find-tool-call-event
                  find-tool-call-event?
                  find-tool-call-event-pattern
                  find-tool-call-event-path
                  custom-tool-call-event
                  custom-tool-call-event?
                  tool-call-event-tool-call-id
                  tool-call-event-tool-name))

(define-test-suite event-name-mapping-tests
                   (test-case "event-name->tool-name maps known tool types"
                     (check-equal? (event-name->tool-name "tool.bash.called") "bash")
                     (check-equal? (event-name->tool-name "tool.edit.called") "edit")
                     (check-equal? (event-name->tool-name "tool.write.called") "write")
                     (check-equal? (event-name->tool-name "tool.read.called") "read")
                     (check-equal? (event-name->tool-name "tool.grep.called") "grep")
                     (check-equal? (event-name->tool-name "tool.find.called") "find"))
                   (test-case "event-name->tool-name returns #f for unknown"
                     (check-false (event-name->tool-name "tool.custom.called"))
                     (check-false (event-name->tool-name "session.started"))
                     (check-false (event-name->tool-name "unknown.event"))
                     (check-false (event-name->tool-name "")))
                   (test-case "all-known-event-types returns non-empty list"
                     (define types (all-known-event-types))
                     (check-true (and (pair? types) #t) "returns a non-empty list"))
                   (test-case "all-known-event-types includes key types"
                     (define types (all-known-event-types))
                     (check-true (and (member "turn.started" types) #t))
                     (check-true (and (member "tool.bash.called" types) #t))
                     (check-true (and (member "session.started" types) #t))
                     (check-true (and (member "browser.session.opened" types) #t)))
                   (test-case "all-known-event-types is consistent across calls"
                     (check-equal? (all-known-event-types) (all-known-event-types))))

(define-test-suite
 bash-serializer-tests
 (test-case "serialize-bash-tool-call extracts all fields"
   (define evt
     (bash-tool-call-event "tool.bash.called"
                           12345
                           "s1"
                           "t1"
                           "bash"
                           (hasheq)
                           "tc-1"
                           "ls -la"
                           60
                           "/tmp"))
   (define h (serialize-bash-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "bash")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'command) "ls -la")
   (check-equal? (hash-ref h 'timeout) 60)
   (check-equal? (hash-ref h 'cwd) "/tmp"))
 (test-case "deserialize-bash-tool-call constructs event from hash"
   (define h (hasheq 'toolName "bash" 'toolCallId "tc-2" 'command "echo hi" 'timeout 30 'cwd "/home"))
   (define evt (deserialize-bash-tool-call "tool.bash.called" 100 "s2" "t2" h))
   (check-true (bash-tool-call-event? evt))
   (check-equal? (bash-tool-call-event-command evt) "echo hi")
   (check-equal? (bash-tool-call-event-timeout evt) 30)
   (check-equal? (bash-tool-call-event-cwd evt) "/home"))
 (test-case "deserialize-bash-tool-call uses defaults for missing fields"
   (define h (hasheq))
   (define evt (deserialize-bash-tool-call "tool.bash.called" 100 "s2" "t2" h))
   (check-true (bash-tool-call-event? evt))
   (check-equal? (bash-tool-call-event-command evt) "")
   (check-equal? (bash-tool-call-event-timeout evt) 30)
   (check-equal? (bash-tool-call-event-cwd evt) "")))

(define-test-suite
 edit-serializer-tests
 (test-case "serialize-edit-tool-call extracts all fields"
   (define evt
     (edit-tool-call-event "tool.edit.called" 12345 "s1" "t1" "edit" (hasheq) "tc-1" "/foo.rkt" '()))
   (define h (serialize-edit-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "edit")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'path) "/foo.rkt")
   (check-equal? (hash-ref h 'edits) '()))
 (test-case "deserialize-edit-tool-call constructs event"
   (define h (hasheq 'toolName "edit" 'toolCallId "tc-2" 'path "/bar.rkt" 'edits (list "edit1")))
   (define evt (deserialize-edit-tool-call "tool.edit.called" 100 "s2" "t2" h))
   (check-true (edit-tool-call-event? evt))
   (check-equal? (edit-tool-call-event-path evt) "/bar.rkt")
   (check-equal? (edit-tool-call-event-edits evt) (list "edit1"))))

(define-test-suite
 write-serializer-tests
 (test-case "serialize-write-tool-call extracts all fields"
   (define evt
     (write-tool-call-event "tool.write.called"
                            12345
                            "s1"
                            "t1"
                            "write"
                            (hasheq)
                            "tc-1"
                            "/out.txt"
                            "hello"))
   (define h (serialize-write-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "write")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'path) "/out.txt")
   (check-equal? (hash-ref h 'content) "hello"))
 (test-case "deserialize-write-tool-call constructs event"
   (define h (hasheq 'toolName "write" 'toolCallId "tc-2" 'path "/out2.txt" 'content "world"))
   (define evt (deserialize-write-tool-call "tool.write.called" 100 "s2" "t2" h))
   (check-true (write-tool-call-event? evt))
   (check-equal? (write-tool-call-event-path evt) "/out2.txt")
   (check-equal? (write-tool-call-event-content evt) "world")))

(define-test-suite
 read-serializer-tests
 (test-case "serialize-read-tool-call extracts all fields"
   (define evt
     (read-tool-call-event "tool.read.called" 12345 "s1" "t1" "read" (hasheq) "tc-1" "/in.txt" 0 100))
   (define h (serialize-read-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "read")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'path) "/in.txt")
   (check-equal? (hash-ref h 'offset) 0)
   (check-equal? (hash-ref h 'limit) 100))
 (test-case "deserialize-read-tool-call constructs event"
   (define h (hasheq 'toolName "read" 'toolCallId "tc-2" 'path "/in2.txt" 'offset 10 'limit 50))
   (define evt (deserialize-read-tool-call "tool.read.called" 100 "s2" "t2" h))
   (check-true (read-tool-call-event? evt))
   (check-equal? (read-tool-call-event-path evt) "/in2.txt")
   (check-equal? (read-tool-call-event-offset evt) 10)
   (check-equal? (read-tool-call-event-limit evt) 50)))

(define-test-suite
 grep-serializer-tests
 (test-case "serialize-grep-tool-call extracts all fields"
   (define evt
     (grep-tool-call-event "tool.grep.called"
                           12345
                           "s1"
                           "t1"
                           "grep"
                           (hasheq)
                           "tc-1"
                           "pattern"
                           "/src"
                           "*.rkt"))
   (define h (serialize-grep-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "grep")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'pattern) "pattern")
   (check-equal? (hash-ref h 'path) "/src")
   (check-equal? (hash-ref h 'glob) "*.rkt"))
 (test-case "deserialize-grep-tool-call constructs event"
   (define h (hasheq 'toolName "grep" 'toolCallId "tc-2" 'pattern "foo" 'path "/lib" 'glob "*.rkt"))
   (define evt (deserialize-grep-tool-call "tool.grep.called" 100 "s2" "t2" h))
   (check-true (grep-tool-call-event? evt))
   (check-equal? (grep-tool-call-event-pattern evt) "foo")
   (check-equal? (grep-tool-call-event-path evt) "/lib")
   (check-equal? (grep-tool-call-event-glob evt) "*.rkt")))

(define-test-suite
 find-serializer-tests
 (test-case "serialize-find-tool-call extracts all fields"
   (define evt
     (find-tool-call-event "tool.find.called" 12345 "s1" "t1" "find" (hasheq) "tc-1" "*.rkt" "/root"))
   (define h (serialize-find-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "find")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'pattern) "*.rkt")
   (check-equal? (hash-ref h 'path) "/root"))
 (test-case "deserialize-find-tool-call constructs event"
   (define h (hasheq 'toolName "find" 'toolCallId "tc-2" 'pattern "*.txt" 'path "/home"))
   (define evt (deserialize-find-tool-call "tool.find.called" 100 "s2" "t2" h))
   (check-true (find-tool-call-event? evt))
   (check-equal? (find-tool-call-event-pattern evt) "*.txt")
   (check-equal? (find-tool-call-event-path evt) "/home")))

(define-test-suite
 custom-serializer-tests
 (test-case "serialize-custom-tool-call extracts all fields"
   (define evt
     (custom-tool-call-event "tool.custom.called"
                             12345
                             "s1"
                             "t1"
                             "mytool"
                             (hasheq 'arg "val")
                             "tc-1"))
   (define h (serialize-custom-tool-call evt))
   (check-equal? (hash-ref h 'toolName) "mytool")
   (check-equal? (hash-ref h 'toolCallId) "tc-1")
   (check-equal? (hash-ref h 'arguments) (hasheq 'arg "val")))
 (test-case "deserialize-custom-tool-call constructs event"
   (define h (hasheq 'toolName "customtool" 'toolCallId "tc-2" 'arguments (hasheq 'key "data")))
   (define evt (deserialize-custom-tool-call "tool.custom.called" 100 "s2" "t2" h))
   (check-true (custom-tool-call-event? evt))
   (check-equal? (tool-call-event-tool-name evt) "customtool")
   (check-equal? (tool-call-event-tool-call-id evt) "tc-2"))
 (test-case "deserialize-custom-tool-call defaults for missing toolName"
   (define h (hasheq))
   (define evt (deserialize-custom-tool-call "tool.custom.called" 100 "s2" "t2" h))
   (check-true (custom-tool-call-event? evt))
   (check-equal? (tool-call-event-tool-name evt) "unknown")))

(define-test-suite
 round-trip-consistency-tests
 (test-case "bash serialize->deserialize preserves fields"
   (define orig
     (bash-tool-call-event "tool.bash.called" 42 "s" "t" "bash" (hasheq) "id-1" "pwd" 45 "/var"))
   (define h (serialize-bash-tool-call orig))
   (define rt (deserialize-bash-tool-call "tool.bash.called" 42 "s" "t" h))
   (check-equal? (bash-tool-call-event-command rt) (bash-tool-call-event-command orig))
   (check-equal? (bash-tool-call-event-timeout rt) (bash-tool-call-event-timeout orig))
   (check-equal? (bash-tool-call-event-cwd rt) (bash-tool-call-event-cwd orig)))
 (test-case "edit serialize->deserialize preserves fields"
   (define orig
     (edit-tool-call-event "tool.edit.called" 42 "s" "t" "edit" (hasheq) "id-1" "/x.rkt" (list "e1")))
   (define h (serialize-edit-tool-call orig))
   (define rt (deserialize-edit-tool-call "tool.edit.called" 42 "s" "t" h))
   (check-equal? (edit-tool-call-event-path rt) (edit-tool-call-event-path orig))
   (check-equal? (edit-tool-call-event-edits rt) (edit-tool-call-event-edits orig))))

(define-test-suite all-event-json-helpers-tests
                   event-name-mapping-tests
                   bash-serializer-tests
                   edit-serializer-tests
                   write-serializer-tests
                   read-serializer-tests
                   grep-serializer-tests
                   find-serializer-tests
                   custom-serializer-tests
                   round-trip-consistency-tests)

(module+ test
  (run-tests all-event-json-helpers-tests))

(module+ main
  (run-tests all-event-json-helpers-tests))
