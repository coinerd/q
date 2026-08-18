#lang racket/base

;; @speed fast
;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-command-intent.rkt — Command intent boundary + corpus tests
;; @boundary unit
;;
;; v0.99.89 W3 "Command Parsing & Intent Boundary": the parser stays I/O-free
;; and command INTENT is classified purely (command-parser.rkt
;; gsd-command-intent / command-wave-intent), separate from the executor.
;; The command corpus pins valid + malformed commands to their exact parse
;; and intent; the fitness test proves the parser has no I/O imports; and the
;; /go N assertion semantics (go-wave-valid? ≡ assert-go-n) are pinned so the
;; executor's use of the pure intent cannot drift.

(require rackunit
         racket/list
         racket/file
         racket/string
         "helpers/arch-utils.rkt"
         "../extensions/gsd/command-parser.rkt")

;; ============================================================
;; Command corpus: (input → parsed-kind? → intent)
;; ============================================================

(define (parse-kind cmd input)
  (define p (parse-gsd-command cmd input))
  (and p
       (cond
         [(gsd-cmd-go? p) 'go]
         [(gsd-cmd-plan? p) 'plan]
         [(gsd-cmd-skip? p) 'skip]
         [(gsd-cmd-done? p) 'done]
         [(gsd-cmd-status? p) 'status]
         [(gsd-cmd-replan? p) 'replan]
         [(gsd-cmd-reset? p) 'reset]
         [(gsd-cmd-wave-done? p) 'wave-done]
         [(gsd-cmd-artifact? p) 'artifact]
         [else 'other])))

;; (input expected-kind expected-intent) — cmd is derived from the input's
;; leading token so aliases are exercised exactly as a user would type them.
(define (split-cmd input)
  (car (string-split input)))

(define corpus
  '(("/go" go (go-all)) ("/go 3" go (go-wave 3))
                        ("/go 0" go (go-wave 0))
                        ("/implement" go (go-all))
                        ("/implement 2" go (go-wave 2))
                        ("/i 5" go (go-wave 5))
                        ("/go 3 extra" go (go-all)) ; last token non-numeric
                        ("/go x" go (go-all)) ; non-numeric arg
                        ("/plan" plan (plan-display))
                        ("/plan implement feature X" plan (plan-text "implement feature X"))
                        ("/p do something" plan (plan-text "do something"))
                        ("/p" plan (plan-display))
                        ("/state" artifact (display "STATE"))
                        ("/s" artifact (display "STATE"))
                        ("/handoff" artifact (display "HANDOFF"))
                        ("/ho" artifact (display "HANDOFF"))
                        ("/gsd" status (status))
                        ("/replan" replan (replan))
                        ("/skip" skip (skip-all))
                        ("/skip 2" skip (skip-wave 2))
                        ("/skip abc" skip (skip-all))
                        ("/reset" reset (reset))
                        ("/wave-done 1" wave-done (wave-done-wave 1))
                        ("/wave-done" wave-done (wave-done-unspecified))
                        ("/wd 5" wave-done (wave-done-wave 5))
                        ("/wd nope" wave-done (wave-done-unspecified))
                        ("/done" done (done-default))
                        ("/done --force" done (done-force))
                        ("/done --forcex" done (done-force)) ; substring match is legacy — pinned
                        ("/done --force more" done (done-force))))

(test-case "command corpus: every input parses to the expected kind"
  (for ([entry corpus])
    (define input (car entry))
    (define expected-kind (cadr entry))
    (define got (parse-kind (split-cmd input) input))
    (check-eq? got expected-kind (format "~a → ~a (expected ~a)" input got expected-kind))))

(test-case "command corpus: every input classifies to the expected intent"
  (for ([entry corpus])
    (define input (car entry))
    (define expected-intent (caddr entry))
    (define parsed (parse-gsd-command (split-cmd input) input))
    (check-not-false parsed (format "~a must parse" input))
    (check-equal? (gsd-command-intent parsed)
                  expected-intent
                  (format "~a → ~a" input (gsd-command-intent parsed)))))

;; ============================================================
;; Malformed inputs
;; ============================================================

(test-case "malformed: unknown commands return #f and unknown intent"
  (for ([input '("/unknown" "/foo" "/goo" "/")])
    (check-false (parse-gsd-command (split-cmd input) input) (format "~a must not parse" input))))

(test-case "malformed: non-slash inputs with a valid cmd still parse (args empty)"
  ;; The pure parser keys on the cmd token; a non-slash input-text is treated
  ;; as args (extract-cmd-args returns ""). This pins the existing contract.
  (define r (parse-gsd-command "/go" "go"))
  (check-pred gsd-cmd-go? r)
  (check-equal? (parsed-gsd-command-args r) "")
  (check-equal? (gsd-command-intent r) '(go-all))
  (define r2 (parse-gsd-command "/go" "3"))
  (check-pred gsd-cmd-go? r2)
  ;; "3" is not slash-prefixed so extract-cmd-args drops it → go-all.
  (check-equal? (gsd-command-intent r2) '(go-all)))

(test-case "intent: unparsable commands classify to unknown"
  (check-equal? (gsd-command-intent #f) '(unknown)))

;; ============================================================
;; command-wave-intent extraction
;; ============================================================

(test-case "command-wave-intent: trailing numeric token"
  (check-equal? (command-wave-intent "3") 3)
  (check-equal? (command-wave-intent "2") 2)
  (check-equal? (command-wave-intent "0") 0)
  (check-equal? (command-wave-intent "") #f)
  (check-equal? (command-wave-intent "3 extra") #f)
  (check-equal? (command-wave-intent "abc") #f)
  (check-equal? (command-wave-intent "3.5") #f)
  (check-equal? (command-wave-intent "-1") #f)
  (check-false (command-wave-intent "  ")))

(test-case "command-wave-timeout-arg: --wave-timeout=SECONDS flag"
  (check-equal? (command-wave-timeout-arg "--wave-timeout=3600") 3600)
  (check-equal? (command-wave-timeout-arg "3 --wave-timeout=7200") 7200)
  (check-equal? (command-wave-timeout-arg "--wave-timeout=1800 3") 1800)
  (check-equal? (command-wave-timeout-arg "") #f)
  (check-equal? (command-wave-timeout-arg "3") #f)
  (check-equal? (command-wave-timeout-arg "--wave-timeout") #f)
  (check-equal? (command-wave-timeout-arg "--wave-timeout=") #f)
  (check-equal? (command-wave-timeout-arg "--wave-timeout=abc") #f)
  (check-false (command-wave-timeout-arg "  ")))

(test-case "command-wave-timeout-arg: never collides with wave-intent"
  ;; The flag is keyword=value, so /go N keeps its trailing-numeric intent
  ;; while /go --wave-timeout=SECONDS classifies as go-all (no wave number).
  (check-equal? (command-wave-intent "3 --wave-timeout=7200") #f)
  (check-equal? (command-wave-timeout-arg "3 --wave-timeout=7200") 7200)
  (check-equal? (command-wave-intent "--wave-timeout=3600") #f)
  (check-equal? (command-wave-timeout-arg "--wave-timeout=3600") 3600))

;; ============================================================
;; /go N assertion semantics (pure mirror of assert-go-n)
;; ============================================================

(test-case "go-wave-valid?: requested equals next-actionable only"
  (check-true (go-wave-valid? 0 0))
  (check-true (go-wave-valid? 1 1))
  (check-false (go-wave-valid? 2 0))
  (check-false (go-wave-valid? 0 1))
  (check-false (go-wave-valid? 1 0))
  ;; No next actionable wave → no /go N is valid.
  (check-false (go-wave-valid? 0 #f))
  (check-false (go-wave-valid? 1 #f)))

(test-case "go-wave-valid?: matches assert-go-n over the actionable space"
  ;; assert-go-n (go-orchestrator.rkt): (and next (= n next)). The pure mirror
  ;; must agree for every (requested, next) pair that can arise.
  (for* ([next '(#f 0 1 2)]
         [requested '(0 1 2)])
    (define expected (and next (= requested next)))
    (check-equal? (go-wave-valid? requested next)
                  expected
                  (format "requested=~a next=~a" requested next))))

;; ============================================================
;; Parser fitness: I/O-free require scan
;; ============================================================

(define (spec-module-path spec)
  (cond
    [(symbol? spec) (symbol->string spec)]
    [(string? spec) spec]
    [(pair? spec)
     (case (car spec)
       [(only-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (cadr spec)
            #f)]
       ;; (prefix-in "pref" module-path): the module path is the SECOND arg.
       [(prefix-in)
        (if (and (pair? (cdr spec)) (pair? (cddr spec)) (string? (caddr spec)))
            (caddr spec)
            #f)]
       [else #f])]
    [else #f]))

(define (module-imports path)
  (define reqs (extract-requires (path->string path)))
  (for/list ([spec (in-list reqs)]
             #:when (spec-module-path spec))
    (spec-module-path spec)))

;; Normalize an import to its module basename: "../../util/command-helpers.rkt"
;; → "command-helpers", "racket/file" → "file". Both the forbidden and the
;; allowed lists below are written in this normalized form.
(define (normalize-import i)
  (define base (car (reverse (string-split i "/"))))
  (if (string-suffix? base ".rkt")
      (substring base 0 (- (string-length base) 4))
      base))

(define forbidden-parser-imports
  '("file" "port" "path" "date" "system" "runtime-path" "openssl" "net" "tcp"))

(test-case "parser fitness: command-parser.rkt is I/O-free"
  (define path (build-path q-dir "extensions" "gsd" "command-parser.rkt"))
  (check-true (file-exists? path))
  (define imports (map normalize-import (module-imports path)))
  (check-true (pair? imports) "parser has require forms")
  (define violations
    (for/list ([i (in-list imports)]
               #:when (member i forbidden-parser-imports))
      i))
  (check-equal? violations '() (format "parser imports I/O modules: ~a" violations))
  ;; The parser may only depend on pure base libs + the pure command helpers.
  (define allowed '("base" "match" "string" "command-helpers" "command-types"))
  (define unexpected
    (for/list ([i (in-list imports)]
               #:unless (member i allowed))
      i))
  (check-equal? unexpected '() (format "parser imports unexpected modules: ~a" unexpected)))

(test-case "parser fitness: command-helpers.rkt is I/O-free and whitelisted"
  (define path (build-path q-dir "util" "command-helpers.rkt"))
  (check-true (file-exists? path))
  (define imports (map normalize-import (module-imports path)))
  (define violations
    (for/list ([i (in-list imports)]
               #:when (member i forbidden-parser-imports))
      i))
  (check-equal? violations '() (format "command-helpers imports I/O modules: ~a" violations))
  ;; Strict whitelist: command-helpers may only depend on base/contract/string.
  (define allowed '("base" "contract" "string"))
  (define unexpected
    (for/list ([i (in-list imports)]
               #:unless (member i allowed))
      i))
  (check-equal? unexpected '() (format "command-helpers imports unexpected modules: ~a" unexpected)))

;; ============================================================
;; Intent classification edges
;; ============================================================

(test-case "intent: /go intent preserves the wave-arg exactly"
  (define p (parse-gsd-command "/go" "/go 7"))
  (check-equal? (gsd-command-intent p) '(go-wave 7))
  (define q (parse-gsd-command "/i" "/i"))
  (check-equal? (gsd-command-intent q) '(go-all)))

(test-case "intent: artifact display names"
  (check-equal? (gsd-command-intent (parse-gsd-command "/state" "/state")) '(display "STATE"))
  (check-equal? (gsd-command-intent (parse-gsd-command "/ho" "/ho")) '(display "HANDOFF")))

(test-case "intent: done force/default"
  (check-equal? (gsd-command-intent (parse-gsd-command "/done" "/done")) '(done-default))
  (check-equal? (gsd-command-intent (parse-gsd-command "/done" "/done --force")) '(done-force)))
