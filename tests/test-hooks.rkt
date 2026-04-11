#lang racket

(require rackunit
         "../extensions/hooks.rkt"
         "../extensions/api.rkt")

;; ============================================================
;; Hook result constructors
;; ============================================================

(test-case "hook-pass creates pass result"
  (define r (hook-pass "data"))
  (check-pred hook-result? r)
  (check-eq? (hook-result-action r) 'pass)
  (check-equal? (hook-result-payload r) "data"))

(test-case "hook-pass default payload is #f"
  (define r (hook-pass))
  (check-false (hook-result-payload r)))

(test-case "hook-amend creates amend result"
  (define r (hook-amend "new-data"))
  (check-eq? (hook-result-action r) 'amend)
  (check-equal? (hook-result-payload r) "new-data"))

(test-case "hook-block creates block result"
  (define r (hook-block "denied"))
  (check-eq? (hook-result-action r) 'block)
  (check-equal? (hook-result-payload r) "denied"))

(test-case "hook-block default reason is #f"
  (define r (hook-block))
  (check-false (hook-result-payload r)))

;; ============================================================
;; dispatch-hooks — no handlers
;; ============================================================

(test-case "dispatch-hooks with no handlers returns pass"
  (define reg (make-extension-registry))
  (define result (dispatch-hooks 'before-send "payload" reg))
  (check-eq? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "payload"))

;; ============================================================
;; dispatch-hooks — pass through
;; ============================================================

(test-case "dispatch-hooks with pass handlers preserves payload"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "ext" "1" "1"
               (hasheq 'test (λ (p) (hook-pass p)))))
  (define result (dispatch-hooks 'test "original" reg))
  (check-eq? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "original"))

;; ============================================================
;; dispatch-hooks — amend chain
;; ============================================================

(test-case "dispatch-hooks with amend updates payload"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "amender" "1" "1"
               (hasheq 'test (λ (p) (hook-amend "amended")))))
  (define result (dispatch-hooks 'test "original" reg))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "amended"))

(test-case "dispatch-hooks chains amend then pass"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "amender" "1" "1"
               (hasheq 'test (λ (p) (hook-amend "changed")))))
  (register-extension! reg
    (extension "passer" "1" "1"
               (hasheq 'test (λ (p) (hook-pass p)))))
  (define result (dispatch-hooks 'test "original" reg))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "changed"))

;; ============================================================
;; dispatch-hooks — block stops dispatch
;; ============================================================

(test-case "dispatch-hooks with block stops immediately"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "blocker" "1" "1"
               (hasheq 'test (λ (p) (hook-block "nope")))))
  (define result (dispatch-hooks 'test "payload" reg))
  (check-eq? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "nope"))

(test-case "dispatch-hooks block prevents later handlers from running"
  (define reg (make-extension-registry))
  (define side-effect-box (box #f))
  (register-extension! reg
    (extension "blocker" "1" "1"
               (hasheq 'test (λ (p) (hook-block "stop")))))
  (register-extension! reg
    (extension "effect" "1" "1"
               (hasheq 'test (λ (p) (set-box! side-effect-box #t) (hook-pass p)))))
  (dispatch-hooks 'test "payload" reg)
  (check-false (unbox side-effect-box)))

;; ============================================================
;; dispatch-hooks — exception isolation
;; ============================================================

(test-case "dispatch-hooks isolates handler exceptions"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "crasher" "1" "1"
               (hasheq 'test (λ (p) (error "boom")))))
  ;; Should not raise, should treat as pass
  (define result (dispatch-hooks 'test "payload" reg))
  (check-eq? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "payload"))
