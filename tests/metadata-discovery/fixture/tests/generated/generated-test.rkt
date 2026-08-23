;; @suite unit
;; @speed fast
;; Fixture: file inside a generated/ (documented ignorable) directory. The
;; CURRENT discovery contract has NO ignore rule for "generated/" (only
;; /compiled/ and @not-test are excluded), so this file IS discovered today.
;; This pins the current contract; changing it is a deliberate contract
;; change, not a parity fix.
(module+ test
  (require rackunit)
  (check-true #t))
