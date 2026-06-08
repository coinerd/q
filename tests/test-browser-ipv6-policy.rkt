#lang racket

;; @speed fast
;; @suite default

;; tests/test-browser-ipv6-policy.rkt — F3: IPv6 policy classification

(require rackunit
         "../browser/policy.rkt"
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; classify-ipv6 unit tests
;; ---------------------------------------------------------------------------

(test-case "classify-ipv6: ::1 → loopback"
  (check-eq? (classify-ipv6 "::1") 'loopback))

(test-case "classify-ipv6: [::1] → loopback (bracket-stripped)"
  (check-eq? (classify-ipv6 "[::1]") 'loopback))

(test-case "classify-ipv6: 0:0:0:0:0:0:0:1 → loopback"
  (check-eq? (classify-ipv6 "0:0:0:0:0:0:0:1") 'loopback))

(test-case "classify-ipv6: fe80::1 → link-local"
  (check-eq? (classify-ipv6 "fe80::1") 'link-local))

(test-case "classify-ipv6: fe90::1 → link-local"
  (check-eq? (classify-ipv6 "fe90::1") 'link-local))

(test-case "classify-ipv6: fea0::1 → link-local"
  (check-eq? (classify-ipv6 "fea0::1") 'link-local))

(test-case "classify-ipv6: feb0::1 → link-local"
  (check-eq? (classify-ipv6 "feb0::1") 'link-local))

(test-case "classify-ipv6: fd00::1 → private (ULA)"
  (check-eq? (classify-ipv6 "fd00::1") 'private))

(test-case "classify-ipv6: fc00::1 → private (ULA)"
  (check-eq? (classify-ipv6 "fc00::1") 'private))

(test-case "classify-ipv6: ff00::1 → link-local (multicast)"
  (check-eq? (classify-ipv6 "ff00::1") 'link-local))

(test-case "classify-ipv6: ::ffff:127.0.0.1 → loopback (IPv4-mapped)"
  (check-eq? (classify-ipv6 "::ffff:127.0.0.1") 'loopback))

(test-case "classify-ipv6: ::ffff:169.254.169.254 → cloud-metadata (IPv4-mapped)"
  (check-eq? (classify-ipv6 "::ffff:169.254.169.254") 'cloud-metadata))

(test-case "classify-ipv6: ::ffff:10.0.0.1 → private (IPv4-mapped)"
  (check-eq? (classify-ipv6 "::ffff:10.0.0.1") 'private))

(test-case "classify-ipv6: 2001:db8::1 → public"
  (check-eq? (classify-ipv6 "2001:db8::1") 'public))

(test-case "classify-ipv6: ::abcd:ef01 → public"
  (check-eq? (classify-ipv6 "::abcd:ef01") 'public))

;; ---------------------------------------------------------------------------
;; classify-ip dispatches IPv6 via colon detection
;; ---------------------------------------------------------------------------

(test-case "classify-ip dispatches IPv6: ::1 → loopback"
  (check-eq? (classify-ip "::1") 'loopback))

(test-case "classify-ip dispatches IPv6: fe80::1 → link-local"
  (check-eq? (classify-ip "fe80::1") 'link-local))

;; ---------------------------------------------------------------------------
;; URL validation with IPv6 hosts
;; ---------------------------------------------------------------------------

(test-case "URL https://[::1]:3000/ blocked (wrong port)"
  (define settings (make-browser-policy-settings #:allowed-localhost-ports '(8080)))
  (define-values (ok? reason) (validate-browser-url "https://[::1]:3000/" settings))
  (check-false ok?)
  (check-not-false (string-contains? reason "localhost")))

(test-case "URL https://[::1]:8080/ allowed (allowed port)"
  (define settings (make-browser-policy-settings #:allowed-localhost-ports '(8080)))
  (define-values (ok? reason) (validate-browser-url "https://[::1]:8080/" settings))
  (check-true ok?))

(test-case "URL https://[::ffff:169.254.169.254]/metadata blocked (cloud metadata)"
  (define settings (make-browser-policy-settings))
  (define-values (ok? reason) (validate-browser-url "https://[::ffff:169.254.169.254]/metadata" settings))
  (check-false ok?))

(test-case "URL https://[fd00::1]/internal blocked (private ULA)"
  (define settings (make-browser-policy-settings))
  (define-values (ok? reason) (validate-browser-url "https://[fd00::1]/internal" settings))
  (check-false ok?))

(test-case "URL https://[2001:db8::1]/ allowed (public IPv6)"
  (define settings (make-browser-policy-settings))
  (define-values (ok? reason) (validate-browser-url "https://[2001:db8::1]/" settings))
  (check-true ok?))

(test-case "URL https://[::ffff:10.0.0.1]/ blocked (private IPv4-mapped)"
  (define settings (make-browser-policy-settings))
  (define-values (ok? reason) (validate-browser-url "https://[::ffff:10.0.0.1]/" settings))
  (check-false ok?))
