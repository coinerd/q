#lang racket/base
(require file/sha1
         racket/list
         racket/path
         racket/string)
(provide audited
         audit-ok?)
;; Re-audit each test and its complete transitive local load graph, including
;; conditional loads, before updating fingerprints. Every reviewed source is
;; bound, so changing an import forces review of the graph again. Installed
;; Racket collections are outside this local-source review boundary. Reviewed
;; test code is trusted, not sandboxed; fingerprints are not a defense against
;; concurrent source changes or malicious code that forges result evidence.
(define audited
  '(("tests/test-exn-recovery.rkt" . "a28172b8cd1decaae46aeb936524d72d135a506a")
    ("tests/test-vdom-html-render.rkt" . "992256b166a3076999661e5993a3b91b84f422ea")
    ("tests/test-gsd-campaign-decision-pure.rkt" . "69e8eb7b0f50a2a3c9a65e8465c951c82c1401db")
    ("tests/test-gsd-delivery-verifier-decision.rkt" . "1364aec6b7c3da32cab64ffdf9b4713abb4a5781")))
;; Each string is the reviewed closure for the corresponding audited test.
(define closures
  (map
   (lambda (s) (map (lambda (line) (string-split line)) (string-split s "\n")))
   (list
    "util/exn.rkt 8b8633608e27f4c8ec0f85f59c0c132f6a3504ba"
    "tui/vdom.rkt cb1e298616970119883ad215cf80abaad6021241
tui/vdom-html-render.rkt 3cb146e542a81a8cf3f54b6b1b6009dd1d8ecdbd
tui/render/message-layout.rkt 1188690f3e6078a978e9d637a568504038099e02
tui/state-types.rkt 544e7d868697ecf1de13aaeca62abb773dcea414
tui/input.rkt 86d6c27ee2073023a56aa143870a9a77548f1f25
tui/char-width.rkt e45cbe25d5b26582659c3999a1a0f8a172b53e15
tui/theme.rkt 286746465b506cd69b03d5a9432130e71a7ff7b1
util/markdown.rkt d9b03b764b590efd2c8747f0dfebc9d16292b15b
extensions/custom-renderer-registry.rkt fe8bc784ff74a47f2f1c12dd7e1df17ae7224f99
ui-core/disclosure-state.rkt 6a29ade292b45bab55a23c9998f8873727f14016
util/cost-tracker.rkt b2da2505a68bf5e17d0df0e92a5eac3c5fe85348
util/string-helpers.rkt e9af4a51b38f016bd33dfa23cba714ea526cfde5
ui-core/conversation-reducer.rkt 6723cfda0192c8f4cb6986b0d3236c9cb8a0edb7
tui/arg-summary.rkt b21165be46128a8aec190bfaece9c9abecf6e9f3
tui/command-parse.rkt 360f8a61833240a97cc0c2d2f528e6ac7312e3a8
tui/input/state-types.rkt 28752972c19f4d3a52d4f1fbb8fdee8200dc4412
tui/input/editing-ops.rkt 787ab5bedfd3a5be4e86fb8ac64bb21b7201c116
tui/input/history-ops.rkt 30b7ac7823c234e75f8c9b602f65d933996c4403
tui/input/completion-ops.rkt 5f34b124de8b00460571768e643e1683f8e7c954
util/error/error-helpers.rkt 275e3065634449528e785f4afe89f0078178eeb9
ui-core/conversation-artifact.rkt 251df8a679bd836536af72c9aa923935506c8740
ui-core/feature-flags.rkt d275052ce8bebe000be8bdd9a75d2f8ffcb996e4
ui-core/composer-model.rkt e4487fa694874808416327455bf1ac862f16a988
ui-core/composer-layout.rkt b465ae2e05c8eadf20dc73c56f42dd6d0be6232c
tui/terminal.rkt ab2bb3fad916a81871e418c26f0a3105f1825d5f
util/telemetry.rkt ee5449482cf093956e938e4e5c928e3e7c508019
tui/clipboard.rkt 7670d0914e7e3d7b56b3fae226db0447ea4af7c9
tui/terminal-native.rkt 03698aff30216f37e1e8f2ea27a88ecf106e6496
tui/terminal-input.rkt 41cbd061e7a7ae4bfb35a0c32551240823d9f5c9
tui/input/kitty-protocol.rkt 408e4b8edb135b6ace10cec5ece7eaa777137bf2"
    "extensions/gsd/campaign-state.rkt b95d8699fa76cc5c27755061db88101ab0fa8cde
extensions/gsd/campaign-decision.rkt 59731aa4cf5561e3080bbe7cbbc4ba935dced74c
extensions/gsd/wave-docs.rkt 8877996c7b05298b8cb63202ef6b1ade2087973c
extensions/gsd/wave-status.rkt 73d371d5e89f4eaf683954b3b562d456eda738f7
extensions/gsd/planning-paths.rkt 51c7a0b2e97fd0746f2a0ba4250dd282e655f4a7
util/json/checksum.rkt 41062a49c5097b7e9fff9424b9e6f0ef8d66ed6c
extensions/gsd/plan-snapshot.rkt 0050d62d2a46ff5d0f16d50fc1d977f38d5870a7
util/version.rkt 5508916a46b1daab2ee38a3bf6e0ba561687b4f3
extensions/gsd/shared.rkt 39b4f3e9b456ebcd33fcbe026cc54afce495e7b1
extensions/gsd/planning-artifact-ownership.rkt ce1ffa3a030fbad25ad00ab8cfd6070ab3e11df8"
    "extensions/gsd/verify-command-format.rkt 431a68586833ff7e802cc9b92f6f85047820e87c
extensions/gsd/verify-command-grammar.rkt 71b5b87b7d7541ef8d78715605d486341b94b604")))

(define (link-free? path)
  (let loop ([parts (explode-path (path->complete-path path))]
             [prefix #f])
    (cond
      [(null? parts) #t]
      [(eq? (car parts) 'same) (loop (cdr parts) prefix)]
      [(eq? (car parts) 'up)
       (and prefix (loop (cdr parts) (simplify-path (build-path prefix 'up) #f)))]
      [else
       (define next
         (if prefix
             (build-path prefix (car parts))
             (car parts)))
       (and (not (link-exists? next)) (loop (cdr parts) next))])))

(define (audit-ok? root entry)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define index (index-of audited entry equal?))
    (and index
         (for/and ([item (in-list (cons (list (car entry) (cdr entry)) (list-ref closures index)))])
           (define relative (string->path (car item)))
           (and (relative-path? relative)
                (not (memq 'up (explode-path relative)))
                (let ([path (build-path root relative)])
                  (and (link-free? path)
                       (file-exists? path)
                       (equal? (cadr item) (call-with-input-file path sha1)))))))))
