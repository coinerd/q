#lang racket/base

;; examples/sdk/07-tree.rkt — Branch, navigate, tree-info

(require "../../interfaces/sdk.rkt")

(define rt (create-agent-session #:provider (hasheq 'type 'test)))

;; Branch the session
(printf "Branch result: ~a~n" (q:session-branch rt))

;; Navigate
(printf "Navigate result: ~a~n" (q:session-navigate rt "some-entry-id"))

;; Get tree info
(printf "Tree info: ~a~n" (q:session-tree-info rt))
