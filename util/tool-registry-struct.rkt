#lang racket/base
;; DEPRECATED: This module has moved to util/tool/tool-registry-struct.rkt. Remove this facade in v0.83.
(provide (all-from-out "tool/tool-registry-struct.rkt"))
(require "tool/tool-registry-struct.rkt")
(module* internal #f
  (require (submod "tool/tool-registry-struct.rkt" internal))
  (provide (all-from-out (submod "tool/tool-registry-struct.rkt" internal))))
