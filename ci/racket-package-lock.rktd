;; ci/racket-package-lock.rktd - version-indexed Racket package lock.
;;
;; Schema revision 2 (v1.00.11 W1): one reviewed entry per supported
;; runtime. Each entry binds its racket-version to the reviewed package
;; identities (catalog checksums) resolved for that runtime. Entries are
;; deliberately separate per runtime so one runtime's reviewed graph can
;; never satisfy another runtime's verification (invariants I1-I3).
;;
;;   8.10 - the reviewed v1.00.10 graph, carried over unchanged from the
;;          schema-revision-1 lock (checksums taken via git show HEAD).
;;   8.11 - the identical reviewed sources; catalog checksums are content
;;          hashes of the package archives, so the same reviewed set
;;          resolves under Racket 8.11.
;;
;; Consumed by ci/verify-racket-package-lock.rkt, which selects exactly
;; one entry for the requested/running runtime version and rejects every
;; other runtime (no fallback, no cross-entry prefix match).
#hash((schema-revision . 2)
      (runtimes . #hash(
                         ("8.10" . #hash((racket-version . "8.10")
                                        (packages . #hash(("gui-easy-lib" . "662038709d6c39d2f2ea94116485d29a56292800")
                                                           ("box-extra-lib" . "63ef1bde87fac8856b42b38e0697de8cf3f588a1")
                                                           ("reprovide-lang-lib" . "f38e629f9713d2bc2691538b2ce5784bb1187252")
                                                           ("pretty-expressive" . "27e7be8016b38252a19f3620bc37539100b02503")
                                                           ("quickcheck" . "0b6902d8d79973ec959f99e56ccba038beca8fe9")
                                                           ("syntax-macro-lang" . "d20497348015aecb309bdddd29cebea4a0b35664")
                                                           ("pretty-expressive-lib" . "27e7be8016b38252a19f3620bc37539100b02503")
                                                           ("doc-coverage" . "b1c0e9f3fd3a25e260f8905e6c8211dacf532b25")
                                                           ("lang-file-lib" . "69993f73dab8382796be37998ec47ded7883faf7")
                                                           ("fmt" . "4e1ed68e596e656960b44a8244bb33eb4e65ec64")
                                                           ("version-case-lib" . "0e21bf92773196424ce2b5fc1aefeb61e1ffafb7")))))
                         ("8.11" . #hash((racket-version . "8.11")
                                        (packages . #hash(("gui-easy-lib" . "662038709d6c39d2f2ea94116485d29a56292800")
                                                           ("box-extra-lib" . "63ef1bde87fac8856b42b38e0697de8cf3f588a1")
                                                           ("reprovide-lang-lib" . "f38e629f9713d2bc2691538b2ce5784bb1187252")
                                                           ("pretty-expressive" . "27e7be8016b38252a19f3620bc37539100b02503")
                                                           ("quickcheck" . "0b6902d8d79973ec959f99e56ccba038beca8fe9")
                                                           ("syntax-macro-lang" . "d20497348015aecb309bdddd29cebea4a0b35664")
                                                           ("pretty-expressive-lib" . "27e7be8016b38252a19f3620bc37539100b02503")
                                                           ("doc-coverage" . "b1c0e9f3fd3a25e260f8905e6c8211dacf532b25")
                                                           ("lang-file-lib" . "69993f73dab8382796be37998ec47ded7883faf7")
                                                           ("fmt" . "4e1ed68e596e656960b44a8244bb33eb4e65ec64")
                                                           ("version-case-lib" . "0e21bf92773196424ce2b5fc1aefeb61e1ffafb7"))))))))
