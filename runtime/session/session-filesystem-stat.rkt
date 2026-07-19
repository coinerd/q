#lang racket/base

;; runtime/session/session-filesystem-stat.rkt
;; Shared stat-info type and predicates for the session filesystem boundary.
;;
;; Both POSIX and Win32/NT backends, and the generic dispatch layer, use this
;; single stat-info struct so that artifact-stat returns a value usable by
;; backend-neutral predicates (artifact-regular? etc.).

(provide (struct-out stat-info)
         stat-regular?
         stat-dir?
         stat-link?
         stat-block?
         stat-char?
         stat-fifo?
         stat-socket?)

(struct stat-info (mode nlink uid gid ino size) #:transparent)

;; S_IFMT masks (portable across POSIX platforms; Win32 fills mode equivalently)
(define S_IFMT #o170000)
(define S_IFREG #o100000)
(define S_IFDIR #o040000)
(define S_IFLNK #o120000)
(define S_IFBLK #o060000)
(define S_IFCHR #o020000)
(define S_IFIFO #o010000)
(define S_IFSOCK #o140000)

(define (kind info mask)
  (and info (= (bitwise-and (stat-info-mode info) S_IFMT) mask)))

(define (stat-regular? info)
  (kind info S_IFREG))
(define (stat-dir? info)
  (kind info S_IFDIR))
(define (stat-link? info)
  (kind info S_IFLNK))
(define (stat-block? info)
  (kind info S_IFBLK))
(define (stat-char? info)
  (kind info S_IFCHR))
(define (stat-fifo? info)
  (kind info S_IFIFO))
(define (stat-socket? info)
  (kind info S_IFSOCK))
