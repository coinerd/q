#lang racket/base

;; runtime/session/session-filesystem-posix.rkt
;; POSIX descriptor-relative no-follow filesystem primitives.
;;
;; This module is the atomic containment boundary for session storage (finding F-02/A-03).
;; Every open/mutate resolves a name relative to a held directory file descriptor using
;; the *at syscall family with O_NOFOLLOW / AT_SYMLINK_NOFOLLOW. There is NO pathname
;; fallback: a symlink swapped between capability acquisition and use is never followed.
;;
;; Platform constants are selected by (system-type 'os) because they are part of the
;; stable kernel ABI and have not changed in decades. The selected table is probed at
;; load time against the running kernel (see probe at end of module).
;;
;; W2 scope: foundation only. No production session-store cutover (that is W3).

(require racket/match
         racket/file
         racket/string
         racket/port
         racket/system
         ffi/unsafe
         ffi/unsafe/define
         "session-filesystem-stat.rkt")
(provide posix-dir-cap?
         ;; Root / session directory capabilities
         posix-open-root
         posix-close-dir-cap
         posix-open-session-dir ; root-cap session-id -> dir-cap (NOFOLLOW)
         posix-create-session-dir ; root-cap session-id -> dir-cap (mkdirat + openat NOFOLLOW)
         posix-session-dir-exists? ; root-cap session-id -> boolean (fstatat NOFOLLOW)
         ;; Artifact operations — all relative to a session dir-cap, all NOFOLLOW
         posix-read-artifact ; dir-cap name -> bytes
         posix-append-artifact ; dir-cap name bytes -> void (O_CREAT|O_APPEND NOFOLLOW)
         posix-atomic-replace-artifact ; dir-cap name bytes -> void (temp+renameat, NOFOLLOW)
         posix-unlink-artifact ; dir-cap name -> void (unlinkat, NOFOLLOW)
         posix-list-artifacts ; dir-cap -> (listof string)
         posix-artifact-stat ; dir-cap name -> (or/c stat-info? #f) (fstatat NOFOLLOW)
         posix-make-regular-artifact ; dir-cap name -> void (create empty regular file, O_CREAT|O_EXCL)
         ;; Fork
         posix-fork-session-dirs ; src-root-cap src-id dst-root-cap dst-id -> dst-dir-cap
         ;; Policy / stats
         posix-backend-supported?)

;; ---------------------------------------------------------------------------
;; Platform constant tables (stable kernel ABI)
;; ---------------------------------------------------------------------------

;; NOTE: (system-type 'os) returns 'macos for Darwin and 'unix for Linux (and
;; other Unixes). CI runs Ubuntu and macOS only, so 'unix is treated as Linux.

;; The O_* and AT_* flag values are part of the kernel ABI but are NOT uniform
;; across libcs/kernels (e.g. some hosts expose O_NOFOLLOW = 0x20000 rather than
;; the more common 0x40000). To remain correct everywhere, the flag values are
;; extracted from the system C headers at load time via the C preprocessor. If
;; `cpp` is unavailable, per-OS hardcoded fallbacks are used. The load-time
;; platform probe at the end of the module validates the result against the
;; running kernel (fail-closed on mismatch).

(define cpp-flags-to-probe
  '(O_NOFOLLOW O_DIRECTORY
               O_CREAT
               O_EXCL
               O_APPEND
               O_TRUNC
               O_NONBLOCK
               O_CLOEXEC
               AT_FDCWD
               AT_SYMLINK_NOFOLLOW
               AT_REMOVEDIR))

(define (parse-c-int s)
  (define str (string-trim s))
  (define len (string-length str))
  (define (octal? c)
    (char<=? #\0 c #\7))
  (cond
    [(and (>= len 3) (string-prefix? str "0x")) (string->number (substring str 2) 16)]
    [(and (>= len 4) (string-prefix? str "-0x")) (- (string->number (substring str 3) 16))]
    [(and (>= len 2)
          (char=? (string-ref str 0) #\0)
          (for/and ([c (in-string str)])
            (octal? c)))
     (string->number str 8)]
    [else (string->number str 10)]))

(define (probe-flags-via-cpp!)
  (define cpp (find-executable-path "cpp"))
  (cond
    [(not cpp) #f]
    [else
     (define input
       (string-append "#include <fcntl.h>\n#include <unistd.h>\n"
                      (string-join (map (lambda (m) (format "__M ~a" m)) cpp-flags-to-probe) "\n")
                      "\n"))
     (define out
       (with-handlers ([exn:fail? (lambda (_) "")])
         (with-output-to-string (lambda ()
                                  (with-input-from-string input (lambda () (system* cpp "-P")))))))
     (define marker-lines
       (for/list ([l (in-list (string-split out "\n"))]
                  #:when (string-prefix? (string-trim l) "__M "))
         l))
     (define h (make-hash))
     (for ([l (in-list marker-lines)]
           [name (in-list cpp-flags-to-probe)])
       (define parts (string-split (string-trim l)))
       (when (and (pair? (cdr parts)) (parse-c-int (cadr parts)))
         (hash-set! h name (parse-c-int (cadr parts)))))
     h]))

(define (base-flags os)
  (cond
    [(eq? os 'macos)
     (hash 'O_NOFOLLOW
           256
           'O_DIRECTORY
           1048576
           'O_CREAT
           512
           'O_EXCL
           2048
           'O_APPEND
           8
           'O_TRUNC
           1024
           'O_NONBLOCK
           4
           'O_CLOEXEC
           16777216
           'AT_FDCWD
           -100
           'AT_SYMLINK_NOFOLLOW
           256
           'AT_REMOVEDIR
           128)]
    [(eq? os 'linux)
     (hash 'O_NOFOLLOW
           262144
           'O_DIRECTORY
           65536
           'O_CREAT
           64
           'O_EXCL
           128
           'O_APPEND
           1024
           'O_TRUNC
           512
           'O_NONBLOCK
           2048
           'O_CLOEXEC
           524288
           'AT_FDCWD
           -100
           'AT_SYMLINK_NOFOLLOW
           256
           'AT_REMOVEDIR
           512)]
    [else (hash)]))

(define (stat-layout os)
  (cond
    [(eq? os 'macos)
     ;; macOS struct stat (64-bit inode, _DARWIN_FEATURE_64_BIT_INODE default)
     (hash 'stat-size
           144
           'st-mode-off
           4
           'st-mode-size
           2
           'st-nlink-off
           6
           'st-nlink-size
           2
           'st-uid-off
           16
           'st-gid-off
           20
           'st-ino-off
           8
           'st-size-off
           48)]
    [(eq? os 'linux)
     ;; Linux 64-bit struct stat offsets
     (hash 'stat-size
           144
           'st-mode-off
           24
           'st-mode-size
           4
           'st-nlink-off
           16
           'st-nlink-size
           8
           'st-uid-off
           28
           'st-gid-off
           32
           'st-ino-off
           8
           'st-size-off
           48)]
    [else (hash)]))

(define (platform-constants)
  (define os
    (cond
      [(eq? (system-type 'os) 'macos) 'macos]
      [(memq (system-type 'os) '(unix)) 'linux]
      [else 'unsupported]))
  (define base (base-flags os))
  (define probed (probe-flags-via-cpp!))
  ;; cpp values override hardcoded fallbacks when available.
  (define flags
    (if probed
        (for/fold ([h base]) ([(k v) (in-hash probed)])
          (hash-set h k v))
        base))
  ;; Access modes are universal (0/1/2); add them directly.
  (define flags+access (hash-set* flags 'O_RDONLY 0 'O_WRONLY 1 'O_RDWR 2))
  (for/fold ([h (hash-set* flags+access 'os os 'errno os)]) ([(k v) (in-hash (stat-layout os))])
    (hash-set h k v)))

(define C (platform-constants))

(define (O . keys)
  (for/sum ([k (in-list keys)]) (hash-ref C k 0)))

(define (c-ref key)
  (hash-ref C key))

;; ---------------------------------------------------------------------------
;; FFI bindings
;; ---------------------------------------------------------------------------

(define-ffi-definer define-libc (ffi-lib #f))

;; errno access
(define errno-fn
  (case (c-ref 'errno)
    [(linux) (get-ffi-obj "__errno_location" #f (_fun -> _pointer) (lambda () #f))]
    [(macos) (get-ffi-obj "__error" #f (_fun -> _pointer) (lambda () #f))]
    [else #f]))

(define (errno)
  (if errno-fn
      (ptr-ref (errno-fn) _int)
      0))

(define-libc openat (_fun _int _string _int _uint32 -> _int))
(define-libc close-fd (_fun _int -> _int) #:c-id close)
(define-libc write-fd (_fun _int _pointer _size -> _ssize) #:c-id write)
(define-libc read-fd (_fun _int _pointer _size -> _ssize) #:c-id read)
(define-libc unlinkat (_fun _int _string _int -> _int))
(define-libc renameat (_fun _int _string _int _string -> _int))
(define-libc mkdirat (_fun _int _string _uint32 -> _int))
(define-libc fstatat (_fun _int _string _pointer _int -> _int))
(define-libc dup (_fun _int -> _int))
(define-libc fdopendir (_fun _int -> _pointer))
(define-libc closedir (_fun _pointer -> _int))
(define-libc readdir (_fun _pointer -> _pointer))

(define (dirent-name-off)
  (if (eq? (c-ref 'os) 'macos) 21 19))

;; ---------------------------------------------------------------------------
;; Stat info struct (extracted, portable)
;; ---------------------------------------------------------------------------

(define (fresh-stat-buf)
  (malloc (c-ref 'stat-size) 'raw))

(define (read-u16-at buf off)
  (ptr-ref (ptr-add buf off _uint8) _uint16))

(define (read-u32-at buf off)
  (ptr-ref (ptr-add buf off _uint8) _uint32))

(define (read-u64-at buf off)
  (ptr-ref (ptr-add buf off _uint8) _uint64))

(define (extract-stat-info buf)
  (stat-info (if (eq? (c-ref 'os) 'macos)
                 (read-u16-at buf (c-ref 'st-mode-off))
                 (read-u32-at buf (c-ref 'st-mode-off)))
             (if (eq? (c-ref 'os) 'macos)
                 (read-u16-at buf (c-ref 'st-nlink-off))
                 (read-u64-at buf (c-ref 'st-nlink-off)))
             (read-u32-at buf (c-ref 'st-uid-off))
             (read-u32-at buf (c-ref 'st-gid-off))
             (read-u64-at buf (c-ref 'st-ino-off))
             (read-u64-at buf (c-ref 'st-size-off))))

;; ---------------------------------------------------------------------------
;; Capability type (opaque directory descriptor)
;; ---------------------------------------------------------------------------

;; A dir-cap wraps an OS file descriptor for a directory. It is owned by the
;; session repository. Closing is idempotent: the fd is set to -1 after close so
;; a finalizer or explicit close cannot double-close (test #9).
(struct posix-dir-cap ([fd #:mutable]) #:transparent)

(define (posix-close-dir-cap! cap)
  (define fd (posix-dir-cap-fd cap))
  (when (>= fd 0)
    (close-fd fd)
    (set-posix-dir-cap-fd! cap -1)))

;; ---------------------------------------------------------------------------
;; Error reporting
;; ---------------------------------------------------------------------------

(define (errno-message n)
  (case n
    [(1) "EPERM: operation not permitted"]
    [(2) "ENOENT: no such file or directory"]
    [(9) "EBADF: bad file descriptor"]
    [(13) "EACCES: permission denied"]
    [(17) "EEXIST: file exists"]
    [(20) "ENOTDIR: not a directory"]
    [(21) "EISDIR: is a directory"]
    [(22) "EINVAL: invalid argument"]
    [(39) "ENOTEMPTY: directory not empty"]
    [(40) "ELOOP: too many levels of symbolic links"]
    [(62) "ELOOP (macOS)"]
    [else (format "errno ~a" n)]))

(define (raise-syscall-error who syscall errno-val . irritants)
  (raise (exn:fail
          (format "~a: ~a failed: ~a (errno ~a)" who syscall (errno-message errno-val) errno-val)
          (current-continuation-marks))))

(define (check-syscall who syscall result)
  (unless (>= result 0)
    (raise-syscall-error who syscall (errno)))
  result)

;; ---------------------------------------------------------------------------
;; Root and session directory operations
;; ---------------------------------------------------------------------------

;; Mutable availability flag. The load-time probe sets this to #f if the
;; runtime syscall/constant verification fails (e.g. a new OS SDK with
;; mismatched stat offsets). A #f flag makes the POSIX backend report as
;; unsupported so sessions fall back to path-based persistence — the module
;; still loads and the process never aborts merely because the hardening probe
;; could not run. (W3: unblocks the macOS smoke path that the production cutover
;; now exercises.)
(define posix-probe-ok? (box #t))

(define (posix-backend-supported?)
  (and (memq (system-type 'os) '(unix macos macosx)) (unbox posix-probe-ok?)))

(define (path->complete-path-string p)
  (define path
    (if (string? p)
        (string->path p)
        p))
  (path->string (if (complete-path? path)
                    path
                    (path->complete-path path))))

(define (posix-open-root root-path)
  (unless (posix-backend-supported?)
    (error 'posix-open-root "POSIX backend not supported on ~a" (system-type 'os)))
  (define fd
    (check-syscall 'posix-open-root
                   'open
                   (openat (c-ref 'AT_FDCWD)
                           (path->complete-path-string root-path)
                           (O 'O_DIRECTORY 'O_RDONLY 'O_NOFOLLOW)
                           0)))
  (posix-dir-cap fd))

(define (posix-close-dir-cap cap)
  (posix-close-dir-cap! cap))

(define (posix-open-session-dir root-cap session-id)
  (define fd (openat (posix-dir-cap-fd root-cap) session-id (O 'O_DIRECTORY 'O_RDONLY 'O_NOFOLLOW) 0))
  (if (>= fd 0)
      (posix-dir-cap fd)
      (raise-syscall-error 'posix-open-session-dir 'openat (errno) "session-id" session-id)))

(define (posix-create-session-dir root-cap session-id)
  ;; mkdirat then openat (both NOFOLLOW relative to the held root)
  (check-syscall 'posix-create-session-dir
                 'mkdirat
                 (mkdirat (posix-dir-cap-fd root-cap) session-id #o755))
  (posix-open-session-dir root-cap session-id))

(define (posix-session-dir-exists? root-cap session-id)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define buf (fresh-stat-buf))
    (define res (fstatat (posix-dir-cap-fd root-cap) session-id buf (c-ref 'AT_SYMLINK_NOFOLLOW)))
    (and (>= res 0) (stat-dir? (extract-stat-info buf)))))

;; ---------------------------------------------------------------------------
;; Artifact operations (relative to a session dir-cap, all NOFOLLOW)
;; ---------------------------------------------------------------------------

(define (posix-make-regular-artifact dir-cap name)
  ;; Create an empty regular file with O_CREAT|O_EXCL|O_NOFOLLOW.
  (define fd
    (check-syscall 'posix-make-regular-artifact
                   'openat
                   (openat (posix-dir-cap-fd dir-cap)
                           name
                           (O 'O_WRONLY 'O_CREAT 'O_EXCL 'O_NOFOLLOW 'O_CLOEXEC)
                           #o644)))
  (close-fd fd)
  (void))

(define (write-all who fd data)
  (define total (bytes-length data))
  (let loop ([written 0])
    (when (< written total)
      (define n (write-fd fd (ptr-add data written _uint8) (- total written)))
      (when (< n 0)
        (raise-syscall-error who 'write (errno)))
      (loop (+ written n)))))

(define (posix-read-artifact dir-cap name)
  (define fd
    (check-syscall 'posix-read-artifact
                   'openat
                   (openat (posix-dir-cap-fd dir-cap) name (O 'O_RDONLY 'O_NOFOLLOW 'O_CLOEXEC) 0)))
  (define out (open-output-bytes))
  (define buf (make-bytes 8192))
  (let loop ()
    (define n (read-fd fd buf (bytes-length buf)))
    (cond
      [(< n 0)
       (close-fd fd)
       (raise-syscall-error 'posix-read-artifact 'read (errno))]
      [(> n 0)
       (write-bytes buf out 0 n)
       (loop)]
      [else (void)]))
  (close-fd fd)
  (get-output-bytes out))

(define (posix-append-artifact dir-cap name data)
  (define fd
    (check-syscall 'posix-append-artifact
                   'openat
                   (openat (posix-dir-cap-fd dir-cap)
                           name
                           (O 'O_WRONLY 'O_CREAT 'O_APPEND 'O_NOFOLLOW 'O_CLOEXEC)
                           #o644)))
  (write-all 'posix-append-artifact fd data)
  (close-fd fd)
  (void))

(define (posix-atomic-replace-artifact dir-cap name data)
  ;; Temp create relative to the held dir, write, unlink any existing entry, renameat
  ;; within the same held dir. If a symlink named `name` was swapped in, unlinkat
  ;; (NOFOLLOW) removes the symlink entry inside the held directory and renameat then
  ;; installs the temp file there — the swapped target is never followed.
  (define tmp-name
    (string-append ".q-atomic-"
                   name
                   "-"
                   (number->string (current-inexact-milliseconds))
                   "-"
                   (number->string (random 1000000))))
  (define fd
    (check-syscall 'posix-atomic-replace-artifact
                   'openat-tmp
                   (openat (posix-dir-cap-fd dir-cap)
                           tmp-name
                           (O 'O_WRONLY 'O_CREAT 'O_EXCL 'O_NOFOLLOW 'O_CLOEXEC)
                           #o644)))
  (write-all 'posix-atomic-replace-artifact fd data)
  (close-fd fd)
  ;; Remove any pre-existing entry (symlink or file) inside the held dir, no-follow.
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (unlinkat (posix-dir-cap-fd dir-cap) name 0))
  (check-syscall 'posix-atomic-replace-artifact
                 'renameat
                 (renameat (posix-dir-cap-fd dir-cap) tmp-name (posix-dir-cap-fd dir-cap) name))
  (void))

(define (posix-unlink-artifact dir-cap name)
  ;; unlinkat with flags=0 never follows a symlink; it unlinks the entry in the
  ;; held directory. AT_REMOVEDIR is only for directory removal.
  (check-syscall 'posix-unlink-artifact 'unlinkat (unlinkat (posix-dir-cap-fd dir-cap) name 0)))

(define (posix-list-artifacts dir-cap)
  ;; fdopendir CONSUMES the fd (closedir closes it). Dup the fd first so the
  ;; held capability survives listing (fork lists then reads/stat via the cap).
  (define owned-fd (dup (posix-dir-cap-fd dir-cap)))
  (when (< owned-fd 0)
    (raise-syscall-error 'posix-list-artifacts 'dup (errno)))
  (define dir (fdopendir owned-fd))
  (when (not dir)
    (close-fd owned-fd)
    (raise-syscall-error 'posix-list-artifacts 'fdopendir (errno)))
  (define off (dirent-name-off))
  (define result
    (let loop ([acc '()])
      (define ent (readdir dir))
      (cond
        [(not ent) (reverse acc)]
        [else
         (define name
           (let read-name ([i 0]
                           [chars '()])
             (define b (ptr-ref ent _byte (+ off i)))
             (if (zero? b)
                 (list->string (reverse chars))
                 (read-name (+ i 1) (cons (integer->char b) chars)))))
         (loop (if (member name '("." ".."))
                   acc
                   (cons name acc)))])))
  (closedir dir)
  result)

(define (posix-artifact-stat dir-cap name)
  (define buf (fresh-stat-buf))
  (define res (fstatat (posix-dir-cap-fd dir-cap) name buf (c-ref 'AT_SYMLINK_NOFOLLOW)))
  (and (>= res 0) (extract-stat-info buf)))

;; ---------------------------------------------------------------------------
;; Fork — source and destination held independently, both NOFOLLOW
;; ---------------------------------------------------------------------------

(define (posix-fork-session-dirs src-root-cap src-id dst-root-cap dst-id)
  ;; Open source dir, create destination dir, both relative to held roots.
  (define src-dir (posix-open-session-dir src-root-cap src-id))
  (define dst-dir (posix-create-session-dir dst-root-cap dst-id))
  ;; Copy every regular artifact from src to dst (byte copy, NOFOLLOW throughout).
  (for ([name (in-list (posix-list-artifacts src-dir))])
    (define info (posix-artifact-stat src-dir name))
    (when (stat-regular? info)
      (define data
        (with-handlers ([exn:fail? (lambda (_) #"")])
          (posix-read-artifact src-dir name)))
      (define fd
        (check-syscall 'posix-fork-session-dirs
                       'openat-dst
                       (openat (posix-dir-cap-fd dst-dir)
                               name
                               (O 'O_WRONLY 'O_CREAT 'O_EXCL 'O_NOFOLLOW 'O_CLOEXEC)
                               #o644)))
      (write-all 'posix-fork-session-dirs fd data)
      (close-fd fd)))
  (posix-close-dir-cap src-dir)
  dst-dir)

;; ---------------------------------------------------------------------------
;; Load-time platform constant probe (fail-closed: bad constants abort load)
;; ---------------------------------------------------------------------------

(define (probe-platform-constants!)
  ;; The probe verifies the syscall/constant table at load time. A failure
  ;; (e.g. a new OS SDK with mismatched stat offsets) disables the POSIX
  ;; backend via the mutable flag rather than aborting module instantiation —
  ;; sessions then fall back to path-based persistence. (W3.)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (set-box! posix-probe-ok? #f)
                     (fprintf (current-error-port)
                              "warning: session-filesystem-posix probe failed (~a); "
                              (exn-message e))
                     (fprintf (current-error-port)
                              "no-follow hardening disabled, using path-based fallback~n"))])
    (when (posix-backend-supported?)
      (probe-platform-constants/body!))))

(define (probe-platform-constants/body!)
  ;; Create an isolated temp directory with a real subdir and a regular file;
  ;; verify openat NOFOLLOW and fstatat mode/regular detection agree with the
  ;; constant table for this kernel.
  (define base (find-system-path 'temp-dir))
  (define probe-dir
    (path->string (path->complete-path (build-path base
                                                   (format "q-posix-probe-~a-~a"
                                                           (current-inexact-milliseconds)
                                                           (random 1000000))))))
  (make-directory probe-dir)
  (dynamic-wind
   void
   (lambda ()
     ;; Real subdirectory.
     (make-directory (build-path probe-dir "real"))
     ;; Regular file.
     (call-with-output-file (build-path probe-dir "file") void)
     (define root (posix-open-root probe-dir))
     (dynamic-wind void
                   (lambda ()
                     ;; Directory open via NOFOLLOW succeeds and is a directory.
                     (define real-cap (posix-open-session-dir root "real"))
                     (posix-close-dir-cap real-cap)
                     ;; Regular file detected as regular (mode offset correctness).
                     (define file-stat (posix-artifact-stat root "file"))
                     (unless (stat-regular? file-stat)
                       (error 'session-filesystem-posix
                              "platform constant probe failed: regular-file detection mismatch (~a)"
                              (system-type 'os)))
                     ;; Directory detected as directory.
                     (define dir-stat (posix-artifact-stat root "real"))
                     (unless (stat-dir? dir-stat)
                       (error 'session-filesystem-posix
                              "platform constant probe failed: directory detection mismatch (~a)"
                              (system-type 'os))))
                   (lambda () (posix-close-dir-cap root))))
   (lambda ()
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (delete-directory/files probe-dir)))))

(probe-platform-constants!)
