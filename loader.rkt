#lang racket/base
(require racket/path
         racket/string
         syntax/modresolve)
(provide (all-defined-out))

(define indent 0)
(define (in!) (set! indent (+ indent 1)))
(define (out!) (set! indent (- indent 1)))
(define (iprintf fmt . args)
  (eprintf "~a" (make-string indent #\-))
  (apply eprintf fmt args))
(define (tprintf who args)
  (iprintf "~a: ~a\n"
           who
           (string-join (map (lambda (x) (format "~s" x)) args))))

;; ----------------------------------------

;; Never use zo for any mod s.t. ((current-blacklist?) mod) is true.
(define current-blacklist? (make-parameter (lambda (mod) #f)))

(define (blacklist . specs)
  (current-blacklist?
   (let ([preds (map blacklist-spec->pred specs)])
     (lambda (mod)
       (for/or ([pred preds]) (pred mod))))))

(define (blacklist-spec->pred spec)
  (cond [(procedure? spec)
         spec]
        [(regexp? spec)
         (lambda (mod) (regexp-match? spec (path->string mod)))]
        [else (error 'blacklist "bad blacklist spec: ~e" spec)]))

;; ----------------------------------------

;; use-zo?-table : hash[path => boolean]
(define use-zo?-table (make-hash))

;; use-zo? : (U symbol path (list 'submod ...)) -> boolean
(define (use-zo? mod)
  (cond [(symbol? mod) #t]
        [(pair? mod)
         (use-zo? (cadr mod))]
        [(path-string? mod)
         (let ([mod (simplify-path mod)])
           (and
            (hash-ref! use-zo?-table mod
                       (lambda ()
                         ;; Submodules can cause cycles in use-zo?, so tentatively
                         ;; put mod in table as ok to use zo?, then replace once checked.
                         (hash-set! use-zo?-table mod 'pending)
                         (file-use-zo? mod)))
            #t))]))

;; file-use-zo? : path -> boolean
(define (file-use-zo? file)
  (cond [((current-blacklist?) file)
         (iprintf "blacklisted: ~s\n" file)
         #f]
        [else
         (file-use-zo?* file)]))

(define (file-use-zo?* file)
  (define dir (path-only file))
  (define file-name (file-name-from-path file))
  (define zo-file (find-zo-file dir file-name))
  (cond [(not zo-file)
         (iprintf "absent zo\n")
         #f]
        [(> (file-or-directory-modify-seconds file)
            (file-or-directory-modify-seconds zo-file))
         ;; zo is stale
         (iprintf "stale zo: ~s\n" zo-file)
         #f]
        [(read-zo-module zo-file)
         => (lambda (zo)
              ;; (iprintf "checking dependencies: ~s\n" file)
              (for*/and ([phase+imps (in-list (module-compiled-imports zo))]
                         [imp (in-list (cdr phase+imps))])
                (use-zo? (resolve-module-path-index imp file))))]
        [else
         (iprintf "garbage zo: ~s\n" zo-file)
         #f]))

(define (find-zo-file dir file-name)
  (define zo-file-name (path-add-suffix file-name ".zo"))
  (for/or ([root (in-list (current-compiled-file-roots))])
    ;; root : (U 'same path?)
    (define dir*
      (cond [(and (path? root) (absolute-path? root)) (reroot-path dir root)]
            [else (build-path dir root)]))
    (for/or ([suffix (in-list (use-compiled-file-paths))])
      ;; suffix : relative-path?
      (define zo-file (build-path dir* suffix zo-file-name))
      (and (file-exists? zo-file) zo-file))))

(define (read-zo-module zo-file)
  (parameterize ((read-accept-compiled #t))
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (call-with-input-file* zo-file
        (lambda (in)
          (define zo (read in))
          (define more (read in))
          (and (compiled-module-expression? zo)
               (eof-object? more)
               zo))))))

;; ----------------------------------------

;; We assume the current load/use-compiled handler implements the same
;; behavior as the default handler.

;; Not implemented (yet?):
;; - if file is X.rkt, also check for X.ss
;; - check for .so, .dll, .dylib

(current-load
 (let ([old (current-load)])
   (lambda args
     (dynamic-wind void
                   (lambda ()
                     (tprintf "load" args)
                     (in!)
                     (apply old args))
                   out!))))

(current-load/use-compiled
 (let ([old (current-load/use-compiled)])
   (lambda (file name)
     (dynamic-wind void
                   (lambda ()
                     (tprintf "load/uc" (list file name))
                     (in!)
                     (cond [(use-zo? file)
                            (iprintf "using zo for ~s\n" file)
                            (old file name)]
                           [(and (pair? name) (eq? (car name) #f))
                            ;; means don't load from source; since can't use bytecode,
                            ;; must not load at all (w/o complaint)
                            (iprintf "forced to skip ~s\n" file)
                            (void)]
                           [else
                            (iprintf "using source for ~s\n" file)
                            (parameterize ((current-load-relative-directory (path-only file)))
                              ((current-load) file name))]))
                   out!))))
