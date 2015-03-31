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
  (iprintf "~a(~a) ~a\n"
           who
           (length args)
           (string-join (map (lambda (x) (format "~s" x)) args))))

;; ----------------------------------------

;; use-zo?-table : hash[path => boolean]
(define use-zo?-table (make-hash))

;; use-zo? : (U symbol path (list 'submod ...)) -> boolean
(define (use-zo? mod)
  (cond [(symbol? mod) #t]
        [(pair? mod)
         (use-zo? (cadr mod))]
        [(path? mod)
         (let ([mod (simplify-path mod)])
           (hash-ref! use-zo?-table mod
                      (lambda ()
                        ;; Submodules can cause cycles in use-zo?, so tentatively
                        ;; put mod in table as ok to use zo?, then replace once checked.
                        (hash-set! use-zo?-table mod 'pending)
                        (file-use-zo? mod))))]))

;; file-use-zo? : path -> boolean
(define (file-use-zo? file)
  (define dir (path-only file))
  (define file-name (file-name-from-path file))
  (define zo-file (file->zo-file dir file-name))
  (define zo (and (file-exists? zo-file)
                  (with-handlers ([(lambda (e) #t) (lambda (e) #f)])
                    (parameterize ((read-accept-compiled #t))
                      (call-with-input-file zo-file (lambda (in) (read in)))))))
  (cond [(not (compiled-module-expression? zo))
         (iprintf "absent or garbage: ~s\n" zo-file)
         ;; zo file doesn't exist or contains garbage
         #f]
        [(> (file-or-directory-modify-seconds file)
            (file-or-directory-modify-seconds zo-file))
         ;; zo is stale
         (iprintf "stale ~s\n" file)
         #f]
        [else ;; zo is compiled-module-expression
         (for*/and ([phase+imps (in-list (module-compiled-imports zo))]
                    [imp (in-list (cdr phase+imps))])
           (use-zo? (resolve-module-path-index imp file)))]))

(define (file->zo-file dir file-name)
  ;; (use-compiled-file-paths)     : (listof relative-path?)
  ;; (current-compiled-file-roots) : (listof (U 'same path?))
  (define zo-file-name (path-add-suffix file-name ".zo"))
  (define zo-file (build-path dir "compiled" zo-file-name))
  zo-file)

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
                     (cond [(and (pair? name) (eq? (car name) #f))
                            ;; Never load from source; must trust bytecode
                            (iprintf "forced bytecode: ~s\n" file)
                            (old file name)]
                           [(use-zo? file)
                            (iprintf "zo ok: ~s\n" file)
                            (old file name)]
                           [else
                            ;; avoid bad zo; load file directly
                            (iprintf "zo bad: ~s\n" file)
                            (parameterize ((current-load-relative-directory (path-only file)))
                              ((current-load) file name))]))
                   out!))))
