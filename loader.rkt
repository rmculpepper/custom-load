#lang racket/base
(require racket/path
         racket/string
         syntax/modresolve)
(provide (all-defined-out))

;; TODO (maybe)
;; - if file is X.rkt, also check for X.ss
;; - check for .so, .dll, .dylib

;; ----------------------------------------

;; A ModName is (U #f Symbol (cons (U #f Symbol) (Listof Symbol)))
;; A ResolvedMod is (U Symbol Path (list* 'submod (U Symbol Path) (Listof Symbol)))

;; We assume the current load/use-compiled handler implements the same
;; behavior as the default handler.

;; A CLUC is an instance of custom-load/use-compiled:
(struct custom-load/use-compiled
  (cache        ;; hash[ Path => Boolean ]
   blacklist    ;; Path -> Boolean
   load-zo      ;; (Path ModName -> Any)  -- Note: given orig path, NOT zo path
   load-src     ;; (Path ModName -> Any)
   indent       ;; (U #f (box Nat))
   )
  #:property prop:procedure
  (lambda (self file name)
    (dynamic-wind void
                  (lambda ()
                    ((adjust-indent! self 1))
                    (cond [(use-zo? self file)
                           (iprintf self "using zo for ~s\n" file)
                           (let ([load-zo (custom-load/use-compiled-load-zo self)])
                             (load-zo file name))]
                          [(and (pair? name) (eq? (car name) #f))
                           ;; means don't load from source; since can't use bytecode,
                           ;; must not load at all (w/o complaint)
                           (iprintf self "forced to skip ~s\n" file)
                           (void)]
                          [else
                           (iprintf self "using source for ~s\n" file)
                           (parameterize ((current-load-relative-directory (path-only file)))
                             (let ([load-src (custom-load/use-compiled-load-src self)])
                               (load-src file name)))]))
                  (adjust-indent! self -1))))

(define (make-custom-load/use-compiled
         #:blacklist [blacklist (lambda (file) #f)]
         #:load-zo   [load-zo (current-load/use-compiled)]
         #:load-src  [load-src (lambda (file name) ((current-load) file name))]
         #:verbose?  [verbose? #f])
  (custom-load/use-compiled (make-hash)
                            (blacklist->pred blacklist)
                            load-zo load-src
                            (and verbose? (box 0))))

;; ----------------------------------------

;; adjust-indent! : CLUC Int -> (-> Void)
(define (adjust-indent! self adjust)
  (cond [(custom-load/use-compiled-indent self)
         => (lambda (b) (lambda () (set-box! b (+ (unbox b) adjust))))]
        [else void]))

;; iprintf : CLUC String Any ... -> Void
(define (iprintf self fmt . args)
  (cond [(custom-load/use-compiled-indent self)
         => (lambda (indent-b)
              (eprintf "~a" (make-string (unbox indent-b) #\-))
              (apply eprintf fmt args))]
        [else (void)]))

;; ----------------------------------------

(define (blacklist->pred spec)
  ;; FIXME: might be better to do all regexps together, avoid repeated path->string
  (cond [(procedure? spec)
         spec]
        [(regexp? spec)
         (lambda (mod) (regexp-match? spec (path->string mod)))]
        [(null? spec)
         (lambda (mod) #f)]
        [(list? spec)
         (let ([preds (map blacklist->pred spec)])
           (lambda (mod) (for/or ([pred (in-list preds)]) (pred mod))))]
        [else (error 'custom-load/use-compiled "bad blacklist: ~e" spec)]))

;; ----------------------------------------

;; use-zo? : CLUC ResolvedMod -> boolean
(define (use-zo? self mod)
  (cond [(symbol? mod) #t]
        [(pair? mod)
         (use-zo? self (cadr mod))]
        [(path-string? mod)
         (let ([mod (simplify-path mod)]
               [cache (custom-load/use-compiled-cache self)])
           (and
            (hash-ref! cache mod
                       (lambda ()
                         ;; Submodules can cause cycles in use-zo?, so tentatively
                         ;; put mod in table as ok to use zo?, then replace once checked.
                         (hash-set! cache mod 'pending)
                         (file-use-zo? self mod)))
            #t))]))

;; file-use-zo? : CLUC Path -> Boolean
(define (file-use-zo? self file)
  (cond [(blacklisted? self file)
         (iprintf self "blacklisted: ~s\n" file)
         #f]
        [else
         (file-use-zo?* self file)]))

;; blacklisted? : CLUC Path -> Boolean
(define (blacklisted? self file)
  ((custom-load/use-compiled-blacklist self) file))

;; file-use-zo?* : CLUC Path -> Boolean
(define (file-use-zo?* self file)
  (define dir (path-only file))
  (define file-name (file-name-from-path file))
  (define zo-file (find-zo-file dir file-name))
  (cond [(not zo-file)
         (iprintf self "no zo for ~s\n" file)
         #f]
        [(> (file-or-directory-modify-seconds file)
            (file-or-directory-modify-seconds zo-file))
         ;; zo is stale
         (iprintf self "stale zo: ~s\n" zo-file)
         #f]
        [(read-zo-module zo-file)
         => (lambda (zo)
              (for*/and ([phase+imps (in-list (module-compiled-imports zo))]
                         [imp (in-list (cdr phase+imps))])
                (use-zo? self (resolve-module-path-index imp file))))]
        [else
         (iprintf self "garbage zo: ~s\n" zo-file)
         #f]))

;; find-zo-file : Path Path -> (U Path #f)
;; Returns the zo file that would be used by the default load/use-compiled handler.
;; Note: doesn't do ".rkt" -> ".ss" mapping, and ignores native libs (.so/.dll/.dylib)
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

;; read-zo-module : Path -> (U #f Compiled-Module-Expression)
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

;; ============================================================
;; Top-level convenience layer

(define current-blacklist (make-parameter (lambda (mod) #f)))

(define (blacklist! . specs)
  (current-blacklist (blacklist->pred specs)))

(current-load/use-compiled
 (make-custom-load/use-compiled
  #:verbose? #t
  #:blacklist (lambda (mod) ((current-blacklist) mod))))
