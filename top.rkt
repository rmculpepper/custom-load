#lang racket/base
(require "private/loader.rkt")
(provide current-zo-blacklist
         zo-blacklist!)

;; ============================================================
;; Top-level convenience layer

(define current-zo-blacklist (make-parameter (lambda (mod) #f)))

(define (zo-blacklist! . specs)
  (current-zo-blacklist (blacklist->pred specs)))

(current-load/use-compiled
 (make-custom-load/use-compiled
  #:verbose? #t
  #:blacklist (lambda (mod) ((current-zo-blacklist) mod))))
