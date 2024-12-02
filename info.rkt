;; Copyright 2015 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang info

;; pkg info
(define collection "custom-load")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define license '(Apache-2.0 OR MIT))

;; collection info
(define name "custom-load")
(define scribblings '(["custom-load.scrbl" ()]))
(define compile-omit-paths '("test"))
