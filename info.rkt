#lang info

;; pkg info
(define collection "custom-load")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "pict-doc"
                     "plot-doc"
                     "math-doc"))

;; collection info
(define name "custom-load")
(define scribblings '(["custom-load.scrbl" ()]))
