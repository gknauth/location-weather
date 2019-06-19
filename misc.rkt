#lang racket

(provide (all-defined-out))

(define (fmt-i-03d n)
  (let ((s (format "~a" n)))
    (cond [(= (string-length s) 2) (string-append "0" s)]
          [(= (string-length s) 1) (string-append "00" s)]
          [else s])))
  
(define (feet-hundreds ft)
  (fmt-i-03d (exact-round (/ ft 100.))))

