#lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; *reset*
;;; improved by Phil Nguyen:
;;; - use `cons` instead of struct `node`
;;; - remove the confirmed unneccessary field `val`
;;; - accumulate part of `check`
;;; - use unsafe accessors and fixnum arithmetics
;;; - clean up with `define` instead of nested `let`
;;; - clean up with `for/sum` instead of `for/fold`

(require racket/cmdline
         racket/future
         future-visualizer
         future-visualizer/trace
         ;racket/performance-hint
         )

(define node cons)
(require (rename-in racket/unsafe/ops
                    [unsafe-car node-left]
                    [unsafe-cdr node-right]
                    [unsafe-fx+ +]
                    [unsafe-fx- -]
                    [unsafe-fx= =]))

(define (make d)
    (if (= d 0)
      (node #f #f)
      (let ([d2 (- d 1)])
        (node (make d2) (make d2)))))

(define (check t)
  (let sum ([t t] [acc 0])
    (if (node-left t)
        (sum (node-right t) (sum (node-left t) (+ 1 acc)))
        (+ 1 acc))))

(define (main n)
  (define min-depth 4)
  (define max-depth (max (+ min-depth 2) n))
  (define stretch-depth (+ max-depth 1))
  (printf "stretch tree of depth ~a\t check: ~a\n" stretch-depth (check (make stretch-depth)))
  (define long-lived-tree (make max-depth))
  (start-future-tracing!)
  (for
      ([f (in-list
           (for/list
               ([d (in-range 4 (+ 1 max-depth) 2)])
                (define iterations (unsafe-fxlshift 1 (+ (- max-depth d) min-depth)))
             (future (lambda ()
                       (format "~a\t trees of depth ~a\t check: ~a\n"
                               iterations
                               d
                               (for/sum ([_ (in-range iterations)])
                                 (check (make d))))))))])
    (display (touch f)))
  
  #;(define result-vector (make-vector (unsafe-fxquotient (- (+ max-depth 1) 4) 2)))
  #;(let outer-loop ([d 4])
    (cond
      [(= d (+ 1 max-depth)) (for ([f (in-vector result-vector)]) (display (touch f)))]
      [else (begin
              (define iterations (unsafe-fxlshift 1 (+ (- max-depth d) min-depth)))
               (vector-set!
                result-vector
                (unsafe-fxquotient (- d 4) 2)
                (future
                 (lambda ()
                   (let inner-loop ([i iterations] [inner-acc 0])
                     (if (= 1 i)
                         (format "~a\t trees of depth ~a\t check: ~a\n" iterations d (+ (check (make d)) inner-acc))
                         (inner-loop (- i 1) (+ (check (make d)) inner-acc)))))))
                (outer-loop (+ 2 d)))]))
  
  (stop-future-tracing!)
  (printf "long lived tree of depth ~a\t check: ~a\n" max-depth (check long-lived-tree)))

#;(command-line #:args (n) 
              (main (string->number n)))
(main 15)
(show-visualizer)
