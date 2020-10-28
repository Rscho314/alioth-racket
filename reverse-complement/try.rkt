#lang racket
(require racket/future
         future-visualizer/trace
         future-visualizer)
 
(start-future-tracing!)
(let ([f (future (lambda () (+ 1 2 3)))])
  (+ 4 5 6)
  (touch f))
(stop-future-tracing!)
(show-visualizer)