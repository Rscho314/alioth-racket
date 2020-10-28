#lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;; contributed by Eli Barzilay

#;(require racket/future
         ;future-visualizer/trace
         ;future-visualizer
         )

;(start-future-tracing!)

(define translation (make-vector 128))

(for ([from (in-string "ACGTUMRWSYKVHDBN")]
      [to   (in-string "TGCAAKYWSRMBDHVN")])
  (let ([to (char->integer to)])
    (vector-set! translation (char->integer from) to)
    (vector-set! translation (char->integer (char-downcase from)) to)))

(define I (current-input-port))
;(define I (open-input-file "revcomp-input.txt"))
(define O (current-output-port))
;(define O (open-output-file "revcomp-output.txt" #:exists 'replace))

(define marker (char->integer #\>))

;(define line-length 60)
;(define buf-size (* 64 1024))
;(define out-buf (make-bytes (+ buf-size 1 (quotient buf-size line-length))))
(define LF (char->integer #\newline))

(define futures-hash (make-hasheq))

(define (read-fasta)
  (let loop ([line-pointer 0])
    (define line-bytes (read-bytes-line I))
    (cond
      [(eof-object? line-bytes) (void)]
      [(= marker (bytes-ref line-bytes 0))
       (begin
         (hash-set! futures-hash line-pointer line-bytes)
         (loop (add1 line-pointer)))]
      [else
       (begin
         (hash-set! futures-hash
                    line-pointer
                    (list->bytes
                     (reverse
                      (translate line-bytes))))
         (loop (add1 line-pointer)))])))

(define (translate sequence)
  (for/list
      ([nucleotide (in-bytes sequence)])
    (vector-ref translation nucleotide)))

(define (write-fasta)
  (for ([i (in-range 0 (hash-count futures-hash))])
    (let ([elmt (hash-iterate-value futures-hash i)])
      (begin (write-bytes elmt O) (write-byte LF O))))
  (write-byte LF O))

(read-fasta)
;(display futures-hash)
(write-fasta)

;(close-input-port I)
;(close-output-port O)

;(show-visualizer)