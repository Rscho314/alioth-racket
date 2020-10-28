#lang racket/base

;;; TODO:
;;; Add newlines (line length 60)
;;; parallelize

(require racket/place
         (only-in racket/fixnum
                  fxvector-set!
                  fxvector-ref
                  make-fxvector)
         racket/list
         racket/bytes
         racket/port
         ;optimization-coach
         ;racket/performance-hint
         ;racket/require
         ;(for-syntax racket/base)
         #;(filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         #;(rename-in racket/unsafe/ops
                    [unsafe-fx+ +]
                    [unsafe-fx+ -]
                    [unsafe-fx* *]
                    [unsafe-fxquotient quotient]
                    [unsafe-char->integer char->integer]
                    [unsafe-fxvector-set! fxvector-set!]
                    [unsafe-fxvector-ref fxvector-ref]
                    [unsafe-vector-set! vector-set!]
                    [unsafe-bytes-length bytes-length]))

(define lookup-table (make-fxvector 128 46))
(define buffer-size (* 64 1024))

(for ([from (in-string "ACGTUMRWSYKVHDBN")]
      [to   (in-string "TGCAAKYWSRMBDHVN")])
  (let ([to (char->integer to)])
    (fxvector-set! lookup-table (char->integer from) to)
    (fxvector-set! lookup-table (char->integer (char-downcase from)) to)))

(define (reverse-complement fasta-bytes lookup-table)
  (define output-size (bytes-length fasta-bytes))
  (define indices (flatten (list 0 (regexp-match-positions* #rx">[^\n]+\n" fasta-bytes) output-size)))
  (define seq-bytes (make-bytes output-size 95))
  (letrec
      ([sequences (lambda (i c)
                    (begin
                      ;; INDICES APPEAR OK, WORK ON BYTE PERMUTATION!
                      (define start (car i))
                      (define end (cadr i))
                      (for ([n (in-range (- end start))])
                        (define dec-index (- end n 1))
                        (define inc-index (+ n start))
                        (define dec-byte (bytes-ref fasta-bytes dec-index))
                        (cond
                          #;[(= 10 dec-byte) (begin
                                             (bytes-set! seq-bytes dec-index 10)
                                             #;(bytes-set! seq-bytes inc-index 33))]
                          [else (bytes-set! seq-bytes
                                    inc-index
                                    (fxvector-ref lookup-table dec-byte))]))
                      (heads (cdr i) (+ 1 c))))]
       [heads (lambda (i c)
                (cond
                  [(null? (cdr i)) seq-bytes]
                  [else (begin
                          (bytes-copy! seq-bytes (car i) fasta-bytes (car i) (cadr i))
                          (sequences (cdr i) (+ 1 c)))]))])
    (sequences indices 0)))

  (let loop ([input-buffer (read-bytes buffer-size)])
             (if (eof-object? input-buffer)
                 (void)
                 (begin (write-bytes (reverse-complement input-buffer lookup-table))
                        (loop (read-bytes buffer-size)))))


;(with-input-from-file "revcomp-input.txt" (lambda () (write-bytes (port->bytes))))