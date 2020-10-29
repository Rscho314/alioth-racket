#lang racket/base

(require ;racket/place
         (only-in racket/fixnum
                  fxvector-set!
                  fxvector-ref
                  make-fxvector)
         (only-in racket/list flatten)
         ;racket/require
         ;(for-syntax racket/base)
         ;(filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name "")) racket/unsafe/ops)
         )

(define line-length 60)
(define LF (char->integer #\newline))
(define lookup-table (make-fxvector 128))
(define buffer-size (* 64 1024))

(for ([from (in-string "ACGTUMRWSYKVHDBN")]
      [to   (in-string "TGCAAKYWSRMBDHVN")])
  (let ([to (char->integer to)])
    (fxvector-set! lookup-table (char->integer from) to)
    (fxvector-set! lookup-table (char->integer (char-downcase from)) to)))

(define (reverse-complement fasta-bytes lookup-table)
  (define output-size (bytes-length fasta-bytes))
  (define seq-bytes (make-bytes output-size))
  (letrec
      ([sequences (lambda (i c)
                    (define start (car i))
                    (define end (cadr i))
                    (define write-offset 0)
                    (define current-char 0)
                    (for ([n (in-range start end)])
                      (define write-index (- n write-offset))
                      (define src-byte (bytes-ref fasta-bytes (- end (- n start) 1)))
                      (cond
                        [(= LF src-byte) (begin (set! write-offset (+ write-offset 1)))]
                        [(= line-length current-char)
                         (begin (bytes-set! seq-bytes write-index LF)
                                (bytes-set! seq-bytes (+ write-index 1) (fxvector-ref lookup-table src-byte))
                                (set! write-offset (- write-offset 1))
                                (set! current-char 1))]
                        [(= write-index (- end 2)) (begin
                                                     (bytes-set! seq-bytes write-index (fxvector-ref lookup-table src-byte))
                                                     (bytes-set! seq-bytes (+ 1 write-index) LF))]
                        [(not (= LF src-byte)) (begin
                                                 (bytes-set! seq-bytes write-index (fxvector-ref lookup-table src-byte))
                                                 (set! current-char (+ current-char 1)))]))
                    (heads (cdr i) (+ 1 c)))]
       [heads (lambda (i c)
                (cond
                  [(null? (cdr i)) seq-bytes]
                  [else (begin
                          (bytes-copy! seq-bytes (car i) fasta-bytes (car i) (cadr i))
                          (sequences (cdr i) (+ 1 c)))]))])
    (sequences (flatten (list 0 (regexp-match-positions* #rx">[^\n]+\n" fasta-bytes) output-size)) 0)))

;(define (main)
  (let loop ([input-buffer (read-bytes buffer-size)])
             (if (eof-object? input-buffer)
                 (void)
                 (begin (write-bytes (reverse-complement input-buffer lookup-table))
                        (loop (read-bytes buffer-size)))))
;)
;(require racket/port)
;(with-input-from-file "revcomp-input.txt" (lambda () (main)))