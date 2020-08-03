;; -*- geiser-scheme-implementation: 'chicken -*-

(import (chicken io))

(include "src/util.ss")

(define (load-multiline-text path)
  (call-with-input-file path
   (lambda (port)
     (let loop ([agg (list)]
                [line (read-line port)])
       (if (eof-object? line)
           agg
           (loop (append agg (list line))
                 (read-line port)))))))


(define (multiline-text-width multiline-text)
  (apply max
         (map
          (lambda (line)
            (string-length line))
          multiline-text)))

(define (multiline-text-render base-y base-x lines)
  (for (i 0 (length lines))
    (mvaddstr (+ base-y i) base-x (nth lines i))))
