;; -*- geiser-scheme-implementation: 'chicken -*-

(define (curry function . args)
  "Partially applies arguments to a function."
  (if (null? args)
      function
      (apply curry
             (lambda rest
               (apply function (car args) rest))
             (cdr args))))
