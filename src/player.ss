;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  (chicken pretty-print)

  matchable)

(define (join-symbols . symbols)
  (string->symbol
   (let loop [(symbols (cdr symbols))
              (str (symbol->string (car symbols)))]
     (if (null? symbols)
         str
         (loop (cdr symbols)
               (string-append str "-" (symbol->string (car symbols))))))))

(define (nth list n)
  (cond
   [(null? list) list]
   [(equal? n 0) (car list)]
   [else (nth (cdr list) (- n 1))]))

(define (nths list . ns)
  (if (null? ns)
      list
      (apply nths (nth list (car ns)) (cdr ns))))

(define (drop list n)
  (cond
   [(null? list) list]
   [(equal? n 0) list]
   [else (drop (cdr list) (- n 1))]))


(define (get-function-names defines)
  (map
   (lambda (define) (nths define 1 0))
   defines))

(define-syntax define-class
  (er-macro-transformer
   (lambda (form rename compare?)
     (let [(name (nth form 1))
           (arguments (nth form 2))
           (body (drop form 3))
           (%join-symbols (rename join-symbols))
           (%get-function-names (rename get-function-names))]
       `(define (,(%join-symbols 'make name) ,@arguments)
          ,@body

          (define (self message . args)
            (apply
             (match message
               ,@(map (lambda (function-name)
                       `(',function-name ,function-name))
                     (get-function-names body)))
             args))

          self)))))

(define-class player (x y)
  (define (get-x) x)
  (define (get-y) y)

  (define (get-xy) (list x y))

  (define (move dx dy)
    (make-player (+ x dx)
                 (+ y dy))))
