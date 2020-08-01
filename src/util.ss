;; -*- geiser-scheme-implementation: 'chicken -*-

(import matchable)

(define (curry function . args)
  "Partially applies arguments to a function."
  (if (null? args)
      function
      (apply curry
             (lambda rest
               (apply function (car args) rest))
             (cdr args))))

(define (range min max)
  "Generates a range of numbers from [min, max). I.e. min inclusive to max exclusive."
  (let loop ([i min]
             [agg '()])
    (if (equal? i max)
        agg
        (loop (+ i 1)
              (append agg (list i))))))

(begin-for-syntax
 (define (string-join delimiter . strings)
   "Joins a series of strings with a given delimiter."
   (let loop [(strings (cdr strings))
              (str (car strings))]
     (if (null? strings)
         str
         (loop (cdr strings)
               (string-append str delimiter (car strings))))))

 (define (join-symbols . symbols)
   "When provided a list of symbols (provided as arguments), this function concatenates them together
into one, long symbol using the standard-multiword-symbol-form."
   (string->symbol
    (apply string-join "-"
           (map symbol->string symbols))))

 (define (nth list n)
   "Retrieves the nth item of a list."
   (cond
    [(null? list) list]
    [(equal? n 0) (car list)]
    [else (nth (cdr list) (- n 1))]))

 (define (nths list . ns)
   "Retrieves the nth item of an embedded list. Each provided index scopes into a list. E.g. given
(nths '((1 2 3) 4 5) 0 1) -> 2"
   (if (null? ns)
       list
       (apply nths (nth list (car ns)) (cdr ns))))

 (define (drop list n)
   "Drops the first n values of a list."
   (cond
    [(null? list) list]
    [(equal? n 0) list]
    [else (drop (cdr list) (- n 1))]))

 (define (get-function-names defines)
   "Given a collection of valid function definitions, this returns the (symbol) names of each of
those functions."
   (map
    (lambda (define) (nths define 1 0))
    defines))

 (define (flatten1 list-of-lists)
   "Flattens a single layer of a list-of-lists."
   (let loop [(list-of-lists list-of-lists)
              (agg (list))]
     (if (null? list-of-lists)
         agg
         (loop
          (cdr list-of-lists)
          (append agg (car list-of-lists)))))))

(define-syntax define-class
  ;; Defines a class.
  ;;
  ;; (define-class player (x y)
  ;;   (define (get-x) x)
  ;;   (define (get-y) y)
  ;;
  ;;   (define (translate dx dy)
  ;;     (make-player (+ x dx)
  ;;                  (+ y dy))))
  ;;
  ;; (let [(player (make-player 0 1))]
  ;;   (player 'get-x) => 0
  ;;   (player 'get-y) => 1
  ;;   ((player 'translate 1 1) 'get-y) => 2
  ;; )
  (er-macro-transformer
   (lambda (form rename compare?)
     (let [(name (nth form 1))
           (arguments (nth form 2))
           (body (drop form 3))
           (%join-symbols (rename join-symbols))
           (%flatten1 (rename flatten1))
           (%get-function-names (rename get-function-names))]
       `(define (,(%join-symbols 'make name) ,@arguments)
          ;; Automatically construct getters and setters for each argument
          ,@(%flatten1
             (map
              (lambda (argument)
                `((define (,(%join-symbols 'get argument))
                    ,argument)

                  (define (,(%join-symbols 'set argument) new-value)
                    (,(%join-symbols 'make name)
                     ,@(map
                        (lambda (sub-argument)
                          (if (equal? sub-argument argument)
                              'new-value
                              sub-argument))
                        arguments)))))
              arguments))

          ;; User-defined functions
          ,@body

          ;; Dispatcher that calls all of the other functions
          (define (self message . args)
            (apply
             (match message
               ,@(map (lambda (function-name)
                        `(',function-name ,function-name))
                      (append (get-function-names body)
                              (flatten1 (map (lambda (argument)
                                               (list (%join-symbols 'get argument)
                                                     (%join-symbols 'set argument)))
                                             arguments)))))
             args))

          self)))))
