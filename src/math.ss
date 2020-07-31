;; -*- geiser-scheme-implementation: 'chicken -*-

(define (clamp min-v max-v v)
  (min (max v min-v)
       max-v))

(define (vec2 x y)
  `(,x ,y))

(define (add-vec2 v1 v2)
  (match `(,v1 ,v2)
    [((x1 y1) (x2 y2)) `(,(+ x1 x2) ,(+ y1 y2))]))
