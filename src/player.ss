;; -*- geiser-scheme-implementation: 'chicken -*-

(import matchable)

(define (make-player x y)
  (define (get-x) x)
  (define (get-y) y)
  (define (get-xy) `(,x ,y))

  (define (translate dx dy)
    (make-player (+ x dx)
                 (+ y dy)))

  (define (translate-vec2 dvec2)
    (match dvec2
      ((dx dy) (translate dx dy))))

  (define (self message . args)
    (let ([fn (match message
                ['get-x get-x]
                ['get-y get-y]
                ['translate translate]
                ['translate-vec2 translate-vec2])])
      (apply fn args)))

  self)

(let ([player (make-player 0 0)])
  (player 'translate 1 1))
