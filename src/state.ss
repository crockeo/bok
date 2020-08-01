;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  matchable
  ncurses)

(include "src/util.ss")

(define-class menu-state (options current-index)
  (define (move-selection direction)
    (set-current-index
     (modulo (+ current-index direction)
             (length options))))

  (define (render window)
    (map
     (lambda (i)
       (mvaddstr i 0
                 (string-append
                  (if (equal? i current-index)
                      "> "
                      "  ")
                  (nth options i))))
     (range 0 (length options))))

  (define (update input)
    (match input
      [#\w (move-selection -1)]
      [#\s (move-selection 1)]
      [else self])))
