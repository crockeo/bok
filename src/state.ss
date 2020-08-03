;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  matchable
  ncurses)

(include "src/util.ss")


(define-class menu-item (text on-select))


(define-class menu-state (options current-index)
  (define (move-selection direction)
    (set-current-index
     (modulo (+ current-index direction)
             (length options))))

  (define (render window)
    (let*-values ([(max-y max-x) (getmaxyx window)]
                  [(base-y) (- (quotient max-y 2) (quotient (length options) 2))]
                  [(base-x) (quotient max-x 2)])

      (for (i 0 (length options))
        (let* ([option (nth options i)]
               [s (string-append
                   (if (equal? i current-index)
                       "> "
                       "  ")
                   (option 'get-text)
                   (if (equal? i current-index)
                       " <"
                       "  "))])
          (mvaddstr (+ base-y i)
                    (- base-x (quotient (string-length s) 2))
                    s)))))

  (define (update input)
    (cond
     ((is-key input #\w KEY_UP) (move-selection -1))
     ((is-key input #\s KEY_DOWN) (move-selection 1))
     ;; 10 here denotes the ASCII code of the enter key
     ((is-key input 10) (((nth options current-index) 'get-on-select)))
     (else self))))


(define-class player (x y)
  (define (input-to-direction input)
    (cond
     [(is-key input #\w KEY_UP) '(0 -1)]
     [(is-key input #\s KEY_DOWN) '(0 1)]
     [(is-key input #\a KEY_LEFT) '(-1 0)]
     [(is-key input #\d KEY_RIGHT) '(1 0)]
     [else '(0 0)]))

  (define (translate dx dy)
    (make-player (+ x dx)
                 (+ y dy)))

  (define (render window)
    (mvwaddch window y x #\@))

  (define (update input)
    (apply translate (input-to-direction input))))


(define-class game-state (player)
  (define (render window)
    (player 'render window))

  (define (update input)
    (make-game-state (player 'update input))))
