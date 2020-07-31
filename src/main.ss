;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  (chicken io)
  (chicken process signal)

  matchable
  ncurses

  srfi-69)

(include "src/util.ss")

(define (add-vec2 v1 v2)
  "Adds two 2-dim lists together."
  (match `(,v1 ,v2)
    [((x1 y1) (x2 y2)) `(,(+ x1 x2) ,(+ y1 y2))]))

(define (prepare-ncurses)
  (initscr)
  (cbreak))

(define (destroy-ncurses)
  (endwin))

(define (handle-sigint signal)
  (destroy-ncurses)
  (exit 0))

(define (new-game-state)
  (let ([game-state (make-hash-table)])
    (hash-table-set! game-state 'x 0)
    (hash-table-set! game-state 'y 0)
    game-state))

(define (game-state-move game-state axis direction)
  (hash-table-update! game-state
                   axis
                   (lambda (value) (max (+ value direction) 0))))

(define (input-direction input)
  "Maps an input, in the form of WASD, to a game action, in the form of translation."
  (match input
    [#\w '(0 -1)]
    [#\s '(0 1)]
    [#\a '(-1 0)]
    [#\d '(1 0)]
    [else '(0 0)]))

(define (game-state-get-pos game-state)
  "Get the current position according to the game state."
  (map (curry hash-table-ref game-state)
       '(x y)))

(define (game-state-set-pos game-state xy)
  "Set the current position according to the game state."
  (match xy
    [(x y)
     (map (curry apply hash-table-set! game-state)
          `((x ,x)
            (y ,y)))]))

(define (game-state-update game-state input)
  (game-state-set-pos game-state
   (add-vec2 (game-state-get-pos game-state)
             (input-direction input))))

(let ([game-state (new-game-state)])
  (game-state-update game-state #\d)
  (game-state-get-pos game-state))

(define (print-sorry window)
  (wclear window)
  (mvaddstr 0 0 "sorry, an error occurred")
  (wrefresh window)
  (getch))

(define (render window game-state)
  (wclear window)
  (mvwaddch window
            (hash-table-ref game-state 'y)
            (hash-table-ref game-state 'x)
            #\#)
  (curs_set 0)
  (wrefresh window))

(define (loop window game-state)
  (render window game-state)
  (let ([input (getch)])
    (unless (equal? input #\q)
      ;; TODO: do bounds checking to make sure that we're not accidentally writing off of the
      ;;       screen
      (game-state-update game-state input)
      (loop window game-state))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (prepare-ncurses)

  (let ([window (stdscr)])
   (condition-case (loop window (new-game-state))
     [(exn) (print-sorry window)]
     [var () #f]))

  (destroy-ncurses))

(main)
