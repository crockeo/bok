;; -*- geiser-scheme-implementation: chicken -*-

(import
  (chicken io)
  (chicken process signal)

  ncurses

  srfi-69)

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

(define (update-game-state game-state input)
  (cond
   [(equal? input #\w) (game-state-move game-state 'x -1)]
   [(equal? input #\s) (game-state-move game-state 'x 1)]
   [(equal? input #\a) (game-state-move game-state 'y -1)]
   [(equal? input #\d) (game-state-move game-state 'y 1)]))

(define (render window game-state)
  (wclear window)
  (mvwaddch window
            (hash-table-ref game-state 'x)
            (hash-table-ref game-state 'y)
            #\q)
  (wrefresh window))

(define (loop window game-state)
  (render window game-state)
  (let ([input (getch)])
    (unless (equal? input #\q)
      ;; TODO: do bounds checking to make sure that we're not accidentally writing off of the
      ;;       screen
      (update-game-state game-state input)
      (loop window game-state))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (prepare-ncurses)
  (loop (stdscr) (new-game-state))
  (destroy-ncurses))

(main)
