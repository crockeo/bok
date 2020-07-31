(import
  (chicken io)
  (chicken process signal)

  ncurses)

(define (prepare-ncurses)
  (initscr)
  (cbreak))

(define (destroy-ncurses)
  (endwin))

(define (handle-sigint signal)
  (destroy-ncurses)
  (exit 0))

(define (render window game-state)
  (wclear window)
  (mvwaddch window 5 5 game-state)
  (wrefresh window))

(define (loop window)
  (let ([input (getch)])
    (unless (equal? input #\q)
      (render window input)
      (loop window))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (prepare-ncurses)
  (loop (stdscr))
  (destroy-ncurses))

(main)
