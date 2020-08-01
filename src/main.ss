;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  (chicken io)
  (chicken process signal)
  (chicken string)

  matchable
  ncurses

  srfi-12
  srfi-69)

(include "src/math.ss")
(include "src/util.ss")

(define PLAYER-CHARACTER #\@)

(define-class player (x y)
  (define (repr)
    "Returns a useful representation of the player, used to debug."
    (list x y))

  (define (render window)
    "Renders the player onto the screen."
    (mvwaddch window y x PLAYER-CHARACTER))

  (define (translate dx dy)
    "Translates the player by the provided input."
    (make-player (+ x dx)
                 (+ y dy)))

  (define (translate-from-input input)
    "Translates a player from the provided input, rather than from a dx/dy pair."
    (apply translate
           (match input
             [#\w '(0 -1)]
             [#\s '(0 1)]
             [#\a '(-1 0)]
             [#\d '(1 0)]))))

(define (prepare-ncurses)
  (initscr)
  (cbreak))

(define (destroy-ncurses)
  (endwin))

(define (handle-exn-ncurses window exn)
  (wclear window)

  (mvaddstr 0 0 "Encountered exception. Press any key to quit.")
  (mvaddstr 2 2
            (string-append "Message: " ((condition-property-accessor 'exn 'message) exn)))
  (mvaddstr 3 2
            (string-append "Arguments: " ))

  (wrefresh window)
  (getch)
  (destroy-ncurses)
  (exit 0))

(define (handle-sigint signal)
  (destroy-ncurses)
  (exit 0))

(define (render window player)
  (wclear window)
  (player 'render window)
  (curs_set 0)
  (wrefresh window))

(define (loop window player)
  (render window player)
  (let ([input (getch)])
    (unless (equal? input #\q)
      (loop window (player 'translate-from-input input)))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (prepare-ncurses)

  (let ([window (stdscr)])
    (with-exception-handler
        (curry handle-exn-ncurses window)
      (lambda ()
        (loop (stdscr) (make-plyer 0 0)))))

  (destroy-ncurses))

(main)
