;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  (chicken io)
  (chicken process-context)
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
             [#\d '(1 0)]
             [else '(0 0)]))))

(define (prepare-ncurses)
  (initscr)
  (cbreak))

(define (destroy-ncurses)
  (endwin))

(define (handle-exn-ncurses k window exn)
  (cond-expand
    (chicken-script
     (mvaddstr 0 0 "Encountered exception. Press any key to quit.")
     (mvaddstr 2 2
               (string-append "Message: " ((condition-property-accessor 'exn
                                                                        'message
                                                                        "no message")
                                           exn)))

     (mvaddstr 4 2
               (string-append "Location: " ((condition-property-accessor 'exn
                                                                         'location
                                                                         "no location")
                                            exn)))

     ;; TODO: figure out how to make beter error tracing

     (getch))
    (else))

  (k window))

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

  (define window (stdscr))

  (call/cc
   (lambda (k)
     (with-exception-handler
         (curry handle-exn-ncurses k window)
       (lambda () (loop window (make-player 0 0))))))

  (destroy-ncurses))

(main)
