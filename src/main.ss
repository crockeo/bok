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
(include "src/state.ss")

(define PLAYER-CHARACTER #\@)

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
               (conc "Message: "
                     ((condition-property-accessor 'exn
                                                   'message
                                                   "no message")
                      exn)))

     (mvaddstr 3 2
               (conc "Arguments: "
                     ((condition-property-accessor 'exn
                                                   'arguments
                                                   "no arguments")
                      exn)))

     (mvaddstr 4 2
               (conc "Location: "
                     ((condition-property-accessor 'exn
                                                   'location
                                                   "no location")
                      exn)))

     (getch))
    (else))

  (k window))

(define (handle-sigint signal)
  (destroy-ncurses)
  (exit 0))

(define (render window state)
  (wclear window)
  (state 'render window)
  (curs_set 0)
  (wrefresh window))

(define (loop window state)
  (render window state)
  (let ([input (getch)])
    (unless (equal? input #\q)
      (loop window (state 'update input)))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (prepare-ncurses)

  (define window (stdscr))
  (define menu (make-menu-state '("Play"
                                  "Credits"
                                  "Exit") 0))

  (call/cc
   (lambda (k)
     (with-exception-handler
         (curry handle-exn-ncurses k window)
       (lambda () (loop window menu)))))

  (destroy-ncurses))

(main)
