;; -*- geiser-scheme-implementation: 'chicken -*-

(import
  (chicken io)
  (chicken port)
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
  (cbreak)
  (noecho)
  (timeout 300)

  (let ([window (stdscr)])
    (keypad window #t)
    window))

(define (destroy-ncurses)
  (endwin))

(define (handle-exn-ncurses k window exn)
  (cond-expand
    (chicken-script
     (timeout -1)

     (let ([exn-description (with-output-to-string
                              (lambda ()
                                ;; TODO:
                                ;;   write-exception isn't a real thing here, need to extract it out
                                ;;   of the Geiser source code
                               (write-exception exn)))])
       (mvaddstr 2 2 exn-description))

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
  (loop window (state 'update (getch))))

(define (main)
  (set-signal-handler! signal/int handle-sigint)

  (define window (prepare-ncurses))
  (define menu
    (make-menu-state
     (load-multiline-text "res/bok.txt")
     (list
      (make-menu-item
       "Play"
       (lambda ()
         (make-game-state (make-player 0 0))))
      (make-menu-item
       "Exit"
       (lambda ()
         (destroy-ncurses)
         (exit 0))))
     0))

  (call/cc
   (lambda (k)
     (with-exception-handler
         (curry handle-exn-ncurses k window)
       (lambda () (loop window menu)))))

  (destroy-ncurses))

;; TODO: Put exception handler closer to when we start the program

(main)
