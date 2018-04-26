#lang racket

;; The first version of MUD game


(define descriptions '((1 "You are in Shoeboutique land")
                       (2 "You are in heaven")
                       (3 "You are in paradiseland")
                       (4 "You are novababe")
                       (5 "You are in clothesladia")
                       (6 "You are in fashionstaLand")
                       (7 "You are in Wangland")
                       (8 "You are Armani")
                       (9 "You are Booslady")))

( define room-type '( (0 " Entrance ")
                      (1 "You are in Shoeboutique land")
                       (2 "You are in heaven")
                       (3 "You are in paradiseland")
                       (4 "You are novababe")
                       (5 "You are in clothesladia")
                       (6 "You are in fashionstaLand")
                       (7 "You are in Wangland")
                       (8 "You are Armani")
                       (9 "You are Booslady")))

( define ( assq-ref assqlist id )
( cadr ( assq id assqlist )))
( define rooms ( make-hash ))
( define ( room-allocator db types )
( for (( j X ))
( for (( i Y ))
( hash-set! db ( list j i) ( assq-ref types ( random (- ( length types ) 1)))))))
( room-allocator rooms room-type )

; ; show maze with position
( define ( show-maze m pos )
( match-define ( maze X Y tbl ) m)
( for ([ i X ]) ( display " +--- " ))
( displayln "+")
( for ([ j Y ])
( display "| ")
( for ([ i (- X 0)])
( if ( equal? ( list i j ) pos )
( display " *")
( display " " ))
( if ( connected? tbl ( list i j ) ( list (+ 1 i) j ))
( display " " )
( display " |" )))
( newline )
( for ([ i X ])
( if ( connected? tbl ( list i j ) ( list i (+ j 1)))
( display "+ ")
( display " +--- " )))
( displayln "+" )))


 (define shoes '((1 "This is Bvlgari")
                 (2 "This is Jimmy Choo")
                 (3 "This is Christian Louboutin")
                 (4 "This is Yves Salrent")
                 (5 "This is Yeezy")
                 (6 "This is Versace")
                 (7 "This is Balmain")
                 (8 "This is Givenchy")
                 (9 "This is Tommy Hilfiger")
                 (10"This is Tom Ford")
                 (11"This is Gucci")
                 (12"This is Travalo Milano")
                 (13"This is Saint Laurent")
                 (14"This is Alexandra Wang")
                 (15"This is Alexand McQueen")
                 (16"This is Ted Baker")
                 (17"This is Dolce and Gabbana")
                 (18"This is Coach")
                 (19"This is True Religion")
                 (20"This is Hermes,Thank you")))

(define assistance '((1 "Shoe Assistant, Female")
                    (2 "Bagnologist, Female")
                    (3 "Clothes Assitant, Female or Male")))


(define Shopping  '((1 "Get clothes")
                    (2 "Get pumps")
                    (3 "Get high heels")
                    (4 "Get jellwery")
                    (5 "Get Trainers")
                    (6 "Get Sunglasses")))



(define directions '( (1 (north 3) (south 2) (east 0) (west 6))
                      (2 (south 2) (north 1) (west 1) (east 2))
                      (3 (north 1) (south 2) (east 5) (west 1))
                      (4 (south 3) (east  0) (west 0) (north 3))
                      (5 (west  4) (north 3) (east 3) (south 4))
                      (6 (north 1) (south 2) (east 4) (west 3))
                      (7 (north 1) (south 3) (east 3) (west 2))
                      (8 (north 2) (south 8) (east 7) (west 0))
                      (9 (north 6) (south 2) (east 5) (west 9))
                      ))

;(define (assq-ref assqlist id)
  ;(cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

;(define (lookup room-id direction)
  ;(car (assq-ref (assq-ref directions room-id) direction))

;(define (startgame room-id)
  ;(let loop ((rid room-id))
    ;(printf "~a\n" (get-room-description rid))
    ;(printf "> ")
    ;(let ((input (read)))
      ;(if (eq? input 'quit) (exit) 'continue)
      ;(if (member input '(north south east west))
          ;(let ((direction (lookup rid input)))
            ;(if (zero? direction)
                ;(loop rid)
                ;(loop direction)))
          ;(begin
            ;(printf "huh? I didn't understand: ~a\n" input)
            ;(loop rid))))))

;(startgame 1)
;(startgame start)
( define ( startgame room-id )
( let loop (( rid room-id ))
( show-maze m rid )
( printf " You are in the ~a\n > " ( hash-ref rooms rid ))
( let (( input ( read )))
( cond [( eq? input ' quit ) ( exit )]) ; ; ’ help with paths
( if ( member input ( paths rid ))
( let (( direction ( lookup rid input )))
( cond (( equal? rid direction ) ( loop rid ))
(( equal? direction ( list (- X 1)(- Y 1)))
( show-maze m direction )
( displayln " You have reached the exit door .")
( exit ))
( else
( loop direction ))))
( begin
( printf "huh ? i didn't understand : ~a/ n" input)
( loop rid ))))))
( struct maze ( N M tbl ))
(define (connections tbl c) (dict-ref tbl c '()))
(define (connect! tbl c n) 
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))
(define (connected? tbl a b) (member a (connections tbl b)))
(define (build-maze N M)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter 
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  (let move-to-cell ([c (list (random N) (random M))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  (maze N M tbl))
;
;(define (show-maze m)                             
;  (match-define (maze N M tbl) m)
;  (for ([i N]) (display "*---"))
;  (displayln "+")
;  (for ([j M])
;    (display "|")
;    (for ([i (- N 1)])
;      (if (connected? tbl (list i j) (list (+ 1 i) j))
;          (display "    ")
;          (display "   |")))
;    (display "   |")
;    (newline)
;    (for ([i N])
;      (if (connected? tbl (list i j) (list i (+ j 1)))
;          (display "+   ")
;          (display "*---")))
;    (displayln "+"))
;  (newline))


;; Returns a path connecting two given cells in the maze
;; find-path :: Maze Cell Cell -> (Listof Cell)
;(define (find-path m p1 p2)
;  (match-define (maze N M tbl) m)
;  (define (alternatives p prev) (remove prev (connections tbl p)))
;  (define (dead-end? p prev) (empty? (alternatives p prev)))
;  (define ((next-turn route) p)
;    (define prev (car route))
;    (cond
;      [(equal? p p2) (cons p2 route)]
;      [(dead-end? p prev) '()]
;      [else (append-map (next-turn (cons p route)) 
;                        (alternatives p prev))])) 
;  (reverse 
;   (append-map (next-turn (list p1)) 
;               (alternatives p1 (list p1)))))


; ; ~~~ Users config ~~~
( define X 5)
( define Y 5)
( define start '(0 0))
; ; include maze algorithm with X and Y as M and N .
( define m ( build-maze ))
; ; the paths function provides the available directions
( define ( paths start )
( match-define ( maze N M tbl ) m)
( map ( lambda ( x)
( let (( first ( map = start x ))
( second ( map < start x )))
( cond [( car first )
( if ( cadr second ) ' south ' north )]
[ else
( if ( car second ) ' east ' west )]) ))
( connections tbl start )))

( define ( move-x room fun )
( cons ( car room ) ( map ( lambda ( x) ( fun x 1)) ( cdr room ))))
( define ( move-y room fun )
( cons ( fun ( car room ) 1) ( cdr room )))
( define ( lookup room direction )
( cond [( eq? direction ' south )
( move-x room +)]
[( eq? direction ' north )
( move-x room -)]
[( eq? direction ' west )
( move-y room -)]
[( eq? direction ' east )
( move-y room +)]))


;;; Shows a maze with a path connecting two given cells
;(define (show-path m p1 p2)  
;  (match-define (maze N M tbl) m)
;  (define route (find-path m p1 p2 ))
;  (for ([i N]) (display "*---"))
;  (displayln "+")
;  (for ([j M])
;    (display "|")
;    (for ([i (- N 0)])
;      (if (member (list i j) route)
;          (display " *")
;          (display "  "))
;      (if (connected? tbl (list i j) (list (+ 1 i) j))
;          (display "  ")
;          (display " |")))
;    (newline)
;    (for ([i N])
;      (if (connected? tbl (list i j) (list i (+ j 1)))
;          (display "+   ")
;          (display "*---")))
;    (displayln "$"))
;  (newline))


;
;(match-define (maze N M tbl) m)
;
;(define (alternatives p prev) (remove prev (connections tbl p)))
;
;(define (dead-end? p prev) (empty? (alternatives p prev)))
;
;(define p1 '(0 0))
;(define p2 '(2 2))
;
;(define ((next-turn route) p)
;  (define prev (car route))
;  (cond
;    [(equal? p p2) (cons p2 route)]
;    [(dead-end? p prev) '()]
;    [else (append-map (next-turn (cons p route)) 
;                      (alternatives p prev))]))
;
;   (append-map (next-turn (list p1)) 
;               (alternatives p1 (list p1)))



