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


(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

(define (lookup room-id direction)
  (car (assq-ref (assq-ref directions room-id) direction)))

(define (startgame room-id)
  (let loop ((rid room-id))
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    (let ((input (read)))
      (if (eq? input 'quit) (exit) 'continue)
      (if (member input '(north south east west))
          (let ((direction (lookup rid input)))
            (if (zero? direction)
                (loop rid)
                (loop direction)))
          (begin
            (printf "huh? I didn't understand: ~a\n" input)
            (loop rid))))))

(startgame 1)

