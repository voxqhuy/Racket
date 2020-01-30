#lang plait

(print-only-errors #t)

(define-type Room
  (lab
   [room-num : String]
   [capacity : Number]
   [stations : Number])
  (classroom
   [room-num : String]
   [capacity : Number]
   [projector? : Boolean])
  (bathroom
   [room-num : String])
  (office
   [room-num : String]
   [occupant : String]))

(define-type Building
  (empty-building)
  (building
   [room : Room]
   [rest : Building]))

(define-type RobotCommand
  (forward)
  (right)
  (walk [n : Number]))

(define-type-alias RobotProgram (Listof RobotCommand))

; ---------------------------------------------------- ;

; Part 0

(define (room-capacity [room : Room]) : Number
  (type-case Room room
    [(lab rn cap st) cap]
    [(classroom rn cap pr?) cap]
    [(bathroom rn) 0]
    [(office rn occ) 1]))

(define (building-capacity [b : Building]) : Number
  (type-case Building b
    [(empty-building) 0]
    [(building room rest) (+ (room-capacity room) (building-capacity rest))]))

(define (has-labs? [b : Building]) : Boolean
  (type-case Building b
    [(empty-building) #f]
    [(building room rest) (or (lab? room) (has-labs? rest))]))

(module+ test
  (test (room-capacity (lab "1303" 30 27)) 30)
  (test (room-capacity (classroom "1307" 44 #t)) 44)
  (test (room-capacity (bathroom "1304")) 0)
  (test (room-capacity (office "1117D" "Prof. O")) 1))


(module+ test
  (define bietz-center (empty-building))
  (define hickman
    (building (office "1117D" "Prof. O")
              (building (office "1117A" "Siegwart Mayr")
                        (building (classroom "1307" 44 #t)
                                  (building (lab "1303" 30 27)
                                            (empty-building))))))
  (test (building-capacity bietz-center) 0)
  (test (building-capacity hickman) 76))

(module+ test
  (test (has-labs? bietz-center) #f)
  (test (has-labs? hickman) #t))

; Part 1

(define (abs [n : Number]) : Number
  (if (< n 0)
      (- 0 n)
      n))

(define (command-distance [command : RobotCommand]) : Number
  (type-case RobotCommand command
    [(forward) 1]
    [(right) 0]
    [(walk n) n]))
                                                                              
(define (program-distance [program : RobotProgram]) : Number
  (type-case (Listof RobotCommand) program
    [empty 0]
    [(cons command rest-commands) (abs (+ (command-distance command) (program-distance rest-commands)))]))

(module+ test
  (test (abs 0) 0)
  (test (abs -2) 2)
  (test (abs 5) 5))

(module+ test
  (define fw (forward))
  (define rt (right))
  (define walkFive (walk 5))
  (test (command-distance fw) 1)
  (test (command-distance rt) 0)
  (test (command-distance walkFive) 5))

(module+ test
  (define turnAround (list (right) (right)))
  (define moveLshape (list (forward) (walk 1) (right) (forward)))
  (define backAndForth (list (walk -2) (walk 3) (walk -2) (walk 4)))
  (test (program-distance turnAround) 0)
  (test (program-distance moveLshape) 3)
  (test (program-distance backAndForth) 3))

; Part 2

(define (command-turn [command : RobotCommand]) : Number
  (type-case RobotCommand command
    [(forward) 0]
    [(right) 0.25]
    [(walk n) 0]))
                                                                              
(define (program-total-turn [program : RobotProgram]) : Number
  (type-case (Listof RobotCommand) program
    [empty 0]
    [(cons command rest-commands) (+ (command-turn command) (program-total-turn rest-commands))]))

(module+ test
  (test (command-turn fw) 0)
  (test (command-turn rt) 0.25)
  (test (command-turn walkFive) 0))

(module+ test
  (test (program-total-turn turnAround) 0.5)
  (test (program-total-turn moveLshape) 0.25)
  (test (program-total-turn backAndForth) 0))