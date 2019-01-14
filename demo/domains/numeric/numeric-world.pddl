(define (domain numeric)
   (:predicates (can-square)
   		(can-sqrt)
		(can-add)
		(can-subtract)
		(can-multiply)
		(can-divide))
   (:action square
   	    :parameters (?x - num)
	    :precondition (can-square)
	    :effect (and (= ?x (^ ?x 2))))
   (:action sqrt
   	    :parameters (?x - num)
	    :precondition (can-sqrt)
	    :effect (and (= ?x (^ ?x 0.5))))
   (:action add
   	    :parameters (?x ?y - num)
	    :precondition (can-add)
	    :effect (and (= ?x (+ ?x ?y))))
   (:action subtract
   	    :parameters (?x ?y - num)
	    :precondition (can-subtract)
	    :effect (and (= ?x (- ?x ?y))))
   (:action multiply
   	    :parameters (?x ?y - num)
	    :precondition (can-multiply)
	    :effect (and (= ?x (* ?x ?y))))
   (:action divide
   	    :parameters (?x ?y - num)
	    :precondition (can-divide)
	    :effect (and (= ?x (/ ?x ?y)))))