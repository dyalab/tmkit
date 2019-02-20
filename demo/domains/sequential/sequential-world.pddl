(define (domain sequential)
	(:predicates (first)
		     (second)
		     (can-set-second))
	(:action set-first
		 :parameters ()
		 :precondition (and (not (first)))
		 :effect (and (first)))
	(:action prepare-second
		 :parameters ()
		 :precondition (and (not (first)))
		 :effect (and (can-set-second)))
	(:action set-second
		 :parameters ()
		 :precondition (and (can-set-second))
		 :effect (and (second))))