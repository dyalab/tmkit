(define (problem linear-sussman)
	(:domain blocks)
	(:objects a b c - block
		 x y z - location)
	(:init (= (position c) a)
	       (= (position a) x)
	       (= (position b) y))
	(:goal (and (= (position a) b)
	       	    (= (position b) c))))
		    
	       