(define (problem check-subtraction)
     (:domain numeric)
   (:objects a b - num)
   (:init (= a 5)
   	  (= b 1)
   	  (can-subtract))
   (:goal (and (= a 1))))