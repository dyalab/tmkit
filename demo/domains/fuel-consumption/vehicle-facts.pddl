;;problem optimizing fuel usage for a trip form madrid to munich
(define (problem vehicle-optimization)
	(:domain vehicle)
	(:objects
		car1 - vehicle
		paris munich rome madrid brussels berlin - location)
	(:init
		(at car1 madrid)
		(= (fuel-level car1) 200)
		(accessible car1 brussels berlin)
		(accessible car1 madrid paris)
		(accessible car1 madrid rome)
		(accessible car1 munich berlin)
		(accessible car1 Paris Munich)
		(accessible car1 paris brussels)
		(accessible car1 rome munich)
		(= (fuel-required brussels berlin) 40)
		(= (fuel-required madrid paris) 70)
		(= (fuel-required madrid rome) 60)
		(= (fuel-required munich berlin) 20)
		(= (fuel-required Paris munich) 20)
		(= (fuel-required paris brussels) 10)
		(= (fuel-required rome munich) 40)
		(= (total-fuel-used) 0)
		(= (fuel-used car1) 0))
	(:goal (and (at car1 berlin)))
	(:metric (minimize total-fuel-used)))