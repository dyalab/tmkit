; Task-Motion extension of the 4-operator blocks world domain from the
; 2nd International Planning Competition.

;;; Extensions:
;;; ===========
;;; * Object types for BLOCK and LOCATION
;;; * ONTABLE, PICK-UP, and PUT-DOWN take a second argument for the location

(define (domain blocks)
    (:requirements :typing)
  (:types block - object
  	  location - object)
  (:predicates (on ?x - block ?y - block)
               (ontable ?x - block ?loc - location)
               (clear ?x - object) (handempty ?x - actor) (holding ?x - block ?y - actor))
  (:action pick-up
           :parameters (?x - block ?loc - location ?a - actor)
           :precondition (and (clear ?x)
                              (ontable ?x ?loc)
                              (handempty ?a))
           :effect (and (not (ontable ?x ?loc))
                        (not (clear ?x))
                        (not (handempty ?a))
                        (holding ?x ?a)
			(clear ?loc)))
  (:action put-down
           :parameters (?x - block ?loc - location ?a - actor)
           :precondition (and (holding ?x ?a)
	   		      (clear ?loc))
           :effect (and (not (holding ?x ?a))
                        (handempty ?a)
                        (clear ?x)
                        (ontable ?x ?loc)
			(not (clear ?loc))))
  (:action stack
           :parameters (?x - block ?y - block ?a - actor)
           :precondition (and (holding ?x ?a) (clear ?y))
           :effect (and (not (holding ?x ?a))
                        (handempty ?a)
                        (not (clear ?y))
                        (clear ?x)
                        (on ?x ?y)))
  (:action unstack
           :parameters (?x - block ?y - block ?a - actor)
           :precondition (and (on ?x ?y)
                              (clear ?x)
                              (handempty ?a))
           :effect (and (holding ?x ?a)
                        (not (handempty ?a))
                        (not (clear ?x))
                        (clear ?y)
                        (not (on ?x ?y)))))
