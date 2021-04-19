(define (domain doormat)
  (:agents (anne)
           (bob))
  (:predicates (key-under-mat)
               (has-key ?x - agent))
  
  (:action put-key
           :observability full anne
           :observability none bob
           (:event-nondesignated trivial 
                :precondition (and)
                :effect (and))
           (:event-designated actual
                :precondition (has-key anne)
                :effect (and (key-under-mat)
                        (not (has-key anne))))
  )

  (:action announce
           :observability full anne
           :observability full bob
           (:event-designated actual
                :precondition (has-key anne)
                :effect (and (key-under-mat)
                        (not (has-key anne)))))

  (:action try-take
          :observability none anne
          :observability full bob
          (:event-designated e1
	       :precondition (not (key-under-mat))
              :effect T)
          (:event-designated e2
	       :precondition (key-under-mat)
              :effect and (not (key-under-mat)) (has-key bob)))

## Problem

(define (problem doormat-1)
   (:domain doormat)
   (:objects anne bob - agent) 
   (:init )
    (:worlds
     (:world-designated w1 
          (not (key-under-mat))
          (has-key anne)
          (not (has-key bob)))
     (:world-nondesignated w2 
          (key-under-mat)
          (not (has-key anne))
          (not (has-key bob))))
          (:observability full anne
     :observability none bob)
   (:goal (has-key bob))