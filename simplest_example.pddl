(define (domain post-simple)
  (:requirements :strips :typing)
  (:types letter agent)
  (:predicates 
    (at ?l - letter ?a - agent)
  )

  (:action move
    :parameters (?a1 - agent ?l - letter)
    :byagent ?a1
    :precondition (and (at ?l ?a1))

    :effect (and (not (at ?l ?a1)))
  )
)


(define (problem post-simple-5-1)
  (:domain post-simple)
  (:objects 
    A1 - agent
    L1 - letter)
  (:init 
    (at L1 A1)
  )
  (:worlds
    (:world-designated wto3des
      (at L1 A1)
    )
  )
  (:observability none)
  (:goal 
    (and)
  )
)