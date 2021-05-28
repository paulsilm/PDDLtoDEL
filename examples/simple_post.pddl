(define (domain post-simple)
	(:requirements :strips :typing)
	(:types letter agent)
	(:predicates
		(connected ?a1 ?a2 - agent);p2 = connected A1 A2,  p6 = connected A2 A3
		(destined ?l - letter ?a - agent);p10 = destined L A1, p11 = destined L A2, p12 = destined L A3
		(not-destined ?l - letter ?a - agent); p13 = not-destined L A1, p14 = not-destined L A2, p15 = not-destined L A3
		(at ?l - letter ?a - agent); p16 = at L A1 
		(not-at ?l - letter ?a - agent); p20 = not-at L A2, p21 = not-at L A3 
		(received ?l - letter); p22 = received L A1 
	)

	(:action move
		:parameters (?a1 ?a2 - agent ?l - letter)
		:byagent ?a1
		:precondition 
			(and
				(connected ?a1 ?a2)
				(at ?l ?a1)
				(knows ?a1 (not (destined ?l ?a1))))
		:effect 
			(and
				(not (at ?l ?a1))
				(at ?l ?a2))
	)

	(:action check
		:parameters (?a - agent ?l - letter)
		:byagent ?a
		(:event-designated check-succ
			:precondition 
				(and
					(at ?l ?a)
					(destined ?l ?a))
			:effect (received ?l)
		)
		(:event-designated check-unsucc
			:precondition 
				(and
					(at ?l ?a)
					(not (destined ?l ?a)))
			:effect 
				(and)
		)
		:observability none
		:observability full ?a
	)
)

(define (problem post-simple-5-1)
	(:domain post-simple)
	(:objects
		 A1 A2 A3 - agent
		 L1 - letter)
	(:init
		(connected A1 A2)
		(connected A2 A3)
		(at L1 A1)
		(not-at L1 A2)
		(not-at L1 A3))
		(:world-nondesignated wto1
			(destined L1 A1)
			(not-destined L1 A2)
			(not-destined L1 A3)
		)
		(:world-nondesignated wto2
			(destined L1 A2)
			(not-destined L1 A1)
			(not-destined L1 A3)
		)
		(:world-designated wto3des
			(destined L1 A3)
			(not-destined L1 A1)
			(not-destined L1 A2)
		)
	(:observability none)
	(:goal
		(forall (?l - letter)
			(received ?l)
		)
	)
)
