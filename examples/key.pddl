(define (domain doormat)
	(:types agent)
	(:predicates
		(key-under-mat)
		(has-key ?a - agent)
	)

	(:action put-key
		:parameters (?a - agent)
		:byagent ?a
		(:event-nondesignated trivial
			:precondition 
				(and)
			:effect 
				(and)
		)
		(:event-designated actual
			:precondition (has-key ?a)
			:effect 
				(and
					(key-under-mat)
					(not (has-key ?a)))
		)
		:observability full ?a
		:observability none
	)

	(:action announce
		:parameters (?a - agent)
		:byagent ?a
		:precondition (key-under-mat)
		:effect 
			(and)
	)

	(:action try-take
		:parameters (?a - agent)
		:byagent ?a
		(:event-designated e1
			:precondition (not (key-under-mat))
			:effect 
				(and)
		)
		(:event-designated e2
			:precondition (key-under-mat)
			:effect 
				(and
					(not (key-under-mat))
					(has-key ?a))
		)
		:observability none
		:observability full ?a
	)
)

(define (problem doormat-1)
	(:domain doormat)
	(:objects
		 anne bob - agent)
	(:world-designated w1
		(has-key anne)
	)
	(:world-nondesignated w2
		(key-under-mat)
	)
	(:observability full anne
	 :observability none bob)
	(:goal
		(has-key bob)
	)
)
