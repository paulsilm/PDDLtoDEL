(define (domain muddy)
    (:requirements :strips :typing)
    (:types agent - object
            god child - agent)
    (:constants A1 A2 A3 - child
                God - god)
    (:predicates
        (muddy ?a - child)
    )

    (:action announce-at-least-one
        :parameters ()
        :byagent God
        (:event-nondesignated trivial 
            :precondition (and)
            :effect (and)
        )
        (:event-designated actual
            :precondition
                (exists (?b - child) (muddy ?b))
            :effect
                (and)
        )
    )

    (:action round-kkk
        :parameters (?a1 ?a2 ?a3 - child)
        :byagent God
        :precondition
            (and
                (not (= ?a1 ?a2))
                (not (= ?a1 ?a3))
                (not (= ?a2 ?a3))
                (or (knows ?a1 (muddy ?a1))
                    (knows ?a1 (not (muddy ?a1))))
                (or (knows ?a2 (muddy ?a2))
                    (knows ?a2 (not (muddy ?a2))))
                (or (knows ?a3 (muddy ?a3))
                    (knows ?a3 (not (muddy ?a3))))
            )
        :effect (and)
    )
    (:action round-kkn
        :parameters (?a1 ?a2 ?a3 - child)
        :byagent God
        :precondition
            (and
                (not (= ?a1 ?a2))
                (not (= ?a1 ?a3))
                (not (= ?a2 ?a3))
                (or (knows ?a1 (muddy ?a1))
                    (knows ?a1 (not (muddy ?a1))))
                (or (knows ?a2 (muddy ?a2))
                    (knows ?a2 (not (muddy ?a2))))
                (not (knows ?a3 (muddy ?a3)))
                (not (knows ?a3 (not (muddy ?a3))))
            )
        :effect (and)
    )
    (:action round-knn
        :parameters (?a1 ?a2 ?a3 - child)
        :byagent God
        :precondition
            (and
                (not (= ?a1 ?a2))
                (not (= ?a1 ?a3))
                (not (= ?a2 ?a3))
                (or (knows ?a1 (muddy ?a1))
                    (knows ?a1 (not (muddy ?a1))))
                (not (knows ?a2 (muddy ?a2)))
                (not (knows ?a2 (not (muddy ?a2))))
                (not (knows ?a3 (muddy ?a3)))
                (not (knows ?a3 (not (muddy ?a3))))
            )
        :effect (and)
    )
    
    (:action round-nnn
        :parameters (?a1 ?a2 ?a3 - child)
        :byagent God
        :precondition
            (and
                (not (= ?a1 ?a2))
                (not (= ?a1 ?a3))
                (not (= ?a2 ?a3))
                (not (knows ?a1 (muddy ?a1)))
                (not (knows ?a1 (not (muddy ?a1))))
                (not (knows ?a2 (muddy ?a2)))
                (not (knows ?a2 (not (muddy ?a2))))
                (not (knows ?a3 (muddy ?a3)))
                (not (knows ?a3 (not (muddy ?a3))))
            )
        :effect (and)
    )
)


(define (problem muddy-3)
    (:domain muddy)
    ; All children know that other children know whether they're muddy or not
        (:world-nondesignated ccc)
        (:world-nondesignated ccm
            (muddy A3))
        (:world-nondesignated cmc
            (muddy A2))
        (:world-nondesignated cmm
            (muddy A2)
            (muddy A3))
        (:world-nondesignated mcc
            (muddy A1))
        (:world-nondesignated mcm
            (muddy A1)
            (muddy A3))
        (:world-nondesignated mmc
            (muddy A1)
            (muddy A2))
        (:world-designated mmm
            (muddy A1)
            (muddy A2)
            (muddy A3))
    (:observability (partition (cmm mmm) (cmc mmc) (mcm ccm) (mcc ccc)) A1)
    (:observability (partition (cmm ccm) (cmc ccc) (mcm mmm) (mmc mcc)) A2)
    (:observability (partition (ccm ccc) (cmc cmm) (mmm mmc) (mcm mcc)) A3)
    (:goal
        (forall (?a - child)
            (or 
                (knows ?a (muddy ?a))
                (knows ?a (not (muddy ?a)))
            )
        )
    )
)
