(define (domain muddy)
    (:requirements :strips :typing)
    (:types agent uselessObj)
    (:predicates
        (muddy ?a1 - agent)
    )

    (:action announce-at-least-one
        :parameters (?a - agent)
        :byagent ?a
        :precondition
            (exists (?b - agent) (muddy ?b))
        :effect
            (and)
    )

; "Do you know if you are muddy?" - nobody reacts.
; ==
; "Nobody knows whether they are muddy."
; ==
; "Everyone does not know whether they are muddy."

    (:action announce-nobody-knows
        :parameters (?a - agent)
        :byagent ?a
        :precondition 
                (forall (?b - agent)
                        (and (not (knows ?b (muddy ?b)))
                             (not (knows ?b (not (muddy ?b))))
                        )
                )
            :effect (and)
    )
)


(define (problem muddy-3)
    (:domain muddy)
    (:objects
         A1 A2 A3 - agent
         O - uselessObj
    )
    (:init)
        (:world-designated mmm
            (muddy A1)
            (muddy A2)
            (muddy A3)
        )
        (:world-nondesignated mmc
            (muddy A1)
            (muddy A2)
        )
        (:world-nondesignated mcm
            (muddy A1)
            (muddy A3)
        )
        (:world-nondesignated mcc
            (muddy A1)
        )
        (:world-nondesignated cmm
            (muddy A2)
            (muddy A3)
        )
        (:world-nondesignated cmc
            (muddy A2)
        )
        (:world-nondesignated ccm
            (muddy A3)
        )
        (:world-nondesignated ccc
        )
    (:observability (partition (ccm ccc) (cmc cmm) (mmm mmc) (mcm mcc)) A3
    :observability (partition (cmm ccm) (ccc cmc) (mcm mmm) (mmc mcc)) A2
    :observability (partition (cmm mmm) (cmc mmc) (mcm ccm) (mcc ccc)) A1)
    (:goal
        (forall (?a - agent)
            (or (knows ?a (muddy ?a))
                 (knows ?a (not (muddy ?a)))
            )
        )
    )
)
