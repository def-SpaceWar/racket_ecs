#lang racket

(require "ecs.rkt")
(require "components.rkt")
(require "physics.rkt")

(provide forces-system)
;; forces-system :: number -> (world -> world)
(define (forces-system dt)
  (define (update-entity-velocity entity)
    (define position-raw (get-component entity POSITION))
    (if (eq? position-raw null)
        entity
        ((lambda ()
           (define e-position (component-data position-raw))
           (define velocity-raw (get-component entity VELOCITY))
           (if (eq? velocity-raw null)
               entity
               ((lambda ()
                  (define e-velocity (component-data velocity-raw))
                  (define e-position-new (v-add e-position (v-scale e-velocity dt)))
                  (define current-components (entity-components entity))
                  (define filtered-components
                    (filter (lambda (component)
                              (not (eq? (component-type-id component)
                                        POSITION)))
                              current-components))
                  (define new-position
                    (v-position e-position-new))
                  (make-entity (entity-id entity)
                               (cons new-position filtered-components)))))))))
  (lambda (w)
    (make-world (world-entity-count w)
                (map update-entity-velocity (world-entities w)))))
