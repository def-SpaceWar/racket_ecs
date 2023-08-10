#lang racket

(provide make-world
         world-entity-count
         world-entities)
(define-struct world
  (entity-count
   entities))

(provide make-entity
         entity-id
         entity-components)
(define-struct entity
  (id
   components))

(provide make-component
         component-type-id
         component-data)
(define-struct component
  (type-id
   data))

(provide generate-entity)
;; generate-entity :: world -> world
(define (generate-entity w components)
  (define entity-count (world-entity-count w))
  (define new-entity (make-entity entity-count components))
  (make-world (+ 1 entity-count)
              (cons new-entity (world-entities w))))

(provide add-component)
;; add-component :: entity component world -> world
(define (add-component entity component w)
  (define id (entity-id entity))
  (define new-entity
    (make-entity id
                 (cons component (entity-components entity))))
  (define new-entities
    (cons new-entity
          (filter (lambda (e)
                    (not (= (entity-id e) id)))
                  (world-entities w))))
  (make-world (world-entity-count w)
              new-entities))

(provide get-components)
;; get-components :: entity component-type-id -> list component
(define (get-components entity component-id)
  (filter (lambda (comp)
            (eq? (component-type-id comp) component-id))
          (entity-components entity)))

(provide get-component)
;; get-component :: entity component-type-id -> component?
(define (get-component entity component-id)
  (define components (get-components entity component-id))
  (if (= (length components) 0)
      null
      (list-ref components 0)))
