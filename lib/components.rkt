#lang racket

(require "ecs.rkt")
(require "physics.rkt")

(provide DISPLAY-IMAGE
         display-image)
(define DISPLAY-IMAGE "display-image")
;; display-image :: image -> component
(define (display-image img)
  (make-component DISPLAY-IMAGE
                  img))

(provide POSITION
         position
         v-position)
(define POSITION "position")
;; position :: number number -> component
(define (position x y)
  (make-component POSITION
                  (pvector x y)))
;; position :: pvector -> component
(define (v-position v)
  (make-component POSITION
                  v))

(provide VELOCITY
         velocity
         v-velocity)
(define VELOCITY "velocity")
;; velocity :: number number -> component
(define (velocity x y)
  (make-component VELOCITY
                  (pvector x y)))
;; v-velocity :: pvector -> component
(define (v-velocity v)
  (make-component VELOCITY
                  v))
