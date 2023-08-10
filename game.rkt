#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(require "lib/ecs.rkt")
(require "lib/components.rkt")
(require "lib/physics.rkt")
(require "lib/systems.rkt")

;; My Constants
(define DELTA-TIME .01)
(define WIDTH 800)
(define HEIGHT 600)
(define EMPTY-WORLD (make-world 0 null))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SYSTEMS
  (list
    (forces-system DELTA-TIME)))

;; INITIAL-WORLD :: -> world
(define (INITIAL-WORLD)
  (foldl (lambda (components world)
           (generate-entity world components))
         EMPTY-WORLD
         (reverse
          (list
           (list
            (position 100 200)
            (velocity 150 40)
            (display-image (rectangle 100 100 "solid" "red")))
           (list
            (position 200 500)
            (velocity 100 -50)
            (display-image (circle 50 "solid" "orange")))))))

;; imageFromEntity :: entity -> image
(define (imageFromEntity entity previous)
  (define position-preprocessed (get-component entity POSITION))
  (define position
    (if (eq? position-preprocessed null)
        (pvector 0 0)
        (component-data position-preprocessed)))
  (define image-preprocessed (get-component entity DISPLAY-IMAGE))
  (if (eq? image-preprocessed null)
      previous
      (place-image (component-data image-preprocessed)
                   (pvector-x position)
                   (pvector-y position)
                   previous)))

;; draw :: world -> image
(define (draw w)
  (foldl (lambda (e total)
           (imageFromEntity e total))
         BACKGROUND
         (world-entities w)))

;; update :: world -> world
(define (update w)
  (foldl (lambda (sys world)
           (sys world))
         w
         SYSTEMS))

(big-bang (INITIAL-WORLD)
  [name "ECS Game Template"]
  [to-draw draw]
  [on-tick update DELTA-TIME])
