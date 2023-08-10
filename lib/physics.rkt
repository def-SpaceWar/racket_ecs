#lang racket

(provide pvector
         pvector-x
         pvector-y)
(define-struct pvector
  (x y))

(provide v-magnitude-squared)
;; v-magnitude-squared :: pvector -> number
(define (v-magnitude-squared v)
  (+ (sqr (pvector-x v)) (sqr (pvector-y v))))

(provide v-magnitude)
;; v-magnitude :: pvector -> number
(define (v-magnitude v)
  (sqrt (v-magnitude-squared v)))

(provide v-cross)
;; v-cross :: pvector pvector -> number
(define (v-cross v1 v2)
  (- (* (pvector-x v1) (pvector-y v2))
     (* (pvector-x v2) (pvector-y v1))))

(provide v-dot)
;; v-dot :: pvector pvector -> number
(define (v-dot v1 v2)
  (+ (* (pvector-x v1) (pvector-y v2))
     (* (pvector-x v2) (pvector-y v1))))

(provide v-add)
;; v-add :: pvector pvector -> pvector
(define (v-add v1 v2)
  (pvector (+ (pvector-x v1)
              (pvector-x v2))
           (+ (pvector-y v1)
              (pvector-y v2))))

(provide v-scale)
;; v-scale :: pvector number -> pvector
(define (v-scale v n)
  (pvector (* (pvector-x v) n)
           (* (pvector-y v) n)))

(provide make-polygon
         polygon-points)
(define-struct polygon
  (points))

;; inertia-of :: []pvector -> [](pair pvector)
(define (points-to-lines points)
  (foldl (lambda (point-index total)
           (define next-point-index
             (if (= point-index (- (length points) 1))
                 0
                 (point-index + 1)))
           (cons (cons (list-ref points point-index)
                       (list-ref points next-point-index))
                 total))
         null
         (range (length points))))

(provide polygon-inertia)
;; inertia-of :: polygon -> number
(define (polygon-inertia polygon)
  (define lines (points-to-lines (polygon-points polygon)))
  (/ (foldl (lambda (line total)
              (define p1 (first line))
              (define p2 (second line))
              (+ total
                 (* (v-cross p2 p1)
                    (+ (v-dot p1 p1)
                       (v-dot p1 p2)
                       (v-dot p2 p2)))))
            0
            lines)
     (* 6 (foldl (lambda (line total)
                   (+ total
                      (v-cross (second line)
                               (first line))))
                 0
                 lines))))
