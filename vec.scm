(define-module vec
  (use gauche.record)
  (export-all))

(select-module vec)

(define-record-type vec #t #t
  x y z)

(define (sum v1 v2)
  (make-vec (+ (vec-x v1) (vec-x v2))
            (+ (vec-y v1) (vec-y v2))
            (+ (vec-z v1) (vec-z v2))))

(define (diff v1 v2)
  (make-vec (- (vec-x v1) (vec-x v2))
            (- (vec-y v1) (vec-y v2))
            (- (vec-z v1) (vec-z v2))))

(define (prod v1 v2)
  (make-vec (* (vec-x v1) (vec-x v2))
            (* (vec-y v1) (vec-y v2))
            (* (vec-z v1) (vec-z v2))))

(define (quot v1 v2)
  (make-vec (/ (vec-x v1) (vec-x v2))
            (/ (vec-y v1) (vec-y v2))
            (/ (vec-z v1) (vec-z v2))))

(define (length v)
  (sqrt (+ (* (vec-x v) (vec-x v))
           (* (vec-y v) (vec-y v))
           (* (vec-z v) (vec-z v)))))

(define (sq-len v)
  (+ (* (vec-x v) (vec-x v))
     (* (vec-y v) (vec-y v))
     (* (vec-z v) (vec-z v))))

(define (unit v)
  (let ((k (/ 1 (length v))))
    (prod v k)))

(define (dot v1 v2)
  (+ (* (vec-x v1) (vec-x v2))
     (* (vec-y v1) (vec-y v2))
     (* (vec-z v1) (vec-z v2))))

(define (cross v1 v2)
  (make-vec (- (* (vec-y v1) (vec-z v2))
               (* (vec-y v2) (vec-z v1)))
            (- (* (vec-z v1) (vec-x v2))
               (* (vec-z v2) (vec-x v1)))
            (- (* (vec-x v1) (vec-y v2))
               (* (vec-x v2) (vec-y v1)))))
