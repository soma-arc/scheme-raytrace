(define-module vec
  (use gauche.uvector)
  (export-all))

(select-module vec)

(define vec3 f64vector)

(define-inline (x v) (f64vector-ref v 0))
(define-inline (y v) (f64vector-ref v 1))
(define-inline (z v) (f64vector-ref v 2))

(define sum
  (lambda v
    (reduce f64vector-add
            (vec3 0 0 0)
            v)))

(define diff
  (lambda v
    (let loop ((v1 (car v))
               (vecs (cdr v)))
      (if (null? vecs)
          v1
          (loop (f64vector-sub v1 (car vecs))
                (cdr vecs))))))

(define prod
  (lambda v
    (reduce-right f64vector-mul
                  (vec3 1 1 1)
                  v)))

(define scale f64vector-mul)

(define quot
  (lambda v
    (let loop ((v1 (car v))
               (vecs (cdr v)))
      (if (null? vecs)
          v1
          (loop (f64vector-div v1 (car vecs))
                (cdr vecs))))))

(define dot f64vector-dot)

(define-inline (length v)
  (sqrt (dot v v)))

(define-inline (sq-len v)
  (dot v v))

(define (unit v)
  (let ((k (/ 1 (length v))))
    (scale v k)))

(define (cross v1 v2)
  (vec3 (- (* (y v1) (z v2))
           (* (y v2) (z v1)))
        (- (* (z v1) (x v2))
           (* (z v2) (x v1)))
        (- (* (x v1) (y v2))
           (* (x v2) (y v1)))))
