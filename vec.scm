(define-module vec
  (use gauche.record)
  (export-all))

(select-module vec)

(define-record-type vec vec3 vec3?
  (x x x-set!)
  (y y y-set!)
  (z z z-set!))

(define sum
  (lambda v
    (reduce (lambda (v1 v2)
              (vec3 (+ (x v1) (x v2))
                    (+ (y v1) (y v2))
                    (+ (z v1) (z v2))))
            (vec3 0 0 0)
            v)))

(define diff
  (lambda v
    (let loop ((v1 (car v))
               (vecs (cdr v)))
      (if (null? vecs)
          v1
          (loop (vec3 (- (x v1) (x (car vecs)))
                      (- (y v1) (y (car vecs)))
                      (- (z v1) (z (car vecs))))
                (cdr vecs))))))

(define prod
  (lambda v
    (reduce-right (lambda (v1 v2)
              (vec3 (* (x v1) (x v2))
                    (* (y v1) (y v2))
                    (* (z v1) (z v2))))
            (vec3 1 1 1)
            v)))

(define (scale v k)
  (vec3 (* (x v) k)
        (* (y v) k)
        (* (z v) k)))

(define quot
  (lambda v
    (let loop ((v1 (car v))
               (vecs (cdr v)))
      (if (null? vecs)
          v1
          (loop (vec3 (/ (x v1) (x (car vecs)))
                      (/ (y v1) (y (car vecs)))
                      (/ (z v1) (z (car vecs))))
                (cdr vecs))))))

(define (length v)
  (sqrt (+ (* (x v) (x v))
           (* (y v) (y v))
           (* (z v) (z v)))))

(define (sq-len v)
  (+ (* (x v) (x v))
     (* (y v) (y v))
     (* (z v) (z v))))

(define (unit v)
  (let ((k (/ 1 (length v))))
    (scale v k)))

(define (dot v1 v2)
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))
     (* (z v1) (z v2))))

(define (cross v1 v2)
  (vec3 (- (* (y v1) (z v2))
           (* (y v2) (z v1)))
        (- (* (z v1) (x v2))
           (* (z v2) (x v1)))
        (- (* (x v1) (y v2))
           (* (x v2) (y v1)))))
