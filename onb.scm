(define-module onb
  (use util)
  (use vec :prefix v:)
  (export-all))

(select-module onb)

(define (make-onb-from-w n)
  (let* ((axis2 (v:unit n))
         (a (if (> (abs (v:x axis2)) 0.9)
                (v:vec3 0 1 0)
                (v:vec3 1 0 0)))
         (axis1 (v:unit (v:cross axis2 a)))
         (axis0 (v:cross axis2 axis1))
         (axis (vector axis0 axis1 axis2)))
    axis))

(define (u onb)
  (vector-ref onb 0))

(define (v onb)
  (vector-ref onb 1))

(define (w onb)
  (vector-ref onb 2))

(define-syntax local
  (syntax-rules ()
    ((_ onb a b c)
     (v:sum (v:scale (u onb) a)
            (v:scale (v onb) b)
            (v:scale (w onb) c)))
    ((_ onb a)
     (v:sum (v:scale (u onb) (v:x a))
            (v:scale (v onb) (v:y a))
            (v:scale (w onb) (v:z a))))))
