(define-module ray
  (use gauche.record)
  (use vec :prefix v:)
  (export-all))

(select-module ray)

(define-inline (make-ray origin dir)
  (vector origin dir 0))

(define-inline (make-ray-with-time origin dir time)
  (vector origin dir time))

(define-inline (origin ray)
  (vector-ref ray 0))

(define-inline (dir ray)
  (vector-ref ray 1))

(define-inline (ray-time ray)
  (vector-ref ray 2))

(define (point-at-parameter ray t)
  (v:sum (origin ray)
         (v:scale (dir ray) t)))

(define-inline (make-hit-record t p normal material u v)
  (vector t p normal material u v))

(define-inline (t hit-rec)
  (vector-ref hit-rec 0))

(define-inline (p hit-rec)
  (vector-ref hit-rec 1))

(define-inline (normal hit-rec)
  (vector-ref hit-rec 2))

(define-inline (normal-set! hit-rec value)
  (set! (vector-ref hit-rec 2) value)
  hit-rec)

(define-inline (material hit-rec)
  (vector-ref hit-rec 3))

(define-inline (u hit-rec)
  (vector-ref hit-rec 4))

(define-inline (v hit-rec)
  (vector-ref hit-rec 5))
