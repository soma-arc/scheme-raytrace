(define-module ray
  (use gauche.record)
  (use vec :prefix v:)
  (export-all))

(select-module ray)

(define-inline (make-ray origin dir)
  (vector origin dir))

(define-inline (origin ray)
  (vector-ref ray 0))

(define-inline (dir ray)
  (vector-ref ray 1))

(define (point-at-parameter ray t)
  (v:sum (origin ray)
         (v:scale (dir ray) t)))

(define-inline (make-hit-record t p normal material)
  (vector t p normal material))

(define-inline (t hit-rec)
  (vector-ref hit-rec 0))

(define-inline (p hit-rec)
  (vector-ref hit-rec 1))

(define-inline (normal hit-rec)
  (vector-ref hit-rec 2))

(define-inline (material hit-rec)
  (vector-ref hit-rec 3))
