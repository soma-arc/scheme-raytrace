(define-module texture
  (use srfi-27)
  (use vec :prefix v:)
  (use perlin)
  (export-all))

(select-module texture)

(define-inline (value tex u v p)
  ((vector-ref tex 0) u v p))

(define (constant-texture color)
  (vector (lambda (u v p)
            color)))

(define (checker-texture even-tex odd-tex)
  (vector (lambda (u v p)
            (let ((sines (* (sin (* 10 (v:x p)))
                            (sin (* 10 (v:y p)))
                            (sin (* 10 (v:z p))))))
              (if (< sines 0)
                  (value odd-tex u v p)
                  (value even-tex u v p))))))

(define (noise-texture sc)
  (vector (lambda (u v p)
            (v:scale (v:vec3 1 1 1)
                     (noise (v:scale p sc))))))

(define (marble-texture sc)
  (vector (lambda (u v p)
            (v:scale (v:vec3 1 1 1)
                     (* 0.5 (+ 1 (sin (+ (* sc (v:z p))
                                         (* 10 (turb p))))))))))

