(add-load-path "." :relative)

(define-module main
  (use vec :prefix v:)
  (use ray)
  (use geometry :prefix g:))

(select-module main)

(define +max-float+ 999999999999)

(define (color r obj-list)
  (receive (hit? hit-rec)
           (g:calc-nearest obj-list r 0.0 +max-float+)
           (if hit?
               (v:scale (v:sum (g:normal hit-rec)
                               (v:vec3 1 1 1))
                        0.5)
               (let* ((unit-dir (v:unit (dir r)))
                      (t (* 0.5 (+ 1.0 (v:y unit-dir)))))
                 (v:sum (v:scale (v:vec3 1 1 1) (- 1 t))
                        (v:scale (v:vec3 0.5 0.7 1.0) t))))))


(let ((nx 200)
      (ny 100)
      (lower-left-corner (v:vec3 -2 -1 -1))
      (horizontal (v:vec3 4 0 0))
      (vertical (v:vec3 0 2 0))
      (origin (v:vec3 0 0 0))
      (obj-list (list (make g:<sphere>
                        :center (v:vec3 0 0 -1)
                        :radius 0.5)
                      (make g:<sphere>
                        :center (v:vec3 0 -100.5 -1)
                        :radius 100))))
  (with-output-to-file "test.ppm"
    (lambda ()
      (display (format "P3\n ~D ~D\n255\n" nx ny))
      (let loop-y ((y (- ny 1)))
        (if (>= y 0)
            (let loop-x ((x 0))
              (if (< x nx)
                  (let* ((u (/ x nx))
                         (v (/ y ny))
                         (ray (make-ray origin
                                        (v:sum lower-left-corner
                                               (v:scale horizontal u)
                                               (v:scale vertical v))))
                         (col (color ray obj-list))
                         (ir (floor->exact (* 255.99 (v:x col))))
                         (ig (floor->exact (* 255.99 (v:y col))))
                         (ib (floor->exact (* 255.99 (v:z col)))))
                    (display (format "~D ~D ~D\n"
                                     ir ig ib))
                    (loop-x (+ x 1)))
                  (loop-y (- y 1)))))))))
