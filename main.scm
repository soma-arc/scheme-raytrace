(add-load-path "." :relative)

(define-module main
  (use vec :prefix v:)
  (use ray))

(select-module main)

(define (hit-sphere center rad r)
  (let* ((oc (v:diff (origin r) center))
         (a (v:dot (dir r) (dir r)))
         (b (* 2 (v:dot oc (dir r))))
         (c (- (v:dot oc oc) (* rad rad)))
         (discriminant (- (* b b) (* 4 a c))))
    (> discriminant 0)))

(define (color r)
  (if (hit-sphere (v:vec3 0 0 -1) 0.5 r)
      (v:vec3 1 0 0)
      (let* ((unit-dir (v:unit (dir r)))
             (t (* 0.5 (+ 1.0 (v:y unit-dir)))))
        (v:sum (v:scale (v:vec3 1 1 1) (- 1 t))
               (v:scale (v:vec3 0.5 0.7 1.0) t)))))


(let ((nx 200)
      (ny 100)
      (lower-left-corner (v:vec3 -2 -1 -1))
      (horizontal (v:vec3 4 0 0))
      (vertical (v:vec3 0 2 0))
      (origin (v:vec3 0 0 0)))
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
                         (col (color ray))
                         (ir (floor->exact (* 255.99 (v:x col))))
                         (ig (floor->exact (* 255.99 (v:y col))))
                         (ib (floor->exact (* 255.99 (v:z col)))))
                    (display (format "~D ~D ~D\n"
                                     ir ig ib))
                    (loop-x (+ x 1)))
                  (loop-y (- y 1)))))))))
