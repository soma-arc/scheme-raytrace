(add-load-path "." :relative)

(define-module main
  (use vec :prefix v:)
  (use srfi-27)
  (use math.const)
  (use util)
  (use ray)
  (use geometry :prefix g:)
  (use material :prefix m:)
  (use camera))

(select-module main)

(define +max-float+ 999999999999)

(define +max-depth+ 30)

(define (color r obj-list depth)
  (if (> depth +max-depth+)
      (v:vec3 0 0 0)
      (receive (hit? hit-rec)
               (g:calc-nearest obj-list r 0.0 +max-float+)
               (if hit?
                   (receive (valid? scattered attenuation)
                            (m:scatter (g:material hit-rec) r hit-rec)
                            (if valid?
                                (v:prod attenuation (color scattered obj-list (+ depth 1)))
                                (v:vec3 0 0 0)))
                   (let* ((unit-dir (v:unit (dir r)))
                          (t (* 0.5 (+ 1.0 (v:y unit-dir)))))
                     (v:sum (v:scale (v:vec3 1 1 1) (- 1 t))
                            (v:scale (v:vec3 0.5 0.7 1.0) t)))))))


(let* ((nx 200)
       (ny 100)
       (ns 3)
       (camera (cam (v:vec3 -2 2 1)
                    (v:vec3 0 0 -1)
                    (v:vec3 0 1 0) 45 (/ nx ny)))
       (R (cos pi/4))
       (obj-list
        (list (make g:<sphere>
                :center (v:vec3 0 0 -1)
                :radius 0.5
                :material (make m:<lambertian>
                            :albedo (v:vec3 0.1 0.2 0.5)))
              (make g:<sphere>
                :center (v:vec3 0 -100.5 -1)
                :radius 100
                :material (make m:<lambertian>
                            :albedo (v:vec3 0.8 0.8 0)))
              (make g:<sphere>
                :center (v:vec3 1 0 -1)
                :radius 0.5
                :material (make m:<metal>
                            :albedo (v:vec3 0.8 0.6 0.2)
                            :fuzz 1.0))
              (make g:<sphere>
                :center (v:vec3 -1 0 -1)
                :radius 0.5
                :material (make m:<dielectric>
                            :ref-idx 1.5))
              (make g:<sphere>
                :center (v:vec3 -1 0 -1)
                :radius -0.45
                :material (make m:<dielectric>
                            :ref-idx 1.5)))))
  (with-output-to-file "test.ppm"
    (lambda ()
      (display (format "P3\n ~D ~D\n255\n" nx ny))
      (let loop-y ((y (- ny 1)))
        (if (>= y 0)
            (let loop-x ((x 0))
              (if (< x nx)
                  (let* ((c (let loop ((sample-count 0)
                                       (col (v:vec3 0 0 0)))
                              (if (>= sample-count ns)
                                  (v:quot col (v:vec3 ns ns ns))
                                  (let* ((u (/ (+ x (random-real)) nx))
                                         (v (/ (+ y (random-real)) ny))
                                         (ray (get-ray camera u v))
                                         (col (v:sum col
                                                     (color ray obj-list 0))))
                                    (loop (+ 1 sample-count) col)))))
                         (c (v:vec3 (sqrt (v:x c)) (sqrt (v:y c)) (sqrt (v:z c))))
                         (ir (floor->exact (* 255.99 (v:x c))))
                         (ig (floor->exact (* 255.99 (v:y c))))
                         (ib (floor->exact (* 255.99 (v:z c)))))
                    (display (format "~D ~D ~D\n"
                                     ir ig ib))
                    (loop-x (+ x 1)))
                  (loop-y (- y 1)))))))))

