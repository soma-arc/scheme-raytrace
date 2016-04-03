(add-load-path "." :relative)

(define-module main
  (use vec :prefix v:)
  (use srfi-27)
  (use math.const)
  (use util)
  (use ray)
  (use geometry :prefix g:)
  (use material :prefix m:)
  (use camera)
  (use gauche.uvector)
  (use gl)
  (use gl.glut))

(select-module main)

(define +max-float+ 999999999999)
(define +max-depth+ 5)

(define +black+ (v:vec3 0 0 0))
(define +white+ (v:vec3 1 1 1))

(define (random-scene)
  (let ((n 300)
        (obj-list '()))
    (push! obj-list
           (g:make-sphere
            (v:vec3 0 -1000 0) 1000
            (m:make-lambertian
             (v:vec3 0.5 0.5 0.5))))
    (let loop-a ((a -5))
      (if (< a 5)
          (let loop-b ((b -5))
            (if (< b 5)
                (let ((choose-mat (random-real))
                      (center (v:vec3 (+ a (* 0.9 (random-real)))
                                      0.2
                                      (+ b (* 0.9 (random-real))))))
                  (if (> (v:length (v:diff center (v:vec3 4 0.2 0))) 0.9)
                      (cond
                       ((< choose-mat 0.8)
                        (push! obj-list
                               (g:make-moving-sphere
                                center
                                (v:sum center (v:vec3 0 (* 0.5 (random-real)) 0)) 0 1 0.2
                                (m:make-lambertian
                                 (v:vec3 (* (random-real) (random-real))
                                         (* (random-real) (random-real))
                                         (* (random-real) (random-real)))))))
                       ((< choose-mat 0.95)
                        (push! obj-list
                               (g:make-sphere
                                center 0.2
                                (m:make-metal
                                  (v:vec3 (* 0.5 (+ 1 (random-real)))
                                                  (* 0.5 (+ 1 (random-real)))
                                                  (* 0.5 (+ 1 (random-real))))
                                  (* 0.5 (random-real))))))
                       (else (push! obj-list
                                    (g:make-sphere
                                     center 0.2
                                     (m:make-dielectric 1.5))))))
                  (loop-b (inc! b)))
                (loop-a (inc! a))))))
    (push! obj-list
           (g:make-sphere
            (v:vec3 0 1 0) 1
            (m:make-dielectric 1.5)))
    (push! obj-list
           (g:make-sphere
            (v:vec3 -4 1 0) 1
            (m:make-lambertian (v:vec3 0.4 0.2 0.1))))
    (push! obj-list
           (g:make-sphere
            (v:vec3 4 1 0) 1
            (m:make-metal (v:vec3 0.7 0.6 0.5) 0)))))

(define (sky-color t)
  (v:sum (v:scale +white+ (- 1 t))
         (v:scale (v:vec3 0.5 0.7 1.0) t)))

(define (color r obj-list)
  (letrec ((col
            (lambda (r obj-list depth acc)
              (if (> depth +max-depth+)
                  +black+
                  (receive (hit? hit-rec)
                           (g:calc-nearest obj-list r 0.0 +max-float+)
                           (if hit?
                               (receive (valid? scattered attenuation)
                                        (m:scatter (material hit-rec) r hit-rec)
                                        (if valid?
                                            (col scattered obj-list (inc! depth)
                                                 (v:prod acc attenuation))
                                            +black+))
                               (let* ((unit-dir (v:unit (dir r)))
                                      (t (* 0.5 (+ 1.0 (v:y unit-dir)))))
                                 (v:prod acc (sky-color t)))))))))
    (col r obj-list 0 +white+)))

(define (calc-pixel-color x y nx ny camera obj-list ns)
  (let loop ((sample-count 0)
             (col (v:vec3 0 0 0)))
    (if (>= sample-count ns)
        (v:quot col (v:vec3 ns ns ns))
        (let* ((u (/ (+ x (random-real)) nx))
              (v (/ (+ y (random-real)) ny))
              (ray (get-ray camera u v)))
          (loop (inc! sample-count) (v:sum col (color ray obj-list)))))))

(define-inline (correct-gamma c)
  (v:vec3 (sqrt (v:x c)) (sqrt (v:y c)) (sqrt (v:z c))))

(define test-scene
  (list (g:make-sphere (v:vec3 0 0 -1) 0.5
                                   (m:make-lambertian
                                    (v:vec3 0.1 0.2 0.5)))
                    (g:make-sphere (v:vec3 0 -100.5 -1) 100
                                   (m:make-lambertian
                                     (v:vec3 0.8 0.8 0)))
                    (g:make-sphere (v:vec3 1 0 -1) 0.5
                                   (m:make-metal
                                     (v:vec3 0.8 0.6 0.2)
                                      0.3))
                    (g:make-sphere (v:vec3 -1 0 -1) 0.5
                                   (m:make-dielectric 1.5))
                    (g:make-sphere (v:vec3 -1 0 -1) -0.45
                                   (m:make-dielectric 1.5))))

(time (let* ((nx 200)
             (ny 100)
             (ns 20)
             (lookfrom (v:vec3 10 2 3))
;             (lookfrom (v:vec3 0 2 5))
             (lookat (v:vec3 0 0 0))
             (aperture 0)
             (dist-to-focus 1)
             (camera (make-camera lookfrom
                                  lookat
                                  (v:vec3 0 1 0)
                                  20 (/ nx ny)
                                  aperture
                                  dist-to-focus 0 1))
             (obj-list
;              (random-scene)
              test-scene
              ))
        (with-output-to-file "test.ppm"
          (lambda ()
            (display (format "P3\n ~D ~D\n255\n" nx ny))
            (dotimes (y ny)
                     (dotimes (x nx)
                              (let* ((c (correct-gamma (calc-pixel-color x (- ny y) nx ny camera
                                                                         obj-list ns)))
                                     (ir (floor->exact (* 255.99 (v:x c))))
                                     (ig (floor->exact (* 255.99 (v:y c))))
                                     (ib (floor->exact (* 255.99 (v:z c)))))
                                (display (format "~D ~D ~D\n"
                                                 ir ig ib)))))))))

