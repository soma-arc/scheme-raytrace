(add-load-path "." :relative)

(define-module main
  (use vec :prefix v:)
  (use srfi-11)
  (use srfi-27)
  (use srfi-43)
  (use math.const)
  (use util)
  (use ray)
  (use constant)
  (use geometry :prefix g:)
  (use material :prefix m:)
  (use texture :prefix t:)
  (use camera :prefix cam:)
  (use gauche.threads)
  (use gauche.uvector)
  (use bezier :prefix b:)
  (use points :prefix p:)
  (use gl)
  (use gl.glut)
  (use gauche.time))

(select-module main)

(define +max-depth+ 100)

(define +black+ (v:vec3 0 0 0))
(define +white+ (v:vec3 1 1 1))

(define (random-scene)
  (let ((n 300)
        (obj-list '())
        (checker (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
                                    (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
    (push! obj-list
           (g:make-sphere
            (v:vec3 0 -1000 0) 1000
            (m:make-lambertian
             checker)))
    (let loop-a ((a -5))
      (if (< a 10)
          (let loop-b ((b -5))
            (if (< b 10)
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
                                 (t:constant-texture
                                  (v:vec3 (* (random-real) (random-real))
                                          (* (random-real) (random-real))
                                          (* (random-real) (random-real))))))))
                       ((< choose-mat 0.95)
                        (push! obj-list
                               (g:make-sphere
                                center 0.2
                                (m:make-metal
                                 (t:constant-texture
                                  (v:vec3 (* 0.5 (+ 1 (random-real)))
                                          (* 0.5 (+ 1 (random-real)))
                                          (* 0.5 (+ 1 (random-real)))))
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
            (m:make-lambertian (t:constant-texture (v:vec3 0.4 0.2 0.1)))))
    (push! obj-list
           (g:make-sphere
            (v:vec3 4 1 0) 1
            (m:make-metal (t:constant-texture (v:vec3 0.7 0.6 0.5)) 0)))
    (g:make-scene obj-list)))

(define (sky-color ray)
  (let* ((unit-dir (v:unit (dir ray)))
         (t (* 0.5 (+ 1.0 (v:y unit-dir)))))
    (v:sum (v:scale +white+ (- 1 t))
           (v:scale (v:vec3 0.5 0.7 1.0) t))))

(define (black ray)
  +black+)

(define (color r scene)
  (letrec ((col
            (lambda (r scene depth)
              (receive (hit? hit-rec)
                       (g:hit scene r 0.001 +max-float+)
                       (if hit?
                           (let*-values (((valid? scattered attenuation pdf)
                                          (m:scatter (material hit-rec) r hit-rec))
                                         ((emitted)
                                          (m:emitted (material hit-rec)
                                                     r hit-rec
                                                     (u hit-rec) (v hit-rec) (p hit-rec))))
                             (if (and (< depth +max-depth+) valid?)
                                 (v:sum emitted
                                        (v:scale (v:prod (v:scale attenuation
                                                                  (m:scattering-pdf (material hit-rec)
                                                                                    r hit-rec scattered))
                                                         (col scattered scene (+ 1 depth)))
                                                 (/ 1 pdf)))
                                 emitted))
                           ((g:scene-sky-function scene) r))))))
    (col r scene 0)))

(define-inline (correct-gamma c)
  (v:vec3 (sqrt (v:x c)) (sqrt (v:y c)) (sqrt (v:z c))))

(define *size-x* 200)
(define *size-y* 200)

(define *cornell-camera*
  (let ((lookfrom (v:vec3 278 278 -800))
        (lookat (v:vec3 278 278 0))
        (aperture 0)
        (dist-to-focus 1))
    (cam:make-camera lookfrom
                     lookat
                     (v:vec3 0 1 0)
                     40 (/ *size-x* *size-y*)
                     aperture
                     dist-to-focus 0 1)))

(define *camera*
  (let ((lookfrom (v:vec3 0 5 5))
;        (lookfrom (v:vec3 10 5 3))
;        (lookfrom (v:vec3 0 2 5))
        (lookat (v:vec3 0 0 0))
        (aperture 0)
        (dist-to-focus 1))
    (cam:make-camera lookfrom
                     lookat
                     (v:vec3 0 1 0)
                     40 (/ *size-x* *size-y*)
                     aperture
                     dist-to-focus 0 1)))

(define test-scene
  (g:make-scene
   (list (g:make-sphere (v:vec3 0 0 -1) 0.5
                        (m:make-lambertian
                         (t:constant-texture (v:vec3 0.1 0.2 0.5))))
         (g:make-sphere (v:vec3 0 -100.5 -1) 100
                        (m:make-lambertian
                         (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
                                            (t:constant-texture (v:vec3 0.9 0.9 0.9)))
                         ))
         (g:make-sphere (v:vec3 1 0 -1) 0.5
                        (m:make-metal
                         (t:constant-texture (v:vec3 0.8 0.6 0.2))
                         0.3))
         (g:make-sphere (v:vec3 -1 0 -1) 0.5
                        (m:make-dielectric 1.5))
         (g:make-sphere (v:vec3 -1 0 -1) -0.45
                        (m:make-dielectric 1.5)))
   *camera*
   black))


(define (line-upped-spheres nx ny)
  (let ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
        (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
        (blue (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.15 0.45))))
	(obj-list '()))
    (dotimes (x nx)
	     (dotimes (y ny)
		      (push! obj-list
			     (g:make-sphere (v:vec3 x 0 y) 0.5
					    (m:make-lambertian
					     (t:constant-texture (v:vec3 (random-real)
									 (random-real)
									 (random-real))))))))
    obj-list))

(define *spheres-list*
  (line-upped-spheres 10 10))

(define *bvh-node*
  (g:make-bvh-node
   *spheres-list* 0 0))

(define test-scene-non-bvh
  (g:make-scene
   (apply list
	  (g:make-sphere (v:vec3 0 -100.5 -1) 100
			 (m:make-lambertian
			  (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
					     (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
	  *spheres-list*)
   *camera*
   sky-color))

(define test-scene-bvh
  (g:make-scene
   (list
    (g:make-sphere (v:vec3 0 -100.5 -1) 100
		   (m:make-lambertian
		    (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
				       (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
    *bvh-node*)
   *camera*
   sky-color))

(define test-bezier
  (let ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
        (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
        (blue (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.15 0.45)))))
    (g:make-scene
     (list
      (g:make-sphere (v:vec3 0 -100.5 -1) 100
                     (m:make-lambertian
                      (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
                                         (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
      (g:make-bvh-node (list
                        (g:make-sphere (v:vec3 2 0 2) 0.5 red)
                        (g:make-sphere (v:vec3 -2 0 -2) 0.5 green)
                        (g:make-sphere (v:vec3 -1 0 -1) 0.1
                                       blue)
                        (g:make-sphere (v:vec3 -0.8 1 1) 0.1
                                       blue)
                        (g:make-sphere (v:vec3 0.8 -1 1) 0.1
                                       blue)
                        (g:make-sphere (v:vec3 1 0 -1) 0.1
                                       blue)
                        (b:make-bezier (v:vec3 -1   0 -1)
                                          (v:vec3 -0.8 1 1)
                                          (v:vec3 0.8  -1 1)
                                          (v:vec3 1    0 -1)
                                          0.1 red)
                        (b:make-bezier (v:vec3 -1   0 1)
                                       (v:vec3 -0.8 1 -1)
                                       (v:vec3 0.8  -1 -1)
                                       (v:vec3 1    0 1)
                                       0.1 red)
                        (b:make-bezier (v:vec3 -1   0 2)
                                       (v:vec3 -0.8 1 -2)
                                       (v:vec3 0.8  -1 -2)
                                       (v:vec3 1    0 2)
                                       0.1 red))
                     0 0)
      )
     *camera*
     sky-color)))

;; (define limitset-scene
;;   (let* ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
;;          (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
;;          (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
;;          (blue (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.15 0.45))))
;;          (bez-list
;;           (p:bezier->objs (p:points->bezier (p:load-points "2-2.csv" 5))
;;                           0.1 red)))
;;     (g:make-scene
;;      (list
;;       ;; (g:make-sphere (v:vec3 0 -100.5 -1) 100
;;       ;;                (m:make-lambertian
;;       ;;                 (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
;;       ;;                                    (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
;;       (g:make-bvh-node bez-list 0 0))
;;      *camera*
;;      sky-color)))

(define test-bezier-points
  (let* ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
         (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
         (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
         (blue (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.15 0.45))))
         (bez  (p:bezier->objs (p:points->bezier (p:load-points "points.csv" 1))
                               0.1 red)))
    (g:make-scene
     (list
            (g:make-sphere (v:vec3 0 -100.5 -1) 100
                     (m:make-lambertian
                      (t:checker-texture (t:constant-texture (v:vec3 0.2 0.3 0.1))
                                         (t:constant-texture (v:vec3 0.9 0.9 0.9)))))
            ;bez
            (g:make-bvh-node bez 0 0)
            )
     *camera*
     sky-color)))

(define test-scene2
  (let ((per-tex (t:marble-texture 1)))
    (g:make-scene
     (list (g:make-sphere (v:vec3 0 -1000 -1) 1000
                          (m:make-lambertian per-tex))
           (g:make-sphere (v:vec3 0 2 0) 2
                          (m:make-lambertian per-tex))
           (g:make-sphere (v:vec3 0 7 0) 2
                          (m:make-diffuse-light (t:constant-texture (v:vec3 4 4 4))))
           (g:make-xy-rect 3 5 1 3 -2
                           (m:make-diffuse-light (t:constant-texture (v:vec3 4 4 4)))))
     *camera*
     black)))

(define cornell-box
  (let ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
        (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
        (light (m:make-diffuse-light (t:constant-texture (v:vec3 3 3 3)))))
    (g:make-scene
     (list (g:flip-normals (g:make-yz-rect 0 555 0 555 555 green))
           (g:make-yz-rect 0 555 0 555 0 red)
           (g:flip-normals (g:make-xz-rect 213 343 227 332 554 light))
                                        ;             (g:flip-normals (g:make-xz-rect 113 443 127 432 554 light))
           (g:flip-normals (g:make-xz-rect 0 555 0 555 555 white))
           (g:make-xz-rect 0 555 0 555 0 white)
           (g:flip-normals (g:make-xy-rect 0 555 0 555 555 white))
           (g:translate (g:rotate-y (g:make-box (v:vec3 0 0 0) (v:vec3 165 165 165) white)
                                    -18)
                        (v:vec3 130 0 65))
           (g:translate (g:rotate-y (g:make-box (v:vec3 0 0 0) (v:vec3 165 330 165) white)
                                    15)
                        (v:vec3 265 0 295))
           )
     *cornell-camera*
     sky-color)))

(define cornell-bezier
  (let ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
        (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
        (light (m:make-diffuse-light (t:constant-texture (v:vec3 3 3 3)))))
    (g:make-scene
     (list (g:flip-normals (g:make-yz-rect 0 555 0 555 555 green))
           (g:make-yz-rect 0 555 0 555 0 red)
           (g:flip-normals (g:make-xz-rect 213 343 227 332 554 light))
                                        ;             (g:flip-normals (g:make-xz-rect 113 443 127 432 554 light))
           (g:flip-normals (g:make-xz-rect 0 555 0 555 555 white))
           (g:make-xz-rect 0 555 0 555 0 white)
           (g:flip-normals (g:make-xy-rect 0 555 0 555 555 white))
           (b:make-bezier (v:vec3 130 0 65)
                          (v:vec3 150 0 190)
                          (v:vec3 130 0 190)
                          (v:vec3 265 0 295)
                          10 red)
           )
     *cornell-camera*
     sky-color)))

(define cornell-smoke
  (let* ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
         (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
         (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
         (light (m:make-diffuse-light (t:constant-texture (v:vec3 3 3 3))))
         (b1 (g:translate (g:rotate-y (g:make-box (v:vec3 0 0 0) (v:vec3 165 165 165) white)
                                      -18)
                          (v:vec3 130 0 65)))
         (b2 (g:translate (g:rotate-y (g:make-box (v:vec3 0 0 0) (v:vec3 165 330 165) white)
                                      15)
                          (v:vec3 265 0 295))))
    (g:make-scene
     (list (g:flip-normals (g:make-yz-rect 0 555 0 555 555 green))
           (g:make-yz-rect 0 555 0 555 0 red)
           ;(g:flip-normals (g:make-xz-rect 213 343 227 332 554 light))
           (g:flip-normals (g:make-xz-rect 113 443 127 432 554 light))
           (g:flip-normals (g:make-xz-rect 0 555 0 555 555 white))
           (g:make-xz-rect 0 555 0 555 0 white)
           (g:flip-normals (g:make-xy-rect 0 555 0 555 555 white))
           (g:make-constant-medium b1 0.01 (t:constant-texture (v:vec3 1 1 1)))
           (g:make-constant-medium b2 0.01 (t:constant-texture (v:vec3 0 0 0)))
           )
     *cornell-camera*
     black)))

(define klein-scene
  (let ((white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05)))))
    (g:make-scene
     (list (g:make-sphere (v:vec3 0 -1003 -1) 1000 white)
           (g:make-klein (v:vec3 0 2 0) red))
     *camera*
     sky-color)))

(define cornell-klein
  (let ((red (m:make-lambertian (t:constant-texture (v:vec3 0.65 0.05 0.05))))
        (blue (m:make-lambertian (t:constant-texture (v:vec3 0.05 0.65 0.65))))
        (white (m:make-lambertian (t:constant-texture (v:vec3 0.73 0.73 0.73))))
        (green (m:make-lambertian (t:constant-texture (v:vec3 0.12 0.45 0.15))))
        (light (m:make-diffuse-light (t:constant-texture (v:vec3 3 3 3)))))
      (g:make-scene
       (list (g:flip-normals (g:make-yz-rect 0 555 0 555 555 green))
             (g:make-yz-rect 0 555 0 555 0 red)
;             (g:flip-normals (g:make-xz-rect 213 343 227 332 554 light))
             (g:flip-normals (g:make-xz-rect 113 443 127 432 554 light))
             (g:flip-normals (g:make-xz-rect 0 555 0 555 555 white))
             (g:make-xz-rect 0 555 0 555 0 white)
             (g:flip-normals (g:make-xy-rect 0 555 0 555 555 white))
             (g:make-klein (v:vec3 250 200 280) blue)
             )
       *cornell-camera*
       sky-color)))

(define *tex* #f)
(define *image* (make-u8vector (* *size-x* *size-y* 3) 0))
(define *raw-data* (make-vector (* *size-x* *size-y*) 0))
(define *current-y* 0)

(define *max-sample* 20)

(define *rendering?* #f)

(define *scene* limitset-scene)

(define (save-as-ppm nx ny)
  (with-output-to-file "test.ppm"
    (lambda ()
      (display (format "P3\n ~D ~D\n255\n" nx ny))
      (dotimes (y ny)
               (dotimes (x nx)
                        (let* ((i (* (+ (* (- ny y 1) nx) x) 3))
                               (ir (u8vector-ref *image* i))
                               (ig (u8vector-ref *image* (+ i 1)))
                               (ib (u8vector-ref *image* (+ i 2))))
                          (display (format "~D ~D ~D\n"
                                           ir ig ib))))))))

(define (trace-line scene y sample-count)
  (dotimes (x *size-x*)
           (let* ((i (* (+ (* y *size-x*) x) 3))
                  (j (+ (* y *size-x*) x))
                  (col (let* ((u (/ (+ x (random-real)) *size-x*))
                              (v (/ (+ y (random-real)) *size-y*))
                              (ray (cam:get-ray (g:scene-camera scene) u v)))
                         (color ray scene)))
                  (sum-color (v:sum (vector-ref *raw-data* j) col))
                  (col (correct-gamma
                        (v:quot sum-color (v:vec3 sample-count sample-count sample-count))))
                  (ir (floor->exact (* 255.99 (min 1 (v:x col)))))
                  (ig (floor->exact (* 255.99 (min 1 (v:y col)))))
                  (ib (floor->exact (* 255.99 (min 1 (v:z col))))))
             (vector-set! *raw-data* j sum-color)
             (u8vector-set! *image* i       ir)
             (u8vector-set! *image* (+ i 1) ig)
             (u8vector-set! *image* (+ i 2) ib))))

(define (trace-all scene sample-count)
  (dotimes (y *size-y*)
           (dotimes (x *size-x*)
                    (let* ((i (* (+ (* y *size-x*) x) 3))
                           (j (+ (* y *size-x*) x))
                           (col (let* ((u (/ (+ x (random-real)) *size-x*))
                                       (v (/ (+ y (random-real)) *size-y*))
                                       (ray (cam:get-ray (g:scene-camera scene) u v)))
                                  (color ray scene)))
                           (sum-color (v:sum (vector-ref *raw-data* j) col))
                           (col (correct-gamma
                                 (v:quot sum-color (v:vec3 sample-count
                                                           sample-count
                                                           sample-count))))
                           (ir (floor->exact (* 255.99 (min 1 (v:x col)))))
                           (ig (floor->exact (* 255.99 (min 1 (v:y col)))))
                           (ib (floor->exact (* 255.99 (min 1 (v:z col))))))
                      (vector-set! *raw-data* j sum-color)
                      (u8vector-set! *image* i       ir)
                      (u8vector-set! *image* (+ i 1) ig)
                      (u8vector-set! *image* (+ i 2) ib)))))

(define (set-texture)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  (set! *tex* (u32vector-ref (gl-gen-textures 1) 0))
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGB *size-x* *size-y* 0
                   GL_RGB GL_UNSIGNED_BYTE *image*))

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-enable GL_TEXTURE_2D)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-push-matrix)
  (gl-load-identity)
  (gl-begin GL_QUADS)
  (gl-tex-coord '#f32(0.0 0.0)) (gl-vertex '#f32(0.0 0.0))
  (gl-tex-coord '#f32(0.0 1.0)) (gl-vertex '#f32(0.0 1.0))
  (gl-tex-coord '#f32(1.0 1.0)) (gl-vertex '#f32(1.0 1.0))
  (gl-tex-coord '#f32(1.0 0.0)) (gl-vertex '#f32(1.0 0.0))
  (gl-end)
  (gl-pop-matrix)
  (glut-swap-buffers)
  (gl-disable GL_TEXTURE_2D)
  (animate))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0 1 0 1)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity))

(define *sample-count* 1)

(define (animate)
  (if *rendering?*
      (if (< *current-y* *size-y*)
          (begin
            (trace-line *scene* *current-y* *sample-count*)
            (set-texture)
            (inc! *current-y*))
          (begin
            (inc! *sample-count*)
            (set! *current-y* 0)
            (glut-set-window-title (format "sample - ~D" *sample-count*)))))
  (glut-post-redisplay))

(define (key k x y)
  (let1 q (lambda () (glut-post-redisplay))
    (cond
     ((= k (char->integer #\z))
      (set! *rendering?* (not *rendering?*)) (q))
     ((= k (char->integer #\S))
      (save-as-ppm *size-x* *size-y*) (q))
     ((= k (char->integer #\escape)) (exit)))))

(define (mouse button state x y)
  (cond
    ((= button GLUT_LEFT_BUTTON)
     (when (= state GLUT_DOWN) (display (format "(~A,~A)~%" x (- 199 y))
                                        (current-error-port))))
    )
  )

(define (start-glut)
  (glut-init '())
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB))
  (glut-init-window-size *size-x* *size-y*)
  (glut-create-window (format "sample - ~D" *sample-count*))
  (set-texture)
  (glut-reshape-func reshape)
  (glut-display-func disp)
  (glut-keyboard-func key)
  (glut-mouse-func mouse)
  (glut-main-loop))

(define *bez*
  (b:make-bezier (v:vec3 10 10 0)
                 (v:vec3 30 100 0)
                 (v:vec3 160 180 0)
                 (v:vec3 180 100 0)
                 0.1
                 #f))

(define (draw-bezier bezier r g b)
  (let loop ((t 0))
    (if (< t 1)
        (let* ((p (b:bezier-point bezier t))
               (x (+ (/ *size-x* 2) (floor->exact (v:x p))))
               (y (+ (/ *size-y* 2) (floor->exact (v:y p))))
               (i (* (+ (* y *size-x*) x) 3)))
          (display (format "~A, ~A~%" x y))
          (u8vector-set! *image* i       r)
          (u8vector-set! *image* (+ i 1) g)
          (u8vector-set! *image* (+ i 2) b)
          (loop (+ t 0.01))))))

(define (draw-tan-vec bezier t r g b)
  (let* ((tan-vec (b:bezier-tan-vec bezier t))
         (p (b:bezier-point bezier t)))
    (let loop ((param 0))
      (if (< param 50)
          (let* ((vp (v:sum p (v:scale tan-vec param)))
                 (x (max (floor->exact (v:x vp)) 0))
                 (y (max (floor->exact (v:y vp)) 0))
                 (i (* (+ (* y *size-x*) x) 3)))
            (u8vector-set! *image* i       r)
            (u8vector-set! *image* (+ i 1) g)
            (u8vector-set! *image* (+ i 2) b)
            (loop (+ param 0.1)))))))

(define-macro (profiling expr)
  `(dynamic-wind
       profiler-start
       (lambda () ,expr)
       profiler-stop))

;; (let ((r (cam:get-ray *camera* 0.3 0.3))
;;       (y 20)
;;       (x 20))
;;   (display "non bvh")
;;   (profiler-reset)
;;   (profiling (g:hit test-scene-non-bvh r 0.001 +max-float+))
;;   (profiler-show)
;;   (display "bvh")
;;   (profiler-reset)
;;   (profiling (g:hit test-scene-bvh r 0.001 +max-float+))
;;   (profiler-show)
;;    )

(define *gl-thread* (make-thread (cut start-glut)))
;(thread-start! *gl-thread*)

;; bvh
;(time (trace-all test-scene-bvh 1))


;;non bvh
;(time (trace-all test-scene-non-bvh 1))

