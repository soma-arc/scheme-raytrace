(define-module camera
  (use vec :prefix v:)
  (use srfi-27)
  (use math.const)
  (use ray)
  (use util)
  (export-all))

(select-module camera)

(define-class <camera> ()
  ((lower-left-corner :init-value (v:vec3 -2 -1 -1)
                      :init-keyword :lower-left-corner
                      :accessor lower-left-corner)
   (horizontal :init-value (v:vec3 4 0 0)
               :init-keyword :horizontal
               :accessor horizontal)
   (vertical :init-value (v:vec3 0 2 0)
             :init-keyword :vertical
             :accessor vertical)
   (origin :init-value (v:vec3 0 0 0)
           :init-keyword :origin
           :accessor origin)
   (lens-radius :init-value 1
                :init-keyword :lens-radius
                :accessor lens-radius)
   (w :init-keyword :w :accessor w)
   (u :init-keyword :u :accessor u)
   (v :init-keyword :v :accessor v)
   (time0 :init-keyword :time0 :accessor time0)
   (time1 :init-keyword :time1 :accessor time1)))

(define-inline (lower-left-corner camera)
  (vector-ref camera 0))

(define-inline (horizontal camera)
  (vector-ref camera 1))

(define-inline (vertical camera)
  (vector-ref camera 2))

(define-inline (origin camera)
  (vector-ref camera 3))

(define-inline (w camera)
  (vector-ref camera 4))

(define-inline (u camera)
  (vector-ref camera 5))

(define-inline (v camera)
  (vector-ref camera 6))

(define-inline (lens-radius camera)
  (vector-ref camera 7))

(define-inline (time0 camera)
  (vector-ref camera 8))

(define-inline (time1 camera)
  (vector-ref camera 9))

(define (make-camera lookfrom lookat vup vfov aspect aperture focus-dist time0 time1)
  (let* ((theta (* vfov pi/180))
         (half-height (tan (/ theta 2)))
         (half-width (* aspect half-height))
         (w (v:unit (v:diff lookfrom lookat)))
         (u (v:unit (v:cross vup w)))
         (v (v:cross w u)))
    (vector
     (v:diff lookfrom
             (v:scale u (* half-width focus-dist))
             (v:scale v (* half-height focus-dist))
             (v:scale w focus-dist))
     (v:scale u (* 2 half-width focus-dist))
     (v:scale v (* 2 half-height focus-dist))
     lookfrom w u v
     (/ aperture 2) time0 time1)))

(define (get-ray camera s t)
  (let* ((rd (v:scale (random-in-unit-disk) (lens-radius camera)))
         (offset (v:sum (v:scale (u camera) (v:x rd))
                        (v:scale (v camera) (v:y rd))))
         (time (+ (time0 camera) (* (random-real) (- (time1 camera) (time0 camera))))))
    (make-ray-with-time
     (v:sum (origin camera) offset)
     (v:diff (v:sum (lower-left-corner camera)
                    (v:scale (horizontal camera) s)
                    (v:scale (vertical camera) t))
             (origin camera)
             offset)
     time)))
