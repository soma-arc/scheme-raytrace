(define-module camera
  (use vec :prefix v:)
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
   (v :init-keyword :v :accessor v)))

(define (cam lookfrom lookat vup vfov aspect)
  (let* ((theta (* vfov pi/180))
         (half-height (tan (/ theta 2)))
         (half-width (* aspect half-height))
         (w (v:unit (v:diff lookfrom lookat)))
         (u (v:unit (v:cross vup w)))
         (v (v:cross w u)))
    (make <camera>
      :lower-left-corner (v:diff lookfrom
                                 (v:scale u half-width)
                                 (v:scale v half-height)
                                 w)
      :horizontal (v:scale u (* 2 half-width))
      :vertical (v:scale v (* 2 half-height))
      :origin lookfrom
      :w w
      :u u
      :v v)))

(define (cam-with-lens lookfrom lookat vup vfov aspect aperture focus-dist)
  (let* ((theta (* vfov pi/180))
         (half-height (tan (/ theta 2)))
         (half-width (* aspect half-height))
         (w (v:unit (v:diff lookfrom lookat)))
         (u (v:unit (v:cross vup w)))
         (v (v:cross w u)))
    (make <camera>
      :lower-left-corner (v:diff lookfrom
                                 (v:scale u (* half-width focus-dist))
                                 (v:scale v (* half-height focus-dist))
                                 (v:scale w focus-dist))
      :horizontal (v:scale u (* 2 half-width focus-dist))
      :vertical (v:scale v (* 2 half-height focus-dist))
      :origin lookfrom
      :lens-radius (/ aperture 2)
      :w w
      :u u
      :v v)))

(define (get-ray camera u v)
  (make-ray (origin camera)
            (v:diff (v:sum (lower-left-corner camera)
                           (v:scale (horizontal camera) u)
                           (v:scale (vertical camera) v))
                    (origin camera))))

(define (get-ray-with-lens camera s t)
  (let* ((rd (v:scale (random-in-unit-disk) (lens-radius camera)))
         (offset (v:sum (v:scale (u camera) (v:x rd))
                        (v:scale (v camera) (v:y rd)))))
    (make-ray (v:sum (origin camera) offset)
              (v:diff (v:sum (lower-left-corner camera)
                             (v:scale (horizontal camera) s)
                             (v:scale (vertical camera) t))
                      (origin camera)
                      offset))))
