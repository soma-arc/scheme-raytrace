(define-module camera
  (use vec :prefix v:)
  (use math.const)
  (use ray)
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
           :accessor origin)))

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
      :origin lookfrom)))

(define (get-ray camera u v)
  (make-ray (origin camera)
            (v:diff (v:sum (lower-left-corner camera)
                           (v:scale (horizontal camera) u)
                           (v:scale (vertical camera) v))
                    (origin camera))))
