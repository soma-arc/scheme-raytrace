(define-module camera
  (use vec :prefix v:)
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

(define (get-ray camera u v)
  (make-ray (origin camera)
            (v:diff (v:sum (lower-left-corner camera)
                           (v:scale (horizontal camera) u)
                           (v:scale (vertical camera) v))
                    (origin camera))))
