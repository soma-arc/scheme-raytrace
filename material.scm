(define-module material
  (use gauche.record)
  (use util)
  (use vec :prefix v:)
  (use ray)
  (use util)
  (export-all))

(select-module material)

(define-class <lambertian> ()
  ((albedo :init-value (v:vec3 1 1 1)
           :init-keyword :albedo
           :accessor albedo)))

(define-method scatter ((m <lambertian>) ray hit-rec)
  (let ((target (v:sum (p hit-rec)
                       (normal hit-rec)
                       (random-in-unit-sphere))))
    (values #t
            (make-ray (p hit-rec) (v:diff target (p hit-rec)))
            (albedo m))))

(define (reflect v n)
  (v:diff v
          (v:scale n (* 2 (v:dot v n)))))

(define-class <metal> ()
  ((albedo :init-value (v:vec3 1 1 1)
           :init-keyword :albedo
           :accessor albedo)
   (fuzz :init-value 1
         :init-keyword :fuzz
         :accessor fuzz)))

(define-method scatter ((m <metal>) ray hit-rec)
  (let* ((reflected (reflect (v:unit (dir ray)) (normal hit-rec)))
         (scattered (make-ray (p hit-rec)
                              (v:sum reflected
                                     (v:scale (random-in-unit-sphere)
                                              (fuzz m))))))
    (values (> (v:dot (dir scattered) (normal hit-rec)) 0)
            scattered
            (albedo m))))
