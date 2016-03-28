(add-load-path "." :relative)

(define-module geometry
  (use gauche.record)
  (use vec :prefix v:)
  (use ray)
  (export-all))

(select-module geometry)

(define-record-type hit-record #t #t
  (t t t-set!)
  (p p p-set!)
  (normal normal normal-set!))

(define-class <sphere> ()
  ((center :init-value (v:vec3 0 0 0)
           :init-keyword :center
           :accessor center)
   (radius :init-value 0
           :init-keyword :radius
           :accessor radius)))

(define-method hit ((s <sphere>) r t-min t-max)
  (let* ((oc (v:diff (origin r) (center s)))
         (a (v:dot (dir r) (dir r)))
         (b (v:dot oc (dir r)))
         (c (- (v:dot oc oc) (* (radius s) (radius s))))
         (discriminant (- (* b b) (* a c))))
    (if (<= discriminant 0)
        (values #f #f)
        (let ((temp (/ (- (- b) (sqrt discriminant))
                       a)))
          (if (< t-min temp t-max)
              (let* ((p (point-at-parameter r temp))
                     (normal (v:scale (v:diff p (center s))
                                      (/ 1 (radius s)))))
                (values #t (make-hit-record temp p normal)))
              (let ((temp (/ (+ (- b) (sqrt discriminant))
                             a)))
                (if (< t-min temp t-max)
                    (let* ((p (point-at-parameter r temp))
                           (normal (v:scale (v:diff p (center s))
                                            (/ 1 (radius s)))))
                      (values #t (make-hit-record temp p normal)))
                    (values #f #f))))))))

(define-method calc-nearest (obj-list r t-min t-max)
  (let loop ((obj-list obj-list)
             (hit-anything #f)
             (closest-so-far t-max)
             (rec #f))
    (if (null? obj-list)
        (values hit-anything rec)
        (receive (hit? hit-rec)
                 (hit (car obj-list) r t-min closest-so-far)
                 (if hit?
                     (loop (cdr obj-list)
                           #t
                           (t hit-rec)
                           hit-rec)
                     (loop (cdr obj-list)
                           hit-anything
                           closest-so-far
                           rec))))))
