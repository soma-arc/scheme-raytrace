(define-module geometry
  (use gauche.record)
  (use vec :prefix v:)
  (use ray)
  (use material :prefix m:)
  (export-all))

(select-module geometry)

(define (hit obj r t-min t-max)
  ((vector-ref obj 0) r t-min t-max))

(define (make-sphere center radius material)
  (vector (lambda (r t-min t-max)
            (let* ((oc (v:diff (origin r) center))
                   (a (v:dot (dir r) (dir r)))
                   (b (v:dot oc (dir r)))
                   (c (- (v:dot oc oc) (* radius radius)))
                   (discriminant (- (* b b) (* a c))))
              (if (<= discriminant 0)
                  (values #f #f)
                  (let ((temp (/ (- (- b) (sqrt discriminant))
                                 a)))
                    (if (< t-min temp t-max)
                        (let* ((p (point-at-parameter r temp))
                               (normal (v:scale (v:diff p center)
                                                (/ 1 radius))))
                          (values #t (make-hit-record temp p normal material)))
                        (let ((temp (/ (+ (- b) (sqrt discriminant))
                                       a)))
                          (if (< t-min temp t-max)
                              (let* ((p (point-at-parameter r temp))
                                     (normal (v:scale (v:diff p center)
                                                      (/ 1 radius))))
                                (values #t (make-hit-record temp p normal material)))
                              (values #f #f))))))))
          center radius material))

(define-inline (center sphere)
  (vector-ref sphere 1))

(define-inline (radius sphere)
  (vector-ref sphere 2))

(define-inline (material sphere)
  (vector-ref sphere 3))

(define (calc-nearest obj-list r t-min t-max)
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

