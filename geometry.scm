(define-module geometry
  (use gauche.record)
  (use vec :prefix v:)
  (use ray)
  (use material :prefix m:)
  (export-all))

(select-module geometry)

(define (hit obj r t-min t-max)
  ((vector-ref obj 0) r t-min t-max))

(define (bounding-box obj t0 t1)
  ((vector-ref obj 1) t0 t1))

(define-inline (material obj)
  (vector-ref obj 2))

(define-inline (aabb-min aabb)
  (vector-ref aabb 1))

(define-inline (aabb-max aabb)
  (vector-ref aabb 2))

(define (make-aabb box-min box-max)
  (vector (lambda (r t-min t-max)
            (let loop ((axis 0))
              (if (= axis 3)
                  #t
                  (let* ((1/dir (/ 1 (v:vec3-ref (dir r) axis)))
                         (t0 (min (* (- (v:vec3-ref box-min axis)
                                        (v:vec3-ref (origin r) axis))
                                     1/dir)
                                  (* (- (v:vec3-ref box-max axis)
                                        (v:vec3-ref (origin r) axis))
                                     1/dir)))
                         (t1 (max (* (- (v:vec3-ref box-min axis)
                                        (v:vec3-ref (origin r) axis))
                                     1/dir)
                                  (* (- (v:vec3-ref box-max axis)
                                        (v:vec3-ref (origin r) axis))
                                     1/dir)))
                         (t-min (max t0 t-min))
                         (t-max (min t1 t-max)))
                    (if (<= t-max t-min)
                        #f
                        (loop (inc! axis)))))))
          box-min box-max))

(define (surrounding-box box0 box1)
  (make-aabb (v:vec3 (min (v:x (aabb-min box0)) (v:x (aabb-min box1)))
                     (min (v:y (aabb-min box0)) (v:y (aabb-min box1)))
                     (min (v:z (aabb-min box0)) (v:z (aabb-min box1))))
             (v:vec3 (max (v:x (aabb-max box0)) (v:x (aabb-max box1)))
                     (max (v:y (aabb-max box0)) (v:y (aabb-max box1)))
                     (max (v:z (aabb-max box0)) (v:z (aabb-max box1))))))

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
          (lambda (t0 t1)
            (values #t (make-aabb (v:diff center (v:vec3 radius radius radius))
                                  (v:sum center (v:vec3 radius radius radius)))))
          material center radius))

(define (make-moving-sphere center0 center1 time0 time1 radius material)
  (let ((center (lambda (time)
                  (v:sum center0
                         (v:scale (v:diff center1 center0)
                                  (/ (- time time0)
                                     (- time1 time0)))))))
    (vector (lambda (r t-min t-max)
              (let* ((current-center (center (ray-time r)))
                     (oc (v:diff (origin r) current-center))
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
                                 (normal (v:scale (v:diff p current-center)
                                                  (/ 1 radius))))
                            (values #t (make-hit-record temp p normal material)))
                          (let ((temp (/ (+ (- b) (sqrt discriminant))
                                         a)))
                            (if (< t-min temp t-max)
                                (let* ((p (point-at-parameter r temp))
                                       (normal (v:scale (v:diff p current-center)
                                                        (/ 1 radius))))
                                  (values #t (make-hit-record temp p normal material)))
                                (values #f #f))))))))
            (lambda (t0 t1)
              (values #t (surrounding-box
                          (make-aabb (v:diff (center t0) (v:vec3 radius radius radius))
                                     (v:sum (center t0) (v:vec3 radius radius radius)))
                          (make-aabb (v:diff (center t1) (v:vec3 radius radius radius))
                                     (v:sum (center t1) (v:vec3 radius radius radius))))))
            material center0 center1 time0 time1 radius)))

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

