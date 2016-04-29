(define-module bezier
  (use srfi-11)
  (use ray)
  (use gauche.array)
  (use vec :prefix v:)
  (use constant)
  (use geometry :prefix g:)
  (use math.const)
  (export-all))

(select-module bezier)

(define (get-projection-mat ray)
  (let* ((ray-origin (origin ray))
         (ray-dir (dir ray))
         (ox (- (v:x ray-origin)))
         (oy (- (- (v:z ray-origin))))
         (oz (- (v:y ray-origin)))
         (ray-dir (v:unit ray-dir))
         (lx (v:x ray-dir))
         (ly (- (v:z ray-dir)))
         (lz (v:y ray-dir))
         (d (sqrt (+ (* lx lx) (* lz lz))))
         (rotation (if (= d 0)
                       (let ((angle (if (>= ly 0)
                                        (- pi/2)
                                        pi/2)))
                         (array (shape 0 4 0 4)
                                1 0 0 0
                                0 (cos angle) (- (sin angle)) 0
                                0 (sin angle) (cos angle) 0
                                0 0 0 1))
                       (array (shape 0 4 0 4)
                              (/ lz d) (/ (* -1 lx ly) d) lx 0
                              0 d ly 0
                              (/ (- lx) d) (/ (* -1 ly lz) d) lz 0
                              0 0 0 1))))
    (array-mul (array (shape 0 4 0 4)
                      1 0 0 0
                      0 1 0 0
                      0 0 1 0
                      ox oy oz 1)
               rotation)))

(define (internally-divide a b t)
  (v:sum (v:scale a (- 1 t))
         (v:scale b t)))

(define (transform p mat)
  (let ((t (array-mul (array (shape 0 1 0 4)
                             (v:x p) (- (v:z p)) (v:y p) 1)
                      mat)))
    (v:vec3 (array-ref t 0 0)
            (array-ref t 0 1)
            (array-ref t 0 2))))

(define (make-bezier a b c d width material)
  (letrec* ((cp (vector a b c d))
            (n 4)
            (width1 (/ width 2))
            (width2 (* width1 width1))
            (eps (/ width1 20))
            (bez-p
             (lambda (t)
               (let* ((t2 (* t t))
                      (t3 (* t2 t))
                      (1-t (- 1 t))
                      (1-t2 (* 1-t 1-t))
                      (1-t3 (* 1-t2 1-t)))
                 (v:sum (v:scale a 1-t3)
                        (v:scale b (* 3 1-t2 t))
                        (v:scale c (* 3 1-t t2))
                        (v:scale d t3)))))
            (split
             (lambda (t)
               (let* ((split-point (bez-p t))
                      (nbc (internally-divide b c t))
                      (lb (internally-divide a b t))
                      (lc (internally-divide lb nbc t))
                      (rc (internally-divide c d t))
                      (rb (internally-divide nbc rc t)))
                 (values (make-bezier a lb lc split-point width material)
                         (make-bezier split-point rb rc d width material)))))
            (bbox
             (lambda (t0 t1)
               (let* ((min-vec (v:vec3 +max-float+ +max-float+ +max-float+))
                      (max-vec (v:vec3 (- +max-float+) (- +max-float+) (- +max-float+))))
                 (dotimes (i 4)
                          (dotimes (axis 3)
                                   (if (< (v:vec3-ref (vector-ref cp i) axis)
                                          (v:vec3-ref min-vec axis))
                                       (v:vec3-set! min-vec axis (v:vec3-ref (vector-ref cp i) axis)))
                                   (if (> (v:vec3-ref (vector-ref cp i) axis)
                                          (v:vec3-ref max-vec axis))
                                       (v:vec3-set! max-vec axis (v:vec3-ref (vector-ref cp i) axis)))))
                 (g:make-aabb min-vec max-vec))))
            (bezier-transform
             (lambda (mat)
               (make-bezier (transform a mat)
                            (transform b mat)
                            (transform c mat)
                            (transform d mat)
                            width material)))
            (bez-tan-vec
             (lambda (t)
               (let ((t2 (* t t))
                     (coef-a (v:sum (v:scale b 3)
                               d
                               (v:scale c -3)
                               (v:scale a -1)))
                     (coef-b (v:scale (v:sum a (v:scale b -2) c) 3))
                     (coef-c (v:scale (v:diff b a) 3)))
                 (v:unit (v:sum (v:scale coef-a (* 3 t2))
                                (v:scale coef-b (* 2 t))
                                coef-c)))))
            (to-string
             (lambda ()
               (format "~A ~%~A ~%~A ~%~A~%" a b c d)))
            (converge
             (lambda (depth c v0 vn t)
               (let* ((b (g:bounding-box c 0 0))
                      (b-min (g:aabb-min b))
                      (b-max (g:aabb-max b)))
                 (cond ((or (>= (v:z b-min) t) ;(<= (v:z b-max) 0.0001);(<= (v:z b-max) k-wps)
                            (>= (v:x b-min) width1) (<= (v:x b-max) (- width1))
                            (>= (v:y b-min) width1) (<= (v:y b-max) (- width1)))
                        (values #f #f))
                       ((= depth 0)
  ;                      (display "in depth0\n")
                        (let* ((dir (v:diff (bezier-cp c 3) (bezier-cp c 0)))
                               (dp0 (bezier-tan-vec c 0))
                               (dp0 (if (< (v:dot dir dp0) 0)
                                        (v:scale dp0 -1)
                                        dp0))
                               (dpn #f)
                               (w #f)
                               (v #f)
                               (p #f))
                          (if (< (v:dot dp0 (v:scale (bezier-cp c 0) -1)) 0)
                              (begin
 ;                               (display "dp0 return\n")
                                (values #f #f))
                              (begin
                                (set! dpn (bezier-tan-vec c 1))
                                (set! dpn (if (< (v:dot dir dpn) 0)
                                              (v:scale dpn -1)
                                              dpn))
                                (if (< (v:dot dpn (bezier-cp c (- n 1))) 0)
                                    (begin
;                                      (display (display "dpn return\n"))
                                      (values #f #f))
                                    (begin
                                      (set! w (+ (* (v:x dir) (v:x dir))
                                                 (* (v:y dir) (v:y dir))))
                                      (if (= w 0)
                                          (begin
 ;                                           (display (display "w=0 return\n"))
                                            (values #f #f))
                                          (begin
                                            (set! w (/ (+ (* (v:x (bezier-cp c 0)) (v:x dir))
                                                          (* (v:y (bezier-cp c 0)) (v:y dir)))
                                                       (- w)))
                                            (set! w (clamp w 0 1))
                                            (set! v (+ (* v0 (- 1 w)) (* vn w)))
                                            (set! p (bezier-point c v))
                                            (if (or (>= (+ (* (v:x p) (v:x p))
                                                           (* (v:y p) (v:y p))) width2)
                                                    ;(<= (v:z p) 0.0001)
                                                    ;(< t (v:z p))
                                                    )
                                                (begin
;                                                  (display "not on return\n")
                                                  (values #f #f))
                                                (values #t (v:z p)))))))))))
                       (else (let*-values (((vm) (/ (+ v0 vn) 2))
                                           ((cl cr) (bezier-split c 0.5))
                                           ((hit? nt) (converge (- depth 1) cl v0 vm t)))
                               (if hit?
                                   (values hit? nt)
                                   (converge (- depth 1) cr vm vn t))))))))
            (hit (lambda (r t-min t-max)
                   (let*-values (((tr) (get-projection-mat r))
                                 ((transformed) (bezier-transform tr))
                                 ((max-depth) (let ((l0 (let loop ((m (- +max-float+)) (i 0))
                                                          (if (= i (- n 2))
                                                              m
                                                              (let ((x (abs (+ (v:x (bezier-cp transformed i))
                                                                               (* -2 (v:x (bezier-cp transformed (+ i 1))))
                                                                               (v:x (bezier-cp transformed (+ i 2))))))
                                                                    (y (abs (+ (v:y (bezier-cp transformed i))
                                                                               (* -2 (v:y (bezier-cp transformed (+ i 1))))
                                                                               (v:y (bezier-cp transformed (+ i 2)))))))
                                                                (loop (max x y m) (inc! i)))))))
                                                (ceiling->exact (log (/ (* (sqrt 2) n (- n 1) l0) (* 8 eps)) 4))))
                                 ((hit? t) (converge (+ 1 max-depth) transformed 0 1 t-max)))
                     ;; (display (format "max-depth ~A~%" max-depth))
                     ;; (display (format "dir ~A~%origin ~A~%" (dir r) (origin r)))
                     ;; (display (format "~A~%" (transform (v:vec3 0 0 0) tr)))
                     ;; (display (format "bezier~%~Atransformed~%~A"
                     ;;                  (to-string)
                     ;;                  (bezier-to-string transformed)))
                     ;; (display (format "dist~% a-b ~A~% b-c ~A~% c-d ~A~%"
                     ;;                  (v:length (v:diff a b))
                     ;;                  (v:length (v:diff b c))
                     ;;                  (v:length (v:diff c d))))
                     ;; (display (format "transformed dist~% a-b ~A~% b-c ~A~% c-d ~A~%~%"
                     ;;                  (bezier-compare transformed 0 1)
                     ;;                  (bezier-compare transformed 1 2)
                     ;;                  (bezier-compare transformed 2 3)))
                     (if (and hit? (< t-min t))
                         (values #t (make-hit-record t
                                                     (point-at-parameter r t)
                                                     (v:scale (dir r) -1)
                                                     material
                                                     0 0))
                         (values #f #f))))))
           (vector hit
                   bbox
                   cp
                   bez-p
                   split
                   bezier-transform
                   bez-tan-vec
                   to-string)))

(define (bezier-compare b i1 i2)
  (v:length (v:diff (bezier-cp b i1) (bezier-cp b i2))))

(define (bezier-cp bezier index)
  (vector-ref (vector-ref bezier 2) index))

(define (bezier-point bezier t)
  ((vector-ref bezier 3) t))

(define (bezier-split bezier t)
  ((vector-ref bezier 4) t))

(define (bezier-transform bezier mat)
  ((vector-ref bezier 5) mat))

(define (bezier-tan-vec bezier t)
  ((vector-ref bezier 6) t))

(define (bezier-to-string bezier)
  ((vector-ref bezier 7)))
