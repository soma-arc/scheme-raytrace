(define-module geometry
  (use srfi-11)
  (use srfi-27)
  (use math.const)
  (use constant)
  (use gauche.sequence)
  (use vec :prefix v:)
  (use ray)
  (use material :prefix m:)
  (export-all))

(select-module geometry)

(define (hit obj r t-min t-max)
  ((vector-ref obj 0) r t-min t-max))

(define-inline (scene-obj-list scene)
  (vector-ref scene 2))

(define (sub-scene scene start end)
  (let ((obj-list (scene-obj-list scene)))
    (make-scene (subseq obj-list start end))))

(define-inline (scene-num-obj scene)
  (length (scene-obj-list scene)))

(define-inline (scene-camera scene)
  (vector-ref scene 3))

(define-inline (scene-sky-function scene)
  (vector-ref scene 4))

(define (hit-obj-list obj-list r t-min t-max)
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

(define (make-scene obj-list camera sky-function)
  (vector
   (lambda (r t-min t-max)
     (hit-obj-list obj-list r t-min t-max))
   #f obj-list camera sky-function))

(define (bounding-box obj t0 t1)
  ((vector-ref obj 1) t0 t1))

(define-inline (material obj)
  (vector-ref obj 2))

(define-inline (aabb-min aabb)
  (vector-ref aabb 1))

(define-inline (aabb-max aabb)
  (vector-ref aabb 2))

(define-inline (aabb-center aabb)
  (vector-ref aabb 3))

(define (make-aabb box-min box-max)
  (let* ((min-x (v:x box-min))
	 (min-y (v:y box-min))
	 (min-z (v:z box-min))
	 (max-x (v:x box-max))
	 (max-y (v:y box-max))
	 (max-z (v:z box-max))
	 (box-center (v:scale (v:sum box-max box-min) 0.5))
	 (comp (lambda (bmin bmax rorigin 1/dir t-min t-max)
		 (let* ((t0 (min (* (- bmin rorigin)
				    1/dir)
				 (* (- bmax rorigin)
				    1/dir)))
			(t1 (max (* (- bmin rorigin)
				    1/dir)
				 (* (- bmax rorigin)
				    1/dir)))
			(t-min (max t0 t-min))
			(t-max (min t1 t-max)))
		   (<= t-max t-min)))))
    (vector (lambda (r t-min t-max)
	      (let ((1/dir-x (/ 1 (v:x (dir r))))
		    (1/dir-y (/ 1 (v:y (dir r))))
		    (1/dir-z (/ 1 (v:z (dir r))))
		    (origin-x (v:x (origin r)))
		    (origin-y (v:y (origin r)))
		    (origin-z (v:z (origin r))))
		(if (or (comp min-x max-x origin-x 1/dir-x t-min t-max)
			(comp min-y max-y origin-y 1/dir-y t-min t-max)
			(comp min-z max-z origin-z 1/dir-z t-min t-max))
		    (values #f #f)
		    (values #t #f))))
	    box-min box-max box-center)))

;; (define (surrounding-box box0 box1)
;;   (make-aabb (v:vec3 (min (v:x (aabb-min box0)) (v:x (aabb-min box1)))
;;                      (min (v:y (aabb-min box0)) (v:y (aabb-min box1)))
;;                      (min (v:z (aabb-min box0)) (v:z (aabb-min box1))))
;;              (v:vec3 (max (v:x (aabb-max box0)) (v:x (aabb-max box1)))
;;                      (max (v:y (aabb-max box0)) (v:y (aabb-max box1)))
;;                      (max (v:z (aabb-max box0)) (v:z (aabb-max box1))))))

(define (make-embty-box)
  (make-aabb
   (v:vec3 +max-float+ +max-float+ +max-float+)
   (v:vec3 (- +max-float+) (- +max-float+) (- +max-float+))))

(define-syntax surrounding-box
  (syntax-rules ()
    ((_ box0 box1)
     (make-aabb (v:vec3 (min (v:x (aabb-min box0)) (v:x (aabb-min box1)))
			(min (v:y (aabb-min box0)) (v:y (aabb-min box1)))
			(min (v:z (aabb-min box0)) (v:z (aabb-min box1))))
		(v:vec3 (max (v:x (aabb-max box0)) (v:x (aabb-max box1)))
			(max (v:y (aabb-max box0)) (v:y (aabb-max box1)))
			(max (v:z (aabb-max box0)) (v:z (aabb-max box1))))))
    ((_ obj-list)
     (let ((mergedbox (make-embty-box)))
       (for-each (lambda (v)
		   (receive (valid? box)
			    (bounding-box v 0 0)
			    (set! mergedbox (surrounding-box mergedbox box))))
		 obj-list)
       mergedbox))))

(define (get-sphere-uv p)
  (let ((phi (atan (v:z p) (v:z p)))
        (theta (asin (v:y p))))
    (values (- 1 (/ (+ phi pi)
                    (* 2 pi)))
            (/ (+ theta pi/2)
               pi))))

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
                        (let*-values (((p) (point-at-parameter r temp))
                                      ((normal) (v:scale (v:diff p center)
                                                          (/ 1 radius)))
                                      ((u v) (get-sphere-uv p)))
                          (values #t (make-hit-record temp p normal material u v)))
                        (let ((temp (/ (+ (- b) (sqrt discriminant))
                                       a)))
                          (if (< t-min temp t-max)
                              (let*-values (((p) (point-at-parameter r temp))
                                            ((normal) (v:scale (v:diff p center)
                                                                (/ 1 radius)))
                                            ((u v) (get-sphere-uv p)))
                                (values #t (make-hit-record temp p normal material u v)))
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
                          (let*-values (((p) (point-at-parameter r temp))
                                        ((normal) (v:scale (v:diff p current-center)
                                                            (/ 1 radius)))
                                        ((u v) (get-sphere-uv p)))
                            (values #t (make-hit-record temp p normal material u v)))
                          (let ((temp (/ (+ (- b) (sqrt discriminant))
                                         a)))
                            (if (< t-min temp t-max)
                                (let*-values (((p) (point-at-parameter r temp))
                                              ((normal) (v:scale (v:diff p current-center)
                                                                  (/ 1 radius)))
                                              ((u v) (get-sphere-uv p)))
                                  (values #t (make-hit-record temp p normal material u v)))
                                (values #f #f))))))))
            (lambda (t0 t1)
              (values #t (surrounding-box
                          (make-aabb (v:diff (center t0) (v:vec3 radius radius radius))
                                     (v:sum (center t0) (v:vec3 radius radius radius)))
                          (make-aabb (v:diff (center t1) (v:vec3 radius radius radius))
                                     (v:sum (center t1) (v:vec3 radius radius radius))))))
            material center0 center1 time0 time1 radius)))

(define (bvh-left bvh)
  (vector-ref bvh 2))
(define (bvh-right bvh)
  (vector-ref bvh 3))
(define (bvh-aabb bvh)
  (vector-ref bvh 4))
(define (bvh-n bvh)
  (vector-ref bvh 5))

(define (make-bvh-node obj-list time0 time1)
  (let* ((axis (floor->exact (* 3 (random-real))))
         (obj-list (sort obj-list (box-compare axis)))
         (n (length obj-list))
         (half (floor->exact (/ n 2)))
         (left (cond ((= n 1) (ref obj-list 0))
                     ((= n 2) (ref obj-list 0))
                     (else (make-bvh-node (subseq obj-list 0 half)
                                          time0 time1))))
         (right (cond ((= n 1) (ref obj-list 0))
                      ((= n 2) (ref obj-list 1))
                      (else (make-bvh-node (subseq obj-list
                                                   half
                                                   (length obj-list))
                                           time0 time1)))))
    (let*-values (((valid-left? left-box) (bounding-box left 0 0))
                  ((valid-right? right-box) (bounding-box right 0 0))
                  ((aabb) (surrounding-box left-box right-box)))
      (vector (lambda (r t-min t-max)
                (receive (hit? hit-rec)
                         (hit aabb r t-min t-max)
                         (if (not hit?)
                             (values #f #f)
                             (let-values (((hit-left? left-rec) (hit left r t-min t-max))
                                          ((hit-right? right-rec) (hit right r t-min t-max)))
                               (cond
                                ((and hit-left? hit-right?)
                                 (values #t (if (< (t left-rec) (t right-rec))
                                                left-rec right-rec)))
                                (hit-left? (values #t left-rec))
                                (hit-right? (values #t right-rec))
                                (else (values #f #f)))))))
              (lambda (t0 t1)
                (values #t aabb))
	      left right aabb n))))

(define (box-compare axis)
  (lambda (a b)
    (let-values (((valid-left? left-box) (bounding-box a 0 0))
                 ((valid-right? right-box) (bounding-box b 0 0)))
      (if (or (not valid-left?) (not valid-right?))
          (display "No bounding box in bvh node constructor"))
      (if (< (- (v:vec3-ref (aabb-min left-box) axis)
                (v:vec3-ref (aabb-min right-box) axis)) 0)
          -1 1))))

(define (box-center-compare axis)
  (lambda (a b)
    (let-values (((valid-left? left-box) (bounding-box a 0 0))
                 ((valid-right? right-box) (bounding-box b 0 0)))
      (if (or (not valid-left?) (not valid-right?))
          (display "No bounding box in bvh node constructor"))
      (if (< (- (v:vec3-ref (aabb-center left-box) axis)
                (v:vec3-ref (aabb-center right-box) axis)) 0)
          -1 1))))

(define (surface-area bbox)
  (let* ((d (v:diff (aabb-max bbox) (aabb-min bbox)))
	 (dx (v:x d))
	 (dy (v:y d))
	 (dz (v:z d)))
    (* 2 (+ (* dx dy) (* dx dz) (* dy dz)))))

(define (get-bbox obj time0 time1)
  (receive (valid? bbox)
	   (bounding-box obj time0 time1)
	   bbox))

(define (make-bvh-with-sah obj-list time0 time1)
  (let* ((obj-vec (list->vector obj-list))
	 (num-obj (length obj-list))
	 (t-tri 1)
	 (t-aabb 1)
	 (best-cost (* t-tri num-obj))
	 (best-axis -1)
	 (best-split-index -1)
	 (bbox (surrounding-box obj-list))
	 (sa-root (surface-area bbox))
	 (leaf #f)
	 (right #f)
	 (left #f))
    (dotimes (axis 3)
	     (set! obj-list (sort obj-list (box-center-compare axis)))
	     (let* ((s1 '())
		    (s2 (list-copy obj-list))
		    (s1bbox (make-embty-box))
		    (s2bbox (make-embty-box))
		    (s1sa (make-vector (+ num-obj 1) +max-float+))
		    (s2sa (make-vector (+ num-obj 1) +max-float+))
		    (tmp-obj #f))
	       (dotimes (i (+ num-obj 1))
			(vector-set! s1sa i (abs (surface-area s1bbox)))
			(when (> (length s2) 0)
			      (set! tmp-obj (pop! s2))
			      (set! s1 (append! s1 (cons tmp-obj '())))
			      (set! s1bbox (surrounding-box (get-bbox tmp-obj 0 0)
							    s1bbox))))
	       (dotimes (i (+ num-obj 1))
			(let ((i (- num-obj i))
			      (cost #f))
			  (vector-set! s2sa i (abs (surface-area s2bbox)))
			  (when (and (> (length s1) 0)
				     (> (length s2) 0))
				(set! cost (+ (* 2 t-aabb)
					      (/ (* (+ (* (vector-ref s1sa i) (length s1))
						       (* (vector-ref s2sa i) (length s2)))
						    t-tri)
						 sa-root)))
				(when (< cost best-cost)
				      (set! best-cost cost)
				      (set! best-axis axis)
				      (set! best-split-index i))))
			(when (> (length s1) 0)
			      (set! tmp-obj (last s1))
			      (push! s2 tmp-obj)
			      (set! s1 (drop-right! s1 1))
			      (set! s2bbox (surrounding-box (get-bbox tmp-obj 0 0)
							    s2bbox))))))
    (cond ((= best-axis -1)
	   (set! leaf obj-list))
	  (else
	   (set! obj-list (sort obj-list (box-center-compare best-axis)))
	   (set! left (make-bvh-with-sah (subseq obj-list 0 best-split-index) 0 0))
	   (set! right (make-bvh-with-sah (subseq obj-list
						  best-split-index
						  (length obj-list)) 0 0))))
    (vector
     (lambda (r t-min t-max)
       (receive (hit? hit-rec)
		(hit bbox r t-min t-max)
		(if (not hit?)
		    (values #f #f)
		    (if leaf
			(hit-obj-list leaf r t-min t-max)
			(let-values (((hit-left? left-rec) (hit left r t-min t-max))
				     ((hit-right? right-rec) (hit right r t-min t-max)))
			  (cond
			   ((and hit-left? hit-right?)
			    (values #t (if (< (t left-rec) (t right-rec))
					   left-rec right-rec)))
			   (hit-left? (values #t left-rec))
			   (hit-right? (values #t right-rec))
			   (else (values #f #f))))))))
     (lambda (t0 t1)
       (values #t bbox))
     left right bbox num-obj leaf)))

(define (bvh-leaf bvh-sah)
  (vector-ref bvh-sah 6))

(define (make-xy-rect x0 x1 y0 y1 k material)
  (vector (lambda (ray t-min t-max)
            (let ((t (/ (- k (v:z (origin ray)))
                        (v:z (dir ray)))))
              (if (or (< t t-min) (> t t-max))
                  (values #f #f)
                  (let ((x (+ (v:x (origin ray)) (* t (v:x (dir ray)))))
                        (y (+ (v:y (origin ray)) (* t (v:y (dir ray))))))
                    (if (or (< x x0) (> x x1) (< y y0) (> y y1))
                        (values #f #f)
                        (values #t (make-hit-record t (point-at-parameter ray t)
                                                    (v:vec3 0 0 1) material
                                                    (/ (- x x0) (- x1 x0))
                                                    (/ (- y y0) (- y1 y0)))))))))
          (lambda (t0 t1)
            (values #t (make-aabb (v:vec3 x0 y0 (- k 0.0001))
                                  (v:vec3 x1 y1 (+ k 0.0001)))))
          material x0 x1 y0 y1))

(define (make-xz-rect x0 x1 z0 z1 k material)
  (vector (lambda (ray t-min t-max)
            (let ((t (/ (- k (v:y (origin ray)))
                        (v:y (dir ray)))))
              (if (or (< t t-min) (> t t-max))
                  (values #f #f)
                  (let ((x (+ (v:x (origin ray)) (* t (v:x (dir ray)))))
                        (z (+ (v:z (origin ray)) (* t (v:z (dir ray))))))
                    (if (or (< x x0) (> x x1) (< z z0) (> z z1))
                        (values #f #f)
                        (values #t (make-hit-record t (point-at-parameter ray t)
                                                    (v:vec3 0 1 0) material
                                                    (/ (- x x0) (- x1 x0))
                                                    (/ (- z z0) (- z1 z0)))))))))
          (lambda (t0 t1)
            (values #t (make-aabb (v:vec3 x0 (- k 0.0001) z0)
                                  (v:vec3 x1 (+ k 0.0001) z1))))
          material x0 x1 z0 z1 k))

(define (make-yz-rect y0 y1 z0 z1 k material)
  (vector (lambda (ray t-min t-max)
            (let ((t (/ (- k (v:x (origin ray)))
                        (v:x (dir ray)))))
              (if (or (< t t-min) (> t t-max))
                  (values #f #f)
                  (let ((y (+ (v:y (origin ray)) (* t (v:y (dir ray)))))
                        (z (+ (v:z (origin ray)) (* t (v:z (dir ray))))))
                    (if (or (< z z0) (> z z1) (< y y0) (> y y1))
                        (values #f #f)
                        (values #t (make-hit-record t (point-at-parameter ray t)
                                                    (v:vec3 1 0 0) material
                                                    (/ (- y y0) (- y1 y0))
                                                    (/ (- z z0) (- z1 z0)))))))))
          (lambda (t0 t1)
            (values #t (make-aabb (v:vec3 (- k 0.0001) y0 z0)
                                  (v:vec3 (+ k 0.0001) y1 z1))))
          material y0 y1 z0 z1 k))

(define (flip-normals obj)
  (vector (lambda (ray t-min t-max)
            (receive (hit? hit-rec)
                     (hit obj ray t-min t-max)
                     (if hit?
                         (values #t (normal-set! hit-rec (v:scale (normal hit-rec) -1)))
                         (values #f #f))))
          (lambda (t0 t1)
            (bounding-box obj t0 t1))
          (material obj)))

(define (make-box p0 p1 material)
  (let ((box (make-scene
              (list (make-xy-rect (v:x p0) (v:x p1) (v:y p0) (v:y p1)
                                  (v:z p1) material)
                    (flip-normals (make-xy-rect (v:x p0) (v:x p1) (v:y p0) (v:y p1)
                                                (v:z p0) material))
                    (make-xz-rect (v:x p0) (v:x p1) (v:z p0) (v:z p1)
                                  (v:y p1) material)
                    (flip-normals (make-xz-rect (v:x p0) (v:x p1) (v:z p0) (v:z p1)
                                                (v:y p0) material))
                    (make-yz-rect (v:y p0) (v:y p1) (v:z p0) (v:z p1)
                                  (v:x p1) material)
                    (flip-normals (make-yz-rect (v:y p0) (v:y p1) (v:z p0) (v:z p1)
                                                (v:x p0) material))) #f #f))
        (aabb (make-aabb p0 p1)))
    (vector (lambda (ray t-min t-max)
              (hit box ray t-min t-max))
            (lambda (t0 t1)
              (values #t aabb))
            material)))

(define (translate obj offset)
  (vector (lambda (ray t-min t-max)
            (let ((moved-ray (make-ray-with-time (v:diff (origin ray) offset)
                                                 (dir ray)
                                                 (ray-time ray))))
              (receive (hit? hit-rec)
                       (hit obj moved-ray t-min t-max)
                       (if hit?
                           (values #t (p-set! hit-rec (v:sum (p hit-rec) offset)))
                           (values #f #f)))))
          (lambda (t0 t1)
            (receive (valid? box)
                     (bounding-box obj t0 t1)
                     (if valid?
                         (values #t (make-aabb (v:sum (aabb-min box) offset)
                                               (v:sum (aabb-max box) offset)))
                         (values #f #f))))))

(define (rotate-y obj angle)
  (let*-values (((radians) (* pi/180 angle))
                ((sin-theta) (sin radians))
                ((cos-theta) (cos radians))
                ((has-box? box) (bounding-box obj 0 1))
                ((min-vec) (v:vec3 +max-float+ +max-float+ +max-float+))
                ((max-vec) (v:vec3 (- +max-float+) (- +max-float+) (- +max-float+))))
    (dotimes
     (i 2)
     (dotimes
      (j 2)
      (dotimes
       (k 2)
       (let* ((x (+ (* i (v:x (aabb-max box)))
                    (* (- 1 i) (v:x (aabb-min box)))))
              (y (+ (* j (v:y (aabb-max box)))
                    (* (- 1 j) (v:y (aabb-min box)))))
              (z (+ (* k (v:z (aabb-max box)))
                    (* (- 1 k) (v:z (aabb-min box)))))
              (newx (+ (* cos-theta x) (* sin-theta z)))
              (newz (+ (* (- sin-theta) x) (* cos-theta z)))
              (tester (v:vec3 newx y newz)))
         (dotimes (c 3)
                  (if (> (v:vec3-ref tester c) (v:vec3-ref max-vec c))
                      (v:vec3-set! max-vec c (v:vec3-ref tester c)))
                  (if (> (v:vec3-ref tester c) (v:vec3-ref min-vec c))
                      (v:vec3-set! min-vec c (v:vec3-ref tester c))))))))
    (let ((box (make-aabb min-vec max-vec)))
      (vector (lambda (ray t-min t-max)
                (let* ((orig (v:vec3 (- (* cos-theta (v:vec3-ref (origin ray) 0))
                                        (* sin-theta (v:vec3-ref (origin ray) 2)))
                                     (v:vec3-ref (origin ray) 1)
                                     (+ (* sin-theta (v:vec3-ref (origin ray) 0))
                                        (* cos-theta (v:vec3-ref (origin ray) 2)))))
                       (direction (v:vec3 (- (* cos-theta (v:vec3-ref (dir ray) 0))
                                             (* sin-theta (v:vec3-ref (dir ray) 2)))
                                          (v:vec3-ref (dir ray) 1)
                                          (+ (* sin-theta (v:vec3-ref (dir ray) 0))
                                             (* cos-theta (v:vec3-ref (dir ray) 2)))))
                       (rotated-ray (make-ray-with-time orig direction (ray-time ray))))
                  (receive (hit? hit-rec)
                           (hit obj rotated-ray t-min t-max)
                           (if hit?
                               (let ((p (v:vec3 (+ (* cos-theta (v:vec3-ref (p hit-rec) 0))
                                                   (* sin-theta (v:vec3-ref (p hit-rec) 2)))
                                                (v:vec3-ref (p hit-rec) 1)
                                                (+ (* (- sin-theta) (v:vec3-ref (p hit-rec) 0))
                                                   (* cos-theta (v:vec3-ref (p hit-rec) 2)))))
                                     (normal (v:vec3 (+ (* cos-theta (v:vec3-ref (normal hit-rec) 0))
                                                        (* sin-theta (v:vec3-ref (normal hit-rec) 2)))
                                                     (v:vec3-ref (normal hit-rec) 1)
                                                     (+ (* (- sin-theta) (v:vec3-ref (normal hit-rec) 0))
                                                        (* cos-theta (v:vec3-ref (normal hit-rec) 2))))))
                                 (values #t (make-hit-record (t hit-rec) p normal
                                                             (material obj)
                                                             (u hit-rec)
                                                             (v hit-rec))))
                               (values #f #f)))))
              (lambda (t0 t1)
                (values #t box))
              (material obj)))))

(define (make-constant-medium obj density a)
  (let ((phase-function (m:make-lambertian a) ;(make-isotropic a)
                        ))
    (vector (lambda (ray t-min t-max)
              (receive (hit-r1? hit-rec1)
                       (hit obj ray (- +max-float+) +max-float+)
                       (if hit-r1?
                           (receive (hit-r2? hit-rec2)
                                    (hit obj ray (+ (t hit-rec1) 0.0001) +max-float+)
                                    (if hit-r2?
                                        (let* ((t1 (if (< (t hit-rec1) t-min) t-min (t hit-rec1)))
                                               (t2 (if (> (t hit-rec2) t-max) t-max (t hit-rec2))))
                                          (if (>= t1 t2)
                                              (values #f #f)
                                              (let* ((t1 (if (< t1 0) 0 t1))
                                                     (distance-inside-boundary (* (- t2 t1)
                                                                                  (v:length (dir ray))))
                                                     (hit-distance (* (- (/ 1 density))
                                                                      (log (random-real)))))
                                                (if (< hit-distance distance-inside-boundary)
                                                    (let* ((nt (+ t1 (/ hit-distance
                                                                        (v:length (dir ray)))))
                                                           (np (point-at-parameter ray nt)))
                                                      (values #t (make-hit-record nt np
                                                                                  (v:vec3 1 0 0)
                                                                                  phase-function
                                                                                  0 0)))
                                                    (values #f #f)))))
                                        (values #f #f)))
                           (values #f #f))
                       ))
            (lambda (t0 t1)
              (bounding-box obj t0 t1))
            phase-function)))

;; (define +sphere-pos+ (vector (v:vec3 5 5 0)
;;                              (v:vec3 5 -5 0)
;;                              (v:vec3 -5 5 0)
;;                              (v:vec3 -5 -5 0)
;;                              (v:vec3 0 0 7.071)
;;                              (v:vec3 0 0 -7.071)))
;; (define +sphere-r+ 5)
;; (define +sphere-r2+ 25)
;; (define klein-sphere-r 2.0833)

(define +sphere-pos+ (vector (v:vec3 300 300 0)
                             (v:vec3 300 -300 0)
                             (v:vec3 -300 300 0)
                             (v:vec3 -300 -300 0)
                             (v:vec3 0 0 424.26)
                             (v:vec3 0 0 -424.26)))
(define +sphere-r+ 300)
(define +sphere-r2+ (* +sphere-r+ +sphere-r+))
(define klein-sphere-r 125)

(define +max-klein-loop+ 10)

(define (dist-func center pos)
  (let loop ((iter-count 0)
             (pos (v:diff pos center))
             (dr 1))
    (if (>= iter-count +max-klein-loop+)
        (values (* 0.7 (/ (- (v:length pos) klein-sphere-r)
                           (abs dr)))
                (dec! iter-count))
        (let sphere-loop ((index 0))
          (if (< index (vector-length +sphere-pos+))
              (let ((sphere-pos  (vector-ref +sphere-pos+ index)))
                (if (< (v:length (v:diff pos sphere-pos)) +sphere-r+)
                    (let* ((diff (v:diff pos sphere-pos))
                           (dr (* dr (/ +sphere-r2+ (v:dot diff diff)) ))
                           (pos (v:sum (v:scale (v:scale diff +sphere-r2+)
                                                (/ 1 (* (v:length diff) (v:length diff))))
                                       sphere-pos)))
                      (loop (inc! iter-count) pos dr))
                    (sphere-loop (inc! index))))
              (values (* 0.7 (/ (- (v:length pos) klein-sphere-r)
                                 (abs dr)))
                      iter-count
                      ))))))

(define (get-normal center p)
  (v:unit (v:vec3 (- (dist-func center (v:sum p (v:vec3 0.01 0 0)))
                     (dist-func center (v:diff p (v:vec3 0.01 0 0))))
                  (- (dist-func center (v:sum p (v:vec3 0 0.01 0)))
                     (dist-func center (v:diff p (v:vec3 0 0.01 0))))
                  (- (dist-func center (v:sum p (v:vec3 0 0 0.01)))
                     (dist-func center (v:diff p (v:vec3 0 0 0.01)))))))


(define +max-marching-loop+ 100)

(define (hsv->rgb c)
  (let ((p (v:diff (v:scale (v:vec3 (abs (fmod (+ (v:x c) 1)))
                                    (abs (fmod (+ (v:x c) 0.66666666666)))
                                    (abs (fmod (+ (v:x c) 0.333333333))))
                            6)
                   (v:vec3 3 3 3))))))

(define (make-klein center material)
  (vector
   (lambda (ray t-min t-max)
     (let loop ((iter-count 0)
                (dist 0)
                (ray-pos (origin ray))
                (ray-length 0))
       (if (>= iter-count +max-marching-loop+)
           (values #f #f)
           (let*-values (((dist inversion-count) (dist-func center ray-pos))
                         ((ray-length) (+ ray-length dist))
                         ((ray-pos) (v:sum (origin ray) (v:scale (dir ray) ray-length))))
             (if (and (< dist 0.001) (< t-min ray-length t-max))
                 (values #t (make-hit-record ray-length (point-at-parameter ray ray-length)
                                             (get-normal center ray-pos)
                                             material
                                             0 0))
                 (loop (inc! iter-count) dist ray-pos ray-length))))))
   (lambda (t0 t1)
     (values #f #f))
   material))
