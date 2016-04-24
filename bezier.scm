(define-module bezier
  (use ray)
  (use gauche.array)
  (use vec :prefix v:)
  (use constant)
  (use geometry :prefix g:)
  (export-all))

(select-module bezier)

(define (get-projection-mat ray)
  (let* ((ray-origin (origin ray))
         (ray-dir (dir ray))
         (ox (- (v:x ray-origin)))
         (oy (- (v:y ray-origin)))
         (oz (- (v:z ray-origin)))
         (ray-dir (v:unit ray-dir))
         (lx (v:x ray-dir))
         (ly (v:y ray-dir))
         (lz (v:z ray-dir))
         (d (sqrt (+ (* lx lx) (* lz lz))))))
  (array-mul #,(<f64array> (0 4 0 4)
                           1 0 0 0
                           0 1 0 0
                           0 0 1 0
                           ox oy oz 1)
             #,(<f64array> (0 4 0 4)
                           (/ lz d) (/ (* -1 lx ly) d) lx 0
                           0 d ly 0
                           (/ (- lx) d) (/ (* -1 ly lz) d) lz 0
                           0 0 0 1)))

(define (internally-divide a b t)
  (v:sum (v:scale a (- 1 t))
         (v:scale b t)))

(define (make-bezier a b c d)
  (letrec* ((cp (vector a b c d))
            (bezier-point
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
               (let* ((split-point (bezier-point t))
                      (nbc (internally-divide b c t))
                      (lb (internally-divide a b t))
                      (lc (internally-divide lb nbc t))
                      (rc (internally-divide c d t))
                      (rb (internally-divide nbc rc t)))
                 (values (make-bezier a lb lc split-point)
                         (make-bezier split-point rb rc d)))))
            (bbox
             (lambda ()
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
            (hit (lambda (r t-min t-max)
                   (+ 1 2))))
           (vector hit
                   bbox
                   cp
                   bezier-point
                   split)))

(define (bezier-point bezier t)
  ((vector-ref bezier 3) t))

(define (bezier-split bezier t)
  ((vector-ref bezier 4) t))

