(define-module material
  (use gauche.record)
  (use util)
  (use srfi-27)
  (use vec :prefix v:)
  (use ray)
  (use util)
  (export-all))

(select-module material)

(define (scatter material ray hit-rec)
  ((vector-ref material 0) ray hit-rec))

(define (make-lambertian albedo)
  (vector (lambda (ray hit-rec)
            (let ((target (v:sum (p hit-rec)
                                 (normal hit-rec)
                       (random-dir-over-hemisphere (normal hit-rec)))))
              (values #t
                      (make-ray (p hit-rec) (v:diff target (p hit-rec)))
                      albedo)))
          albedo))

(define (reflect v n)
  (v:diff v
          (v:scale n (* 2 (v:dot v n)))))

(define (make-metal albedo fuzz)
  (vector (lambda (ray hit-rec)
            (let* ((reflected (reflect (v:unit (dir ray)) (normal hit-rec)))
                   (scattered (make-ray (p hit-rec)
                                        (v:sum reflected
                                               (v:scale (random-in-unit-sphere)
                                                        fuzz)))))
              (values (> (v:dot (dir scattered) (normal hit-rec)) 0)
                      scattered
                      albedo)))))

(define (refract v n ni-over-nt)
  (let* ((uv (v:unit v))
         (dt (v:dot uv n))
         (discriminant (- 1 (* ni-over-nt ni-over-nt
                               (- 1 (* dt dt))))))
    (if (> discriminant 0)
        (values #t (v:diff (v:scale (v:diff v (v:scale n dt)) ni-over-nt)
                           (v:scale n (sqrt discriminant))))
        (values #f #f))))

(define (schlick cosine ref-idx)
  (let* ((r0 (/ (- 1 ref-idx)
                (+ 1 ref-idx)))
         (r0 (* r0 r0)))
    (+ r0 (* (- 1 r0)
             (expt (- 1 cosine) 5)))))

(define (make-dielectric ref-idx)
  (vector (lambda (ray hit-rec)
            (let* ((reflected (reflect (dir ray) (normal hit-rec)))
                   (attenuation (v:vec3 1 1 1))
                   (dd (v:dot (dir ray) (normal hit-rec)))
                   (outward-normal (if (> dd 0)
                                       (v:scale (normal hit-rec) -1)
                                       (normal hit-rec)))
                   (ni-over-nt (if (> dd 0)
                                   ref-idx
                                   (/ 1 ref-idx)))
                   (cosine (if (> dd 0)
                               (/ (* dd ref-idx) (v:length (dir ray)))
                               (/ (- dd) (v:length (dir ray))))))
              (receive (valid? refracted)
                       (refract (dir ray) (normal hit-rec) ni-over-nt)
                       (let* ((refract-prob (if valid?
                                                (schlick cosine ref-idx)
                                                1))
                              (scattered (if (< (random-real) refract-prob)
                                             (make-ray (p hit-rec) reflected)
                                             (make-ray (p hit-rec) refracted))))
                         (values #t scattered attenuation)))))
          ref-idx))
