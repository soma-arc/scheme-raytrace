(define-module material
  (use gauche.record)
  (use util)
  (use srfi-27)
  (use vec :prefix v:)
  (use ray)
  (use util)
  (use onb)
  (use texture :prefix t:)
  (use math.const)
  (export-all))

(select-module material)

(define (scatter material ray hit-rec)
  ((vector-ref material 0) ray hit-rec))

(define (scattering-pdf material ray hit-rec scattered)
  ((vector-ref material 1) ray hit-rec scattered))

(define (emitted material ray hit-rec u v p)
  ((vector-ref material 2) ray hit-rec u v p))

(define (make-lambertian albedo)
  (vector (lambda (ray hit-rec)
            (let* ((uvw (make-onb-from-w (normal hit-rec)))
                   (target (local uvw (random-cosine-direction)))
                   (scattered (make-ray (p hit-rec) (v:unit target))))
              (values #t
                      scattered
                      (t:value albedo 0 0 (p hit-rec))
                      (/ (v:dot (w uvw) (dir scattered))  pi))))
          (lambda (ray hit-rec scattered)
            (let* ((cosine (v:dot (normal hit-rec) (v:unit (dir scattered))))
                   (cosine (if (< cosine 0) 0 cosine)))
              (/ cosine pi)))
          (lambda (ray hit-rec u v p)
            (v:vec3 0 0 0))
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
                      (t:value albedo 0 0 (p hit-rec)))))
          (lambda (ray hit-rec u v p)
            (v:vec3 0 0 0))
          albedo fuzz))

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
                       (refract (dir ray) outward-normal ni-over-nt)
                       (let* ((reflect-prob (if valid?
                                                (schlick cosine ref-idx)
                                                1))
                              (scattered (if (< (random-real) reflect-prob)
                                             (make-ray (p hit-rec) reflected)
                                             (make-ray (p hit-rec) refracted))))
                         (values #t scattered attenuation)))))
          (lambda (ray hit-rec u v p)
            (v:vec3 0 0 0))
          ref-idx))

(define (make-diffuse-light emit)
  (vector (lambda (ray hit-rec)
            (values #f #f #f #f))
          (lambda (ray hit-rec scattered)
            #f)
          (lambda (ray hit-rec u v p)
            (if (< (v:dot (normal hit-rec) (dir ray)) 0.0)
                (t:value emit u v p)
                (v:vec3 0 0 0)))))
