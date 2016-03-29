(define-module util
  (use srfi-27)
  (use vec :prefix v:)
  (export-all))

(select-module util)

(define (random-in-unit-sphere)
  (let loop ((p (v:scale (v:diff (v:vec3 (random-real) (random-real) (random-real))
                                 (v:vec3 1 1 1))
                         2)))
    (if (< (v:sq-len p) 1)
        p
        (loop (v:scale (v:diff (v:vec3 (random-real) (random-real) (random-real))
                               (v:vec3 1 1 1))
                       2)))))
