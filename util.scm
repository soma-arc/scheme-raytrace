(define-module util
  (use srfi-27)
  (use vec :prefix v:)
  (export-all))

(select-module util)

(define (random-in-unit-sphere)
  (let loop ((p (v:diff (v:scale (v:vec3 (random-real) (random-real) (random-real)) 2)
                        (v:vec3 1 1 1))))
    (if (< (v:sq-len p) 1)
        p
        (loop (v:diff (v:scale (v:vec3 (random-real) (random-real) (random-real)) 2)
                      (v:vec3 1 1 1))))))
