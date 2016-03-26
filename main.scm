(add-load-path "." :relative)

(define-module main
  (use vec))

(select-module main)

(let ((nx 200)
      (ny 100))
  (with-output-to-file "test.ppm"
    (lambda ()
      (display (format "P3\n ~D ~D\n255\n" nx ny))
      (let loop-y ((y (- ny 1)))
        (if (>= y 0)
            (let loop-x ((x 0))
              (if (< x nx)
                  (let* ((col (make-vec (/ x nx) (/ y ny) 0.2))
                         (ir (floor->exact (* 255.99 (vec-x col))))
                         (ig (floor->exact (* 255.99 (vec-y col))))
                         (ib (floor->exact (* 255.99 (vec-z col)))))
                    (display (format "~D ~D ~D\n"
                                     ir ig ib))
                    (loop-x (+ x 1)))
                  (loop-y (- y 1)))))))))
