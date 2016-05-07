(define-module points
  (use vec :prefix v:)
  (use srfi-13)
  (use bezier :prefix b:)
  (use gauche.collection)
  (export-all))

(select-module points)

(define (load-points file-name)
  (let ((points '()))
    (with-input-from-file file-name
      (lambda ()
        (port-for-each (lambda (line)
                         (push! points
                                (apply v:vec3 (map string->number
                                                   (string-split line ",")))))
                       read-line)))
    (list->vector (reverse! points))))

;;tightness = 0.5
(define  (calc-bezier-cp pt p1 p2 p3)
  (let ((d1 (v:quot (v:diff p2 pt) 6))
        (d2 (v:quot (v:diff p3 p1) 6)))
    (list p1 (v:sum p1 d1) (v:diff p2 d2) p2)))

(define (points->bezier points)
  (letrec ((last (- (vector-length points) 2))
           (bezier-list '())
           (loop (lambda (i)
                   (cond
                    ((< i last)
                     (push! bezier-list
                            (calc-bezier-cp (ref points (- i 1))
                                            (ref points i)
                                            (ref points (+ i 1))
                                            (ref points (+ i 2))))
                     (loop (inc! i)))
                    (else bezier-list)))))
    (reverse! (loop 1))))

(define (bezier->objs beziers width material)
  (map (lambda (bez)
         (b:make-bezier (ref bez 0)
                        (ref bez 1)
                        (ref bez 2)
                        (ref bez 3)
                        width material))
       beziers))


