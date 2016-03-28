(add-load-path "." :relative)

(define-module ray
  (use gauche.record)
  (use vec :prefix v:)
  (export-all))

(select-module ray)

(define-record-type ray #t #t
  (origin origin origin-set!)
  (dir dir dir-set!))

(define (point-at-parameter ray t)
  (v:sum (origin ray)
         (v:scale (dir ray) t)))

