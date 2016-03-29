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

(define-record-type hit-record #t #t
  (t t t-set!)
  (p p p-set!)
  (normal normal normal-set!)
  (material material material-set!))
