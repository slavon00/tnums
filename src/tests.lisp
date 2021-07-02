;TESTS
;need to load tnums.lisp and user-functions.lisp before using: (load "tnums.lisp") and (load "user-functions.lisp") or (load "load.lisp")
;(num-to-tnum 42.123)
;#<FUNCTION (LAMBDA (EPS) :IN NUM-TO-TNUM) {1001EBF73B}>

;(tnum-to-num * 0.1)
;34246/813

;(type-of *)
;RATIO

;(float **)
;42.123

;(tnum-to-string (tnum-pi) 50)
;"3.14159265358979323846264338327950288419716939937510..."

;(tnum-to-num (tnum-e) 20)
;611070150698522592097/224800145555521536000
;(tnum-to-string (tnum-e) 20)
;"2.71828182845904523536..."

;(tnum-to-string (tnum-phi) 50)
;"1.61803398874989484820458683436563811772030917980576..."

;(tnum-to-string (tnum-log (num-to-tnum 9) (num-to-tnum 2)) 50)
;"0.31546487678572871854976355717138042714979282006594..."

;(coerce (tnum-to-num (tnum-sin (num-to-tnum 1)) -20) 'long-float)
;0.8414709848078965d0

;; (let ((tn (tnum/ (tnum-pi) (tnum-e) (tnum-phi))))
;;     (time (tnum-to-string tn 50)) 1.415019
;;     (time (tnum-to-string (tnum-sin tn) 50)) 56.346948
;;     (time (tnum-to-string (tnum-csc tn) 50)) 3273.217844
;;     (time (tnum-to-string (tnum-ctan tn) 50)))