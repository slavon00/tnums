;TESTS
;need to load tnums.lisp and user-function.lisp before using (load "tnums.lisp") and (load "user-functions.lisp") or (load "load.lisp")
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

;(time (setq tn (tnum/ (tnum-pi) (tnum-e) (tnum-phi))))
;(...)
;0.000009 s
(...)

;(time (tnum-to-string tn 50))
;(...)
;0.600614 s
;(...)
;"0.71427878389862830105313858884996215912911202055654..."

;(time (tnum-to-string (tnum-sin tn) 50))
;(...)
;30.933354 s
;"0.65507266083018698987837667114834999658318351993255..."

;(time (tnum-to-string (tnum-csc tn) 50))
;(...)
;1242.738475 s
;"1.52654821334274511450496570931719335427594133724623..."

;(time (tnum-to-string (tnum-ctan tn) 50))
;(...)
; ? s
;(...)

