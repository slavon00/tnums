;USERE-FUNCTIONS
;(num-to-tnum 42.123)
;(tnum-to-num * 0.1)
;(type-of *)

;funkce pro prevod tnumu na float
(defun tnum-to-float (tnum eps)
  (float (tnum-to-num tnum eps)))

;(tnum-to-float (num-to-tnum 42.123) 0.1)

;funkce pro ladeni -- vypisuje cisla o presnosti 1, 0.1, 0.01 ... 10^-n; pokud behem vypoctu dojde k chybe -- treba deleni nulou -- nevypise nic, vraci posledni vysledek jako racionalni cislo -- zlomek
(defun tnum-print-first-n (tnum n)
  (let ((result 0))
    (dotimes (i n result)
      (setf result (ignore-errors (tnum-to-num tnum (- 0 i 1))))
      (when result (format t "n = ~D, num = ~c~F~%"
        i #\tab
        (coerce result 'long-float))))))

;(tnum-print-first-n (tnum-pi) 13)
;(tnum-print-first-n (tnum-e) 14)

(defun tnum-1+ (tnum)
  (tnum+ (num-to-tnum 1) tnum))

(defun tnum-1- (tnum)
  (tnum- tnum (num-to-tnum 1)))

(defun tnum-sqrt (tnum)
  (tnum-root (num-to-tnum 2) tnum))

(defun tnum-phi ()
  (tnum/ (tnum-1+ (tnum-sqrt (num-to-tnum 5))) (num-to-tnum 2)))

;(tnum-to-float (tnum-phi) -5)

(defun tnum-log (tnum1 tnum2)
  (tnum/ (tnum-ln tnum2) (tnum-ln tnum1)))

;(tnum-print-first-n (tnum-sin (num-to-tnum 1)) 15)

(defun tsin (arg)
  (when (realp arg)
    (setf arg (num-to-tnum arg)))
  (tnum-sin arg))

;(tnum-to-float (tsin 1) -6)

(defun t+ (&rest args)
  (tnum+ (apply 'tnum+ (print (remove-if 'realp args)))
    (num-to-tnum (apply '+ (print (remove-if-not 'realp args))))))

;(tnum-to-float (t+ 1 (tnum-pi) 2 (tnum-e) 3) -6)

(defun t* (&rest args)
  (tnum*num (apply 'tnum* (remove-if 'realp args))
    (apply '* (remove-if-not 'realp args))))

;(tnum-print-first-n (t* (tnum-phi) 4.2) 10)

(defun calc-sin (num)
  (coerce (tnum-to-num (tsin num) -20) 'long-float))

;(calc-sin 2)

(defvar *calc-eps* -6)
(defvar *calc-conversion* (lambda (num) (float num)))

(defun global-sin (num)
  (funcall *calc-conversion* 
    (tnum-to-num (tsin num) *calc-eps*)))

;(global-sin 3)
;(setq *calc-eps* -20)
;(setq *calc-conversion* (lambda (num)
;    (coerce num 'long-float)))
;(global-sin 4)
