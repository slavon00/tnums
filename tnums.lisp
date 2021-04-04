;PACKAGE TNUMS 
;Author: slavon00
;prevod cisla na tnum
(defun num-to-tnum (num)
  (lambda (eps)
    (rationalize num)))

;prevod tnum na cislo
(defun tnum-to-num (tnum eps)
  (when (>= 0 eps) (setf eps (expt 10 eps)))
  (when (< 1 eps) (setf eps 1))
  (funcall tnum (rationalize eps)))

;faktorial prirozeneho cisla
(defun factorial (n)
  (let ((result 1))
    (loop for i from n downto 1
      do (setf result (* result i)))
    result))

;n-ty castecny soucet
(defun nth-partial-sum (n nth)
  (let ((result 0))
    (dotimes (i (1+ n) result)
      (incf result (funcall nth i)))))

;presnost na pocet clenu souctu
(defun eps-to-n (eps nth)
  (let ((n 1))
    (loop 
      (if (< (abs (funcall nth n)) eps)
        (return n)
        (incf n)))))

;polynom nad faktorialem
(defun factorial-series-to-num (eps nth)
  (nth-partial-sum (eps-to-n eps nth) nth))

;e
(defun tnum-e ()
  (lambda (eps)
    (factorial-series-to-num eps 
      (lambda (n) (/ (factorial n))))))

;pi
(defun tnum-pi ()
  (lambda (eps)
    (nth-partial-sum 
      (eps-to-n eps
        (lambda (n) (/ (* (expt 16 n) 15))))
      (lambda (n)
        (* (/ (expt 16 n))
          (- (/ 4 (+ (* 8 n) 1))
            (/ 2 (+ (* 8 n) 4))
            (/ 1 (+ (* 8 n) 5))
            (/ 1 (+ (* 8 n) 6))))))))

;tnum vynasobeny konstantou
(defun tnum*num (tnum num)
  (lambda (eps)
    (if (zerop num)
      (num-to-tnum 0)
      (let ((rat_num (rationalize num)))
        (* (tnum-to-num tnum (/ eps (abs rat_num))) rat_num)))))

;negace tnumu
(defun -tnum (tnum)
  (tnum*num tnum (- 1)))

;soucet tnumu
(defun tnum+ (&rest tnums)
  (if (null tnums)
    (num-to-tnum 0)
    (lambda (eps)
      (let ((new-eps (/ eps (list-length tnums))))
        (apply '+ 
          (mapcar (lambda (tnum) (tnum-to-num tnum new-eps))
            tnums))))))

;rozdil tnumu
(defun tnum- (tnum1 &rest tnums)
  (if (null tnums)
    (-tnum tnum1)
    (tnum+ tnum1 (-tnum (apply 'tnum+ tnums)))))

;obraceni tnumu
(defun /tnum (tnum)
  (lambda (eps)
    (let ((num (tnum-to-num tnum eps)))
      (/ (tnum-to-num tnum 
        (* eps (if (zerop num) eps (abs num))))))))

;list pro nasobeni
(defun create-list-for-multiplication (tnums eps)
  (let ((result nil)
      (nums
        (mapcar (lambda (tnum) (tnum-to-num tnum eps)) tnums)))
    (dotimes (i (list-length tnums) result)
      (let ((actual-eps eps))
        (dotimes (j (list-length tnums))
          (when (not (= i j))
            (setf actual-eps (/ actual-eps
              (if (zerop (nth j nums))
                1
                (nth j nums))))))
        (setf result (cons
          (tnum-to-num (nth i tnums) actual-eps) result))))))

;soucin tnumu
(defun tnum* (&rest tnums)
  (if (null tnums)
    (num-to-tnum 1)
    (lambda (eps)
      (apply '* (create-list-for-multiplication tnums eps)))))

;podil tnumu
(defun tnum/ (tnum1 &rest tnums)
  (if (null tnums)
    (/tnum tnum1)
    (tnum* tnum1 (/tnum (apply 'tnum* tnums)))))

;exponenciala
(defun tnum-exp (tnum)
  (lambda (eps)
    (let ((x (tnum-to-num tnum eps)))
      (factorial-series-to-num eps
        (lambda (n) (/ (expt x n) (factorial n)))))))

;sinus
(defun tnum-sin (tnum)
  (lambda (eps)
    (let ((x (tnum-to-num tnum eps)))
      (factorial-series-to-num eps
        (lambda (n)
          (/ (expt x (1+ (* 2 n)))
            (factorial (1+ (* 2 n)))
            (expt (- 1) n)))))))

;cosinus
(defun tnum-cos (tnum)
  (lambda (eps)
    (let ((x (tnum-to-num tnum eps)))
      (factorial-series-to-num eps
        (lambda (n)
          (/ (expt x (* 2 n))
            (factorial (* 2 n))
            (expt (- 1) n)))))))

;tangens
(defun tnum-tan (tnum)
  (tnum/ (tnum-sin tnum) (tnum-cos tnum)))

;kosecans
(defun tnum-csc (tnum)
  (/tnum (tnum-sin tnum)))

;secans
(defun tnum-sec (tnum)
  (/tnum (tnum-cos tnum)))

;kotangens
(defun tnum-ctan (tnum)
  (/tnum (tnum-tan tnum)))

;pomocna funkce na logaritmus
(defun do-tnum-ln (tnum)
  (lambda (eps)
    (let* ((x (tnum-to-num tnum eps))
        (x-1/x+1 (/ (1- x) (1+ x))))
      (nth-partial-sum 
        (eps-to-n eps
          (lambda (n)
            (/ (expt x-1/x+1 (* 2 (1+ n))) (- 1 x-1/x+1))))
        (lambda (n)
          (/ (expt x-1/x+1 (1+ (* 2 n))) (1+ (* 2 n))))))))

;logaritmus
(defun tnum-ln (tnum)
  (tnum*num (do-tnum-ln tnum) 2))

;tnum1 na tnum2-tou
(defun tnum-expt (tnum1 tnum2)
  (tnum-exp (tnum* tnum2 (tnum-ln tnum1))))

;tnum1-ta odmocnina tnum2
(defun tnum-root (tnum1 tnum2)
  (tnum-expt tnum2 (/tnum tnum1)))

;TESTY
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
