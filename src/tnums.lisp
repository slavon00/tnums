;PACKAGE TNUMS 
;AUTHOR:    slavon00
;INSTITUTE: UPOL/PRF/KI
;-------------
;prevod cisla na tnum
(defun num-to-tnum (num)
  (let ((rat_num (rationalize num)))
    (lambda (eps) (declare (ignore eps))
      rat_num)))

;racionalizovany exponent
(defun rat-expt (num exp)
  (rationalize (expt num exp)))

;prevod tnum na cislo
(defun tnum-to-num (tnum eps)
  (when (or (>= 0 eps) (<= 1 eps))
    (setf eps (rat-expt 10 (- (abs eps)))))
  (funcall tnum (rationalize eps)))

;pi
(defun tnum-pi ()
  (lambda (eps)
    (let ((result 0))
      (loop for i from 0
            for /16powi = (expt 16 (- i)) and 8i = (* 8 i)
            do (incf result 
                     (* /16powi
                        (- (/ 4 (+ 8i 1))
                           (/ 2 (+ 8i 4))
                           (/ (+ 8i 5))
                           (/ (+ 8i 6)))))
            until (<= (/ /16powi 15) eps)
            finally (return result)))))

;tnum vynasobeny konstantou
(defun tnum*num (tnum num)
  (let ((rat_num (rationalize num)))
    (lambda (eps)
      (if (zerop num)
          (num-to-tnum 0)
        (* (tnum-to-num tnum (/ eps (abs rat_num))) rat_num)))))  

;negace tnumu
(defun -tnum (tnum)
  (lambda (eps)
    (- (tnum-to-num tnum eps))))

;soucet tnumu
(defun tnum+ (&rest tnums)
  (if (null tnums)
      (num-to-tnum 0)
    (lambda (eps)
      (let ((new-eps (/ eps (list-length tnums))))
        (apply '+ 
               (mapcar (lambda (tnum) 
                         (tnum-to-num tnum new-eps))
                       tnums))))))

;rozdil tnumu
(defun tnum- (tnum1 &rest tnums)
  (if (null tnums)
      (-tnum tnum1)
    (tnum+ tnum1 (-tnum (apply 'tnum+ tnums)))))

;vycisleni nenuloveho tnumu
(defun get-nonzero-num+eps (tnum eps)
  (let ((num (tnum-to-num tnum eps)))
    (if (or (zerop num) (<= (abs num) eps))
        (get-nonzero-num+eps tnum (/ eps 10))
      (values num eps))))

;obraceni tnumu
(defun /tnum (tnum)
  (lambda (eps)
    (multiple-value-bind (num eps0)
        (get-nonzero-num+eps tnum eps)
      (let* ((absnum (abs num))
             (neweps (* eps absnum (- absnum eps0))))
        (/ (if (>= neweps eps) num
             (get-nonzero-num+eps tnum neweps)))))))

;list pro nasobeni
(defun create-list-for-multiplication (tnums eps)
  (let* ((result nil) (len (list-length tnums)) (el (/ eps len))
         (nums (mapcar (lambda (tnum) 
                         (tnum-to-num tnum el)) tnums)))
    (dotimes (i len result)
      (let ((neps 1))
        (dotimes (j len)
          (unless (= i j)
            (setf neps (/ neps (+ (abs (nth j nums)) eps)))))
        (setf result (cons
                      (if (<= neps 1) (nth i nums)
                        (tnum-to-num (nth i tnums)
                                     (/ el (max neps 1))))
                      result))))))

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

;faktorial prirozeneho cisla
(defun factorial (n)
  (let ((result 1))
    (loop for i from n downto 1
          do (setf result (* result i)))
    result))

;exponenciala cisla
(defun num-exp (num eps)
  (let ((above (rat-expt 272/100 num)) (result 0))
    (loop for n from 0
          for nfact = (factorial n)
          for xpown = (expt num n)
          do (incf result (/ xpown nfact))
          until (<= (/ (* above xpown) nfact) eps)
          finally (return result))))

;eulerovo cislo
(defun tnum-e ()
  (lambda (eps)
    (num-exp 1 eps)))

;exponenciala tnumu
(defun tnum-exp (tnum)
  (lambda (eps)
    (loop for i from 0
          for num = (if (zerop i) (tnum-to-num tnum eps) new)
          for new = (if (zerop i) 1
                      (if (> expnum 1)
                          (tnum-to-num tnum (/ eps 
                                               (+ expnum eps)))
                        num))
          for expnum = (num-exp num eps)
          until (= num new)
          finally (return expnum))))

;sinus cisla
(defun num-sin (x eps)
  (let ((result 0))
    (loop for n from 0
          for 2n+1 = (1+ (* 2 n))
          do (incf result
                   (/ (rat-expt x 2n+1)
                      (factorial 2n+1) (expt -1 n)))
          until (< (abs (/ (rat-expt x (1+ 2n+1))
                           (factorial (1+ 2n+1))))
                   eps)
          finally (return result))))

;sinus
(defun tnum-sin (tnum)
  (lambda (eps)
    (num-sin (tnum-to-num tnum eps) eps)))

;kosinus cisla
(defun num-cos (x eps)
  (let ((result 0))
    (loop for n from 0
          for 2n = (* 2 n)
          do (incf result
                   (/ (rat-expt x 2n)
                      (factorial 2n)
                      (expt -1 n)))
          until (< (abs (/ (rat-expt x (1+ 2n))
                           (factorial (1+ 2n))))
                   eps)
          finally (return result))))

;kosinus
(defun tnum-cos (tnum)
  (lambda (eps)
    (num-cos (tnum-to-num tnum eps) eps)))

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
  (tnum/ (tnum-cos tnum) (tnum-sin tnum)))

;logaritmus cisla
(defun num-ln (x eps)
  (setf eps (/ eps 2))
  (let ((n 0) (result 0) (q (/ (1- x) (1+ x))))
    (loop 
     do (progn 
          (incf result
                (/ (expt q (1+ (* 2 n))) (1+ (* 2 n))))
          (incf n))
     until (<= (abs (/ (expt q (* 2 (1+ n))) (- 1 q)))
               eps)
     finally (return (* 2 result)))))

;logaritmus
(defun tnum-ln (tnum)
  (lambda (eps)
    (multiple-value-bind (num eps0)
        (get-nonzero-num+eps tnum eps)
      (num-ln (tnum-to-num tnum eps) (* eps (- num eps0))))))

;tnum1 na tnum2-tou
(defun tnum-expt (tnum1 tnum2)
  (tnum-exp (tnum* tnum2 (tnum-ln tnum1))))

;tnum1-ta odmocnina tnum2
(defun tnum-root (tnum1 tnum2)
  (tnum-expt tnum2 (/tnum tnum1)))