;PACKAGE TNUMS 
;AUTHOR:    slavon00
;INSTITUTE: UPOL/PRF/KI
;prevod cisla na tnum
(defun num-to-tnum (num)
  (lambda (eps)
    (declare (ignore eps))
    (rationalize num)))

;prevod tnum na cislo
(defun tnum-to-num (tnum eps)
  (when (or (>= 0 eps) (<= 1 eps))
    (setf eps (expt 10 (- (abs eps)))))
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
  (loop for n from 0
    until (< (abs (funcall nth n)) eps)
    finally (return n)))

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

;vycisleni nenuloveho tnumu
(defun get-nonzero-tnum+eps (tnum eps)
  (let ((num (tnum-to-num tnum eps)))
    (if (and (zerop num) (< (abs num) eps))
      (get-nonzero-tnum+eps tnum (/ eps 10))
      (values num eps))))

;obraceni tnumu
(defun /tnum (tnum)
  (lambda (eps)
    (multiple-value-bind (num eps0)
      (get-nonzero-tnum+eps tnum eps)
      (let* ((absnum (abs num))
          (neweps (* eps absnum (- absnum eps0))))
        (/ (if (> neweps eps) num
          (get-nonzero-tnum+eps tnum neweps)))))))

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