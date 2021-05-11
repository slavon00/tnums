;USER-FUNCTIONS
;need to load tnums.lisp before using: (load "tnums.lisp") or (load "load.lisp") 
;funkce pro prevod tnumu na string o delce minimalne count
(defun tnum-to-string (tnum count)
  (let ((num (tnum-to-num tnum (1+ count))) (output ""))
    (when (< num 0) (setf output "-" num (- num)))
    (multiple-value-bind (digit rem)
        (floor num)
        (setf output (concatenate 'string output 
              (write-to-string digit) ".")
          num (* 10 rem)))
    (dotimes (i count (concatenate 'string output "..."))
      (multiple-value-bind (digit rem)
        (floor num)
        (setf output (concatenate 'string output 
              (write-to-string digit))
          num (* 10 rem))))))

;funkce pro inkrementaci tnumu o jednicku
(defun tnum-1+ (tnum)
  (tnum+ (num-to-tnum 1) tnum))

;funkce pro dekrementaci tnumu o jednicku
(defun tnum-1- (tnum)
  (tnum- tnum (num-to-tnum 1)))

;druha odmocnina
(defun tnum-sqrt (tnum)
  (tnum-root (num-to-tnum 2) tnum))

;zlaty rez
(defun tnum-phi ()
  (tnum/ (tnum-1+ (tnum-sqrt (num-to-tnum 5))) (num-to-tnum 2)))

;tnum1-ty logaritmus tnumu2
(defun tnum-log (tnum1 tnum2)
  (tnum/ (tnum-ln tnum2) (tnum-ln tnum1)))