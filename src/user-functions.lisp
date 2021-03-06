;USER-FUNCTIONS
;need to load tnums.lisp before using: (load "tnums.lisp") or (load "load.lisp")
;--------------
(defun tnum-to-string (tnum count)
  (let ((num (tnum-to-num tnum (1+ count))) (output ""))
    (flet ((get-digit (&optional (end ""))
             (multiple-value-bind (digit rem) (floor num)
               (setf output (concatenate 'string output 
                                         (write-to-string digit)
                                         end)
                     num (* 10 rem)))))
      (when (< num 0) (setf output "-" num (- num)))
      (get-digit ".")  
      (dotimes (i count (concatenate 'string output "..."))
        (get-digit )))))

(tnum-to-string (tnum-pi) 767)

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