;USER-FUNCTIONS
;need to load tnums.lisp before
;funkce pro prevod tnumu na float
(defun tnum-to-float (tnum eps)
  (float (tnum-to-num tnum eps)))

;funkce pro ladeni -- vypisuje cisla o presnosti 1, 0.1, 0.01 ... 10^-n; pokud behem vypoctu dojde k chybe -- treba deleni nulou -- nevypise nic, vraci posledni vysledek jako racionalni cislo -- zlomek
(defun tnum-print-first-n (tnum n)
  (let ((result 0))
    (dotimes (i n result)
      (setf result (ignore-errors (tnum-to-num tnum (- 0 i 1))))
      (when result (format t "n = ~D, num = ~c~F~%"
        i #\tab
        (coerce result 'long-float))))))

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

;sinus tnumu i cisla
(defun tsin (arg)
  (when (realp arg)
    (setf arg (num-to-tnum arg)))
  (tnum-sin arg))

;scitani tnumu a cisel
(defun t+ (&rest args)
  (tnum+ (apply 'tnum+ (remove-if 'realp args))
    (num-to-tnum (apply '+  (remove-if-not 'realp args)))))

;nasobeni tnumu a cisel
(defun t* (&rest args)
  (tnum*num (apply 'tnum* (remove-if 'realp args))
    (apply '* (remove-if-not 'realp args))))

;kalkulackovy sinus vracejici long-float sinu cisla
(defun calc-sin (num)
  (coerce (tnum-to-num (tsin num) -20) 'long-float))

;nastaveni presnosti kalkulacky
(defvar *calc-eps* -6)
;nastaveni konverze z kalkulacky
(defvar *calc-conversion* (lambda (num) (float num)))

;sinus z nastavitelne kalkulacky
(defun global-sin (num)
  (funcall *calc-conversion* 
    (tnum-to-num (tsin num) *calc-eps*)))