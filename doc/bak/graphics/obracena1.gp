set terminal pdf
set output "obr1.pdf"
unset key
unset xtics
unset ytics
set xtics ("x" 0.2, "x+ε" 0.25, "x-ε" 0.15)
set ytics ("f(x)" 5, "f(x+ε)" 4, "f(x-ε)" 6.666, "f(x)+ε" 5.05, "f(x)-ε" 4.95)
unset border
set xtics nomirror
set ytics nomirror
set arrow 1 from 0.13,2 to 0.51,2
set arrow 2 from 0.13,2 to 0.13,7.85
set arrow 3 from 0.2,2 to 0.2,5 nohead
set arrow 4 from 0.13,5 to 0.2,5 nohead
set arrow 5 from 0.25,2 to 0.25,4 nohead
set arrow 6 from 0.13,4 to 0.25,4 nohead
set arrow 7 from 0.15,2 to 0.15,6.666 nohead
set arrow 8 from 0.13,6.666 to 0.15,6.666 nohead
plot [0.13:0.5] 1/x