set terminal pdf
set output "obr2.pdf"
unset key
unset xtics
unset ytics
set xtics ("x+(ε/f(x+ε))" 0.2125, "x-(ε/f(x-ε))" 0.1925)
set ytics ("f(x+(ε/f(x+ε)))" 4.7059, "f(x-(ε/f(x-ε)))" 5.1948, "f(x)+ε" 5.05, "f(x)-ε" 4.95)
unset border
set xtics nomirror
set ytics nomirror
set arrow 1 from 0.13,2 to 0.51,2
set arrow 2 from 0.13,2 to 0.13,7.85
set arrow 3 from 0.2,2 to 0.2,5 nohead
set arrow 4 from 0.13,5 to 0.2,5 nohead
set arrow 5 from 0.2125,2 to 0.2125,4.7059 nohead
set arrow 6 from 0.13,4.7059 to 0.2125,4.7059 nohead
set arrow 7 from 0.1925,2 to 0.1925,5.1948 nohead
set arrow 8 from 0.13,5.1948 to 0.1925,5.1948 nohead
set label "f(x)" at 0.2,5 center offset 1,1
plot [0.13:0.5] 1/x