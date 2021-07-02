set terminal pdf
set bmargin screen 0.07
set lmargin screen 0.12
set output "exp3.pdf"
unset key
set xrange [0.5:1.5]
set yrange [0:4]
set arrow 1 from 1,0 to 1,2.71 nohead lw 2
set arrow 2 from 0.5,2.71 to 1,2.71 nohead lw 2
set arrow 3 from 0.792,0 to 0.792,2.21 nohead lw 1
set arrow 4 from 0.5,2.21 to 0.792,2.21 nohead lw 1
set arrow 5 from 1.166,0 to 1.166,3.21 nohead lw 1
set arrow 6 from 0.5,3.21 to 1.166,3.21 nohead lw 1
unset xtics
unset ytics
set label "x" at 1,0 center offset 0,-1
set label "?" at 0.792,0 center offset 0,-1
set label "?" at 1.166,0 center offset 0,-1
set label "exp(x)" at 0.5,2.71 center offset -3,0
set label "exp(x)-ε" at 0.5,2.21 center offset -4,0
set label "exp(x)+ε" at 0.5,3.21 center offset -4,0
set arrow 7 from 0.5,0 to 0.5,4
set arrow 8 from 0.5,0 to 1.5,0
unset border
plot [0.5:1.5] exp(x)