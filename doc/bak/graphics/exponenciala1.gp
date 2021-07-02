set terminal pdf
set bmargin screen 0.07
set lmargin screen 0.12
set output "exp1.pdf"
unset key
set xrange [-1:1.5]
set yrange [0:4]
set arrow 1 from 1,0 to 1,2.71 nohead lw 2
set arrow 2 from -1,2.71 to 1,2.71 nohead lw 2
set arrow 3 from 0.8,0 to 0.8,2.225 nohead lw 1
set arrow 4 from -1,2.225 to 0.8,2.225 nohead lw 1
set arrow 5 from 1.2,0 to 1.2,3.32 nohead lw 1
set arrow 6 from -1,3.32 to 1.2,3.32 nohead lw 1
unset xtics
unset ytics
set label "x" at 1,0 center offset 0,-1
set label "x-ε" at 0.8,0 center offset 0,-1
set label "x+ε" at 1.2,0 center offset 0,-1
set label "exp(x)" at -1,2.71 center offset -3,0
set label "exp(x-ε)" at -1,2.225 center offset -4,0
set label "exp(x+ε)" at -1,3.32 center offset -4,0
set arrow 7 from -1,0 to -1,4
set arrow 8 from -1,0 to 1.5,0
unset border
plot [-1:1.5] exp(x)