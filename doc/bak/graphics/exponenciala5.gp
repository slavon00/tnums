set terminal pdf
set bmargin screen 0.07
set lmargin screen 0.12
set output "exp5.pdf"
unset key
set xrange [0.9:1.266]
set yrange [2:4]
set arrow 1 from 1,2 to 1,2.71828 nohead lw 2
set arrow 2 from 0.9,2.71 to 1,2.71828 nohead lw 2
set arrow 5 from 1.166,2 to 1.166,3.21 nohead lw 1
set arrow 6 from 0.9,3.21 to 1.166,3.21 nohead lw 1
unset xtics
unset ytics
set label "x" at 1,2 center offset 0,-1
set label "x+w" at 1.166,2 center offset 0,-1
set label "exp(x)" at 0.9,2.71 center offset -3,0
set label "exp(x)+ε" at 0.9,3.21 center offset -4,0
set arrow 7 from 0.9,2 to 0.9,4
set arrow 8 from 0.9,2 to 1.266,2
unset border
plot [0.9:1.266] exp(x)