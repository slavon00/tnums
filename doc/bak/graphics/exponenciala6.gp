set terminal pdf
set bmargin screen 0.07
set lmargin screen 0.12
set output "exp6.pdf"
unset key
set xrange [0.95:1.216]
set yrange [2.5:3.3]
unset xtics
unset ytics
set arrow 1 from 1,2.71828 to 1.166,2.71828 nohead lw 1
set arrow 2 from 1.166,2.71828 to 1.166,3.21 nohead lw 1
set arrow 3 from 1.138,3.12 to 1.166,3.1 nohead lw 1
set arrow 4 from 0.95,2.5 to 0.95,3.3
set arrow 5 from 0.95,2.5 to 1.216,2.5
set label "w" at 1.1,2.71 center offset 0,-0.5
set label "ε" at 1.166,2.9 center offset 1,0
set label "α" at 1.166,3.21 center offset -1,-1
set xtics ("x" 1, "x+w" 1.166)
set ytics ("exp(x)" 2.71828, "exp(x)+ε" 3.21)
unset border
set xtics nomirror
set ytics nomirror
plot [0.95:1.216] exp(x)