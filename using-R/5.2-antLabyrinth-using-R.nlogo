;; The MIT License (MIT)
;;
;; Copyright (c) 2011-24 David O'Sullivan and George Perry
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to  permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;

extensions[sr]

turtles-own [
  startCell
  currCell
  Rhistory
  Rnow
]

patches-own [
  occupied?
]

to setup
  clear-all
  sr:setup

  ask patches [
    set occupied? false
    set pcolor black
    if random-float 1 <= p [
      set occupied? true
      set pcolor white
    ]
  ]
  place-ant
  reset-ticks
end

to place-ant
  ;; place the ant
  ask n-of ants patches with [occupied? = true] [
    sprout 1 [
      set size 2
      set shape "circle"
      set color red
      set currCell patch-here
      set startCell patch-here
      set Rhistory []
      set Rnow 0
    ]
  ]
end

to go
    move
    tick
end

to move
  ask turtles [
    ; if blind -> select one of N4 and move if occupied
    ; if myopic -> select one of occupied N4

    let new-loc currCell

    if antBehaviour = "blind" [
      let n one-of neighbors4
      if [occupied?] of n = true [set new-loc n]
    ]
    if antBehaviour = "myopic" [
      if any? neighbors4 with [occupied? = true] [
        set new-loc one-of neighbors4 with [occupied? = true]
      ]
    ]
    if new-loc != currCell [
      move-to new-loc
      ask new-loc [ set pcolor blue ]
      set currCell new-loc
    ]
    let newR eucl-distance-on-torus startCell [pxcor] of currCell [pycor] of currCell
    set RNow newR
    set Rhistory lput newR Rhistory
  ]
end

;; Euclidean distance **on a torus**  between patch p1 and point (x2,y2)
to-report eucl-distance-on-torus[p1 x2 y2]
  ;; See: http://www.swarm.org/pipermail/modelling/2005-October/003828.html
  let x1 [pxcor] of p1
  let y1 [pycor] of p1
  let xDist min list (abs (x1 - x2)) (max-pxcor - abs (x1 - x2))
  let yDist min list (abs (y1 - y2)) (max-pycor - abs (y1 - y2))
  report sqrt ( xDist ^ 2 + yDist ^ 2)
end

;; This plots (in R) a histogram of the current displacements
to r-displ-hist
  sr:set-agent-data-frame "agentlist1" turtles "Rnow"
  sr:set-plot-device
  sr:run "hist(agentlist1$R, main = '', xlab = 'Distance travelled', las = 1)"
  user-message "Plot will close when you close this dialog."
  sr:run "dev.off()"
end

;; This plots (in R) a time sequence of histograms of the displacements
;; Simple R extension seems to struggle with adjusting plot parameters in base R
;; so commenting this out for now
to r-displ-hist-ts
  let slice 1
  let t ticks
  let idx 0
  let interval t / 10
  let r-at-slice []
  sr:set-plot-device
  sr:run "old.par <- par()"
  sr:run "par(mfrow = c(2, 5))"
  while [slice <= 10] [
    set idx (slice * interval) - 1
    if idx > t [ set idx t ]  ;; make sure don't have list bounds error
    ask turtles [
       let r item idx Rhistory
       set r-at-slice lput r r-at-slice
    ]
    sr:set "r" r-at-slice
    sr:set "title" (word "Time = " round (idx + 1))
    sr:run "hist(r, main = title)"
    set slice slice + 1
  ]
  user-message "Plot will close when you close this dialog."
  sr:run "dev.off()"
end

;; This plots all of the individual R traces and overlays mean, median, ...
to r-displ-trace
  sr:set "n" count turtles
  sr:set "t" ticks
  sr:run "x <- 1:t"
  sr:run "traces <- matrix(0,  ncol = n, nrow = t)"
  ;; build a matrix in R with all of the traces in it (for matplot)
  ask turtles[
    sr:set "r" Rhistory
    sr:set "col" who
    sr:run "traces[,col] <- r"
  ]
  sr:set-plot-device
  sr:run "matplot(x = x, y = traces[,-n], col = 'grey', type = 'l', lty = 1, las = 1, xlab = 'Time', ylab = 'Distance travelled')"
  ;; the -n is to deal with indexing issues (min who = 1, but R is zero-indexed)
  sr:run "mn.disp <- apply(traces, 1, mean)"
  sr:run "md.disp <- apply(traces, 1, median)"
  sr:run "sd.disp <- apply(traces, 1, sd)"
  sr:run "lines(mn.disp ~ x, col = 'black', lwd = 2)"
  sr:run "lines(md.disp ~ x, col = 'red', lwd = 2)"
  sr:run "lines(mn.disp + sd.disp ~ x, col = 'black', lwd = 1)"
  sr:run "lines(mn.disp - sd.disp ~ x, col = 'black', lwd = 1)"
  user-message "Plot will close when you close this dialog."
  sr:run "dev.off()"
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
730
531
-1
-1
2.0
1
10
1
1
1
0
1
1
1
0
255
0
255
1
1
1
ticks
100.0

SLIDER
25
157
197
190
p
p
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
60
17
133
50
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
214
537
437
567
Occupied patches in white
12
0.0
1

BUTTON
139
55
202
88
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
117
431
187
476
Distance
mean [Rnow] of turtles
2
1
11

PLOT
737
10
1170
250
Distance Displaced
Time
Distance
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [Rnow] of turtles"
"pen-1" 1.0 0 -4539718 true "" "plot mean [Rnow] of turtles - standard-deviation [Rnow] of turtles"
"pen-2" 1.0 0 -4539718 true "" "plot mean [Rnow] of turtles + standard-deviation [Rnow] of turtles"
"pen-3" 1.0 0 -2674135 true "" "plot median [Rnow] of turtles"

CHOOSER
58
238
196
283
antBehaviour
antBehaviour
"blind" "myopic"
0

BUTTON
139
16
202
49
step
move
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
23
294
195
327
ants
ants
1
100
100.0
1
1
NIL
HORIZONTAL

BUTTON
744
267
885
300
R-displacements
r-displ-hist
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
55
194
205
224
Remember pc ~ 0.59 in a four-cell neighbourhood
11
0.0
1

BUTTON
746
308
886
341
R-displacements-ts
r-displ-hist-ts
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
747
347
833
380
R-traces
r-displ-trace
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This model is an implementation of Peter Gilles de Gennes 'ant in the labyrinth'.  See:

+   de Gennes PG 1976 La percolation: un concept unificateur. _La Recerche_ **7** 72–82.

This is model is discussed in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

An alternative version of this model that does not require the R-netlogo extension is also available.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   de Gennes PG 1976 La percolation: un concept unificateur. _La Recerche_ **7** 72–82.
+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-24 David O'Sullivan and George Perry

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to  permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
