;; The MIT License (MIT)
;;
;; Copyright (c) 2011-2016 David O'Sullivan and George Perry
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

turtles-own [
  real_x     ; the real x coord dist from origin ie ignoring wrapping around world
  real_y     ; the real y coord dist from origin ie ignoring wrapping around world
]

globals [
  world-edge ; used to detect a turtle hitting the edge, when run will stop
  dists      ; list of the distances from 0 0 of all turtles
  min-d      ; minimum distance from origin of any turtle
  mean-d     ; mean distance from origin of the turtles
  max-d      ; maximum distance from origin of any turtle
  rms-d      ; root mean square distance from origin of the turtles
  max-x
  min-x
  max-y
  min-y
]

to setup
  clear-all

  set max-x 0
  set min-x 0
  set max-y 0
  set min-y 0

  if use-random-seed? [
    random-seed seed-value
  ]

  ; make the world white
  ask patches [ set pcolor white ]
  create-turtles num-of-walkers [
    set color black
    set shape "circle" ; default shape is hard to see at size 1
    set real_x xcor
    set real_y ycor
  ]

  update-stats
  reset-ticks
end

; switches pen-mode to raise or lower pens of turtles
to toggle-tracks
  ask turtles [
    ifelse pen-mode = "up"
    [  set pen-mode "down" ]
    [ set pen-mode "up" ]
  ]
end

; make all turtles move
; update various monitors and plots
; stop model if anyone has reached the edge
to go
  ask turtles [ step ]
  update-stats
  tick
end

; update statistics and plots
to update-status-monitors
  update-stats
end

; select appropropriate step method based on the type-of-walk drop-down
to step
  if random-float 1 >= p-lazy [
    ;; only move from patch centre to patch centre
    if type-of-walk = "lattice" [
      face one-of neighbors4 ; easy!
      set real_x real_x + dx
      set real_y real_y + dy
      fd 1
    ]
    ; move unit distance in a uniform-random direction
    if type-of-walk = "simple" [
      set heading random-float 360
      set real_x real_x + dx
      set real_y real_y + dy
      fd 1
    ]
    ; move unit distance but direction is determined by turning
    ; from current direction
    if type-of-walk = "correlated directions" [
      rt random-normal 0 stdev-angle
      set real_x real_x + dx
      set real_y real_y + dy
      fd 1
    ]
    if type-of-walk = "normally distributed step length" [
      set heading random-float 360
      let step-length abs random-normal 0 (mean-step-length * sqrt (pi / 2))
      set real_x real_x + (dx * step-length)
      set real_y real_y + (dy * step-length)
      fd step-length
    ]
    if type-of-walk = "exponentially distributed step length" [
      set heading random-float 360
      let step-length random-exponential mean-step-length ; rate ;
      set real_x real_x + (dx * step-length)
      set real_y real_y + (dy * step-length)
      fd step-length
    ]
    if type-of-walk = "Cauchy distributed step lengths" [
      set heading random-float 360
      let step-length r-cauchy 0 1
      set real_x real_x + (dx * step-length)
      set real_y real_y + (dy * step-length)
      fd step-length
    ]
  ]
end

to-report r-cauchy [loc scl]
  let X (pi * (random-float 1)) ;; Netlogo tan takes degrees not radians
  report loc + scl * tan(X * (180 / pi))
end

; update various summary statistics for the walks
to update-stats
  with-local-randomness [
    set dists [sqrt(real_x ^ 2 + real_y ^ 2)] of turtles
    set max-d max dists
    set min-d min dists
    set mean-d mean dists
    ; root mean square distance is expected to equal sqrt(#steps) so calculate it
    set rms-d sqrt mean map [? * ?] dists

    let x [real_x] of turtles
    let y [real_y] of turtles
    set max-x max list max-x (max x)
    set min-x min list min-x (min x)
    set max-y max list max-y (max y)
    set min-y min list min-y (min y)
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
194
10
801
638
99
99
3.0
1
10
1
1
1
0
1
1
1
-99
99
-99
99
1
1
1
ticks
200.0

SLIDER
15
239
187
272
num-of-walkers
num-of-walkers
1
500
500
1
1
NIL
HORIZONTAL

CHOOSER
17
276
187
321
type-of-walk
type-of-walk
"lattice" "simple" "normally distributed step length" "exponentially distributed step length" "Cauchy distributed step lengths" "correlated directions"
0

BUTTON
122
12
185
45
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

BUTTON
122
51
185
84
step
go
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
122
166
185
199
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
1

BUTTON
12
11
116
44
NIL
toggle-tracks
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
812
10
1144
304
Distance from origin
Steps
Distance
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"all walkers" 1.0 2 -3026479 true "" "if ticks mod update-plot-every-x-ticks = 0 [\n  foreach dists [\n    plotxy ticks ?\n  ]\n]"
"rms-dist" 1.0 0 -16777216 true "" "if ticks mod update-plot-every-x-ticks = 0 [\n  foreach dists [\n    plotxy ticks rms-d\n  ]\n]"
"expected" 1.0 0 -2674135 true "" "if ticks mod update-plot-every-x-ticks = 0 [\n  ifelse type-of-walk = \"correlated directions\" [\n    ;; These alternative lines use an adjustment for the RMS dist for \n    ;; correlated walks from \n    ;; Bovet & Benhamou, J. theor. Biol. (1988) 131, 419-433\n    ;; itself derived from \n    ;; Random Flight with Multiple Partial Correlations, C. M. Tchen\n    ;; J. Chem. Phys. 20, 214 (1952); doi:10.1063/1.1700381 \n    ;; which shows that for large #ticks RMS-D = sqrt[(1+r)/(1-r)ticks]\n    ;; where r is a correlation angle between walk steps\n    ;; ONLY RELEVANT to the correlated case, so not used in general\n    let r 1 / exp (((stdev-angle * pi / 180) ^ 2) / 2) \n    ;plotxy ticks sqrt ((1 + r) / (1 - r) * ticks)\n    ;; Note that this is approximate: there is an additional correction\n    ;; required for low turn angles\n    ;; It is unclear whether the second term is in r^2 or r; see\n    ;; Hsin-i Wu, Bai-Lian Li, Timothy A. Springer, William H. Neill, \n    ;; Modelling animal movement as a persistent random walk in two dimensions: expected magnitude of net displacement\n    ;; Ecol Mod, 132(1-2), 115-124 DOI: 10.1016/S0304-3800(00)00309-4.\n    plotxy ticks sqrt (((1 + r) / (1 - r) * ticks) - ((2 * r * r * (1 - (r ^ ticks))) / (1 - r) / (1 - r)))\n  ]\n  [\n    plotxy ticks sqrt ticks\n  ]\n]"

MONITOR
929
352
986
397
NIL
max-d
3
1
11

MONITOR
870
352
927
397
NIL
mean-d
3
1
11

MONITOR
809
352
866
397
NIL
min-d
3
1
11

MONITOR
1068
352
1122
397
NIL
rms-d
3
1
11

SLIDER
16
360
188
393
stdev-angle
stdev-angle
0
90
17
1
1
NIL
HORIZONTAL

SWITCH
28
464
186
497
use-random-seed?
use-random-seed?
1
1
-1000

BUTTON
121
88
185
121
go-10
repeat 10 [ go ]
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
122
127
187
160
go-100s
repeat hundreds [ go ]
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
19
126
116
159
hundreds
hundreds
100
1000
500
100
1
NIL
HORIZONTAL

SLIDER
28
499
186
532
seed-value
seed-value
0
1000
500
1
1
NIL
HORIZONTAL

SLIDER
16
324
188
357
mean-step-length
mean-step-length
0.1
5
1
0.1
1
NIL
HORIZONTAL

SLIDER
810
313
966
346
update-plot-every-x-ticks
update-plot-every-x-ticks
1
100
20
1
1
NIL
HORIZONTAL

SLIDER
16
397
188
430
p-lazy
p-lazy
0
0.5
0
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model simulates a number of variations on the random walk as discussed in Chapter 4 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

An alternative version of this model is available which makes use of the R-netlogo extension to provide more useful plots of the model behaviour.

## HOW IT WORKS

Inspection of the model procedures will show that the behaviour is similar in all cases, the only difference from model to model being how the next 'step' in a random walk is determined, which is based on the `type-of-walk` drop-down selection choice.  The choices available are:

**lattice** which will produce a 2D lattice walk, where locations in the walk are restricted to integer (x, y) locations only.  In Netlogo terms this means that each step is performed by randomly facing `one-of neighbors4`, and then moving forward 1 unit.  This could be accomplished by a single `move-to one-of neighbors4` command, but instead the heading is changed, and `dx` and `dy` are used together with `fd 1` to allow `real_x` and `real_y` variables to be correctly updated (see below on  NETLOGO FEATURES for more on this).

**simple** when each walk step is a unit length step in a random direction.

**normally distributed step length** is the same as the simple case, except that the step length is chosen from a normal distribution.  Note that in this case, the `mean-step-length` parameter is interpreted as the standard-deviation of the normal distribution from which step lengths are drawn, 0 is the mean, and the absolute value of the normally distributed number is used for the step length.

**exponentially distributed step length** is the same as the simple case, except that the step length is chosen from an exponential distribution, with mean given by the `mean-step-length` parameter.

**Cauchy distributed step length** is the same as the simple case, except that the step length is chosen from a Cauchy distribution.  This produces a L&eacute;vy flight with heavy-tailed step length distribution.

**correlated directions** introduces non-independence between consecutive steps in a simple walk (i.e. equal unit step lengths). Instead of choosing a direction for each step completely at random, the direction of the next step results from a turn away from the current direction.  Turn angles are drawn from a normal distribution so that continuing in the same direction as the previous step is the single most likely outcome, and large changes in direction are very unlikely.  The sinuosity of the walk is governed by the `st-dev-angle` parameter which is the standard deviation in degrees of the turn angle distribution.


## HOW TO USE IT

Initialise using the `setup` button.  To turn on walk 'tracks' use the `toggle-tracks` button.

The number of walkers which will be created is controlled by the `num-of-walkers` slider, and the number of steps they will move is determined by using the `go`, `step`, `go-10`, and `go-100s` buttons.  If you want a specific number of steps in multiples of 100, use the `hundreds` slider and `go-100s` button.

## NETLOGO FEATURES

**`real_x` and `real_y` turtle variables**

Storage in turtle variables `real_x` and `real_y` of the actual distance moved from the origin, allowing for 'wrapping' around NetLogo's toroidal world.  This is important for calculation of meaningful distance statistics, and enables the R extension plots to show real world coordinates for all walk types.  The variable names have an underscore because R interprets NetLogo hyphenated variable names such as 'real-x' as minus signs.

**Use of `dx` and `dy` in the `step` procedure**

This also explains the use of dx and dy in the step procedure.  `dx` and `dy` report the x and y coordinate offsets associated with moving 1 unit in the current heading direction. This allows `real_x` and `real_y` to be correctly updated even when a walk is about to 'wrap' around the world.

**Use of `with-local-randomness`**

The `toggle-tracks` and `update-stats` procedures use `with-local-randomness` to avoid 'upsetting' the random number seed control.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-2016 David O'Sullivan and George Perry

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
NetLogo 5.3
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
