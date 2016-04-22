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

globals [
  last-stop
  turn-angles   ;; stores the change of heading between locations
]

;; use a breed to record the locations at which walk stopped
breed [locations location]

;; and links to join them at the chosen aggregation level
directed-link-breed [steps step]

steps-own [
  divisor
  len
  turn-angle
]

locations-own [
  real-x ;; keep track of actual x-y distance from 0,0 for R plots
  real-y
]

to setup
  clear-all
  
  ask patches [ set pcolor white ]
  set turn-angles []

  create-locations 1 [
    setup-location
    set real-x xcor
    set real-y ycor
  ]
  ; make 2000 more locations...
  repeat 2000 [
    go
  ] 
end

to go
  ; get the last placed location to make a new one
  ask last-stop [
    ; make a turn first - this will store direction to next location!
    let turn 1000 ;; dummy value to force at least one random selection
    while [turn >= 360 or turn < -360] [ 
      set turn random-normal 0 stdev-angle
    ]
    set turn-angles lput turn turn-angles
    set heading heading + turn
    ;; make a new location at the current coordinates
    ;; new location will move to the next walk location
    make-new-location real-x real-y
  ]
end
  
  
to make-new-location [x y]
  hatch 1 [
    setup-location
    ;; now move to the next spot in the walk
    set real-x x + dx
    set real-y y + dy
    fd 1
  ]
end

to setup-location
  set shape "circle"
  set color orange
  set size 0.7
  set last-stop self
end

to show-aggregated-walk
  ; rub out any previously displayed links
  ask steps [
    die
  ]
  let this-loc location 0
  let next-loc location aggregation-length
  while [next-loc != nobody] [
    ask this-loc [
      let d distance next-loc
      face next-loc
      create-step-to next-loc [
        set color blue
        set len d
      ]  
    ]
    set this-loc next-loc
    set next-loc location ([who] of this-loc + aggregation-length)
  ]
  ask step 0 aggregation-length [
    set turn-angle 0
    ask other steps [
      let this-angle [heading] of end1
      let prev-angle 0
      ask end1 [
        set prev-angle [heading] of one-of in-step-neighbors
      ]
      set turn-angle subtract-angles this-angle prev-angle 
    ]
  ]
  update-plots
end

;; reporter to allow angles to be added correctly
to-report add-angles [a b]
  report (a + b) mod 360
end

;; reports the right-turn angle required to 
;; change from heading b to heading a
to-report subtract-angles [a b]
  let x 0
  ifelse a > b 
  [
    ifelse (a - b) > 180 
    [ set x a - b - 360 ]
    [ set x a - b ]
  ]
  [
    ifelse (a - b) < -180 
    [ set x a - b + 360 ]
    [ set x a - b ]
  ]
  report x
end
@#$#@#$#@
GRAPHICS-WINDOW
155
15
567
448
100
100
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
-100
100
-100
100
0
0
1
ticks
100.0

SLIDER
28
51
141
84
stdev-angle
stdev-angle
5
90
30
5
1
NIL
HORIZONTAL

BUTTON
77
14
140
47
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
9
151
141
184
show-aggregated-walk
show-aggregated-walk
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
586
17
919
226
Turn Angles
Turn Angle
Relative Frequency
-180.0
180.0
0.0
10.0
true
true
"clear-plot" ""
PENS
"agg-turn-angles" 1.0 1 -16777216 true "" "set-histogram-num-bars 18\nhistogram [remainder turn-angle 180] of steps"
"base-turn-angles" 1.0 0 -2674135 true "" "plot-pen-up\nplotxy plot-x-min plot-y-min\nplot-pen-down\nlet t-angles turn-angles\nlet breaks map [? * 20 - 160] n-values 18 [?] \nforeach breaks [\n  let mx ?\n  let cnt length filter [? < mx] t-angles / aggregation-length\n  set t-angles filter [? >= mx] t-angles\n  plotxy (? - 10) cnt\n]"

CHOOSER
3
100
141
145
aggregation-length
aggregation-length
1 2 3 4 5 6 8 10 12 15 20 30 40 50 75 100
11

PLOT
586
228
919
454
Aggregated Step Lengths
Length
Frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range 0 ceiling max [len] of steps\nset-histogram-num-bars 20\nhistogram [len] of steps"

@#$#@#$#@
## WHAT IS IT?

This model is discussed in Chapter 5 of 

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

and is intended to demonstrate the effect of aggregating consecutive steps of a correlated random walk on the distribution of step-by-step turn angles.  

When random walk turn angles are correlated over short time periods, the 'acceleration' in movement of a correlated random walk relative to a simple random walk falls away rapidly with time. This is because over relatively short numbers of steps, the turn angles effectively become random, as the correlation from step to step is lost. This effect is demonstrated in this model. 

## HOW TO USE IT

Clicking the `setup` button creates a 2000 step correlated random walk.  Steps in the walk are unit length, and the heading of the walk changes at each step according to a normal distribution, mean 0 and standard deviation set by the `st-dev-angle` parameter.

Now clicking `show-aggregated-walk` will show a walk based on sequences of walk steps as specified by the `aggregation-length` parameter setting.  The resulting aggregated walk will be displayed, and the associated turn angle distribution will be shown.

## THINGS TO NOTICE

Experiment with the `aggregation-length` setting and observe how even low sinuosity walks (those with `st-dev-angle` set to 5) begin to appear similar to simple random walks when a high `aggregation-length` is used.

## NETLOGO FEATURES

This model makes use of a turtle breed `locations` to store all the locations along a random walk of 2000 steps.  A link breed is used to display aggregated `steps` and perform calculations on them, which enables the walk to be re-aggregated to different `aggregation-length` settings as required.

The `setup` procedure makes a 2000 step correlated random walk by creating a `location` turtle at 0 0 and then calling the `go` procedure repeatedly.

The `go` procedure `hatches` a copy of the most recently created location which is then stored in the `last-stop` global variable.  The newly hatched `location` then changes its heading and moves forward a step, and becomes the new `last-stop`.

This creates a 2000 step correlated random walk marked out by `location` turtles.

When the `show-aggregated-walk` button is pressed a series of `step` links are made between `locations` at that separation.  This code

    let this-loc location 0
    let next-loc location aggregation-length
    while [next-loc != nobody] [
      ask this-loc [
        let d distance next-loc
        face next-loc
        create-step-to next-loc [
          set color blue
          set len d
        ]  
      ]
      set this-loc next-loc
      set next-loc location ([who] of this-loc + aggregation-length)
    ]

does this work, storing the length of each link and also reorienting locations to face the next one in the aggregated walk.  This allows the new statistics for the aggregated walk to be calculated.

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
NetLogo 5.1.0
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
