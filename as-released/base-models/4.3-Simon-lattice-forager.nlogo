; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

;; Loosely based on 
;; Simon, H. A. (1956). Rational choice and the structure of the environment. 
;; Psychological Review, 63(2), 129-138. American Psychological Association. 
;; Retrieved from http://www.ncbi.nlm.nih.gov/pubmed/13310708

globals [
  area-per-step
  search-durations
  search-color
]

breed [walkers walker]
breed [spots spot]
breed [targets target]

walkers-own [
  energy
  spotted-target
  search-duration
]



to setup 
  clear-all
  
  set area-per-step []
  set search-durations []
  set search-color grey + 3
  
  set-default-shape spots "circle"
  ask patches [
    set pcolor white
  ]
  ask n-of n-targets patches [
    sprout-targets 1 [ 
      set color black
      set shape "target"
    ]
  ]
  ask one-of patches [
    sprout-walkers 1 [
      set size 2 ;; easier to see
      set color red
      set energy max-energy
      set search-duration 0
      set spotted-target nobody
      face one-of neighbors4
    ]
    sprout-spots 1 [
      set size 1
      ;set shape "dot"
      set color orange + 2
    ]
  ]
  reset-ticks
end


to go
  ifelse any? walkers [
    ask patches with [pcolor = search-color] [
      set pcolor white
    ]
    ask walkers [
      execute-search
    ]
    tick
  ]
  [
    stop
  ]
end


to execute-search
  set search-duration 0
  while [true] [
    let s one-of spots-here
    let searched-patches patches with [pcolor = search-color]
    if any? targets-on searched-patches [ 
      set spotted-target min-one-of (targets-on searched-patches) [manhattan-dist-to myself]
    ]
    ifelse spotted-target = nobody [
      if random-float 1 < p-direction-change [
        face one-of neighbors4
      ]
      jump 1
      set energy energy - 1
      mark-path
    ]
    [
      ifelse any? targets-here [
        replenish-energy-and-reset
        stop
      ]
      [
        move-towards-target spotted-target
        mark-path
      ]
    ]
    ifelse energy <= 0 [
      die 
    ]
    [
      update-search-area
      set search-duration search-duration + 1
    ]
    display
  ]
end

to move-towards-target [t]
  let next-location min-one-of neighbors4 [manhattan-dist-to t]
  face next-location
  jump 1
  set energy energy - 1
end

to replenish-energy-and-reset
  set area-per-step fput (count patches with [pcolor = search-color] / search-duration) area-per-step
  set search-durations fput search-duration search-durations
  set energy max-energy
  set spotted-target nobody
  ask targets-here [
    move-to one-of patches with [not any? targets-here]
  ]
  mark-path
end


to mark-path
  if count spots <= path-to-show [
    hatch 1 [
      set breed spots
      set size 1
      set color orange + 2
    ]
  ]
  if count spots > path-to-show [
    foreach sublist sort spots 0 (count spots - path-to-show) [
      ask ? [ 
        die
      ]
    ]
  ]
end


to update-search-area
  ask patches in-radius (vision + 0.5) with [manhattan-dist-to myself < vision] [
    set pcolor search-color
  ]
end


;; note that pt can be a patch or a turtle
;; since pxcor/pycor of turtle is that of the patch it occupies
to-report manhattan-dist-to [pt]
  let diff-x abs (pxcor - [pxcor] of pt)
  if diff-x > (world-width / 2) [
    set diff-x world-width - diff-x
  ]  
  let diff-y abs (pycor - [pycor] of pt)
  if diff-y > (world-height / 2) [
    set diff-y world-height - diff-y
  ]  
  report diff-x + diff-y
end
@#$#@#$#@
GRAPHICS-WINDOW
185
12
595
443
-1
-1
4.0
1
8
1
1
1
0
1
1
1
0
99
0
99
1
1
1
ticks
100.0

SLIDER
6
194
178
227
vision
vision
1
25
15
1
1
NIL
HORIZONTAL

SLIDER
6
233
178
266
max-energy
max-energy
100
5000
1000
100
1
NIL
HORIZONTAL

BUTTON
112
14
175
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
55
138
176
171
search-continuously
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
55
101
176
134
search-once
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

MONITOR
603
67
675
112
p
n-targets / count patches
5
1
11

SLIDER
6
51
176
84
n-targets
n-targets
1
20
20
1
1
NIL
HORIZONTAL

SLIDER
6
326
178
359
path-to-show
path-to-show
0
1000
100
10
1
NIL
HORIZONTAL

SLIDER
6
289
178
322
p-direction-change
p-direction-change
0
1
0.25
0.01
1
NIL
HORIZONTAL

PLOT
605
126
882
272
search duration
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 10.0 1 -16777216 true "" "if length search-durations > 0 [\n  set-plot-x-range 0 ceiling (max search-durations / 10) * 10\n  set-histogram-num-bars 20\n]\nhistogram search-durations"

MONITOR
603
13
697
58
area-searched
count patches with [pcolor = search-color]
0
1
11

MONITOR
815
11
872
56
energy
[energy] of one-of walkers
0
1
11

MONITOR
701
13
779
58
search-time
[search-duration] of one-of walkers
0
1
11

PLOT
605
277
880
443
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if length area-per-step > 0 [\n  set-plot-x-range 0 ceiling max area-per-step\n  set-histogram-num-bars 20\n  histogram area-per-step\n]"

@#$#@#$#@
## WHAT IS IT?

This model is a loose _spatial_ implementation of that described in

+   Simon HA 1956 Rational choice and the structure of the environment. _Psychological Review_ **63** 129-138.

and is discussed in detail in Chapter 4 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Simon HA 1956 Rational choice and the structure of the environment. _Psychological Review_ **63** 129-138.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-13 David O'Sullivan and George L. W. Perry

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact David O'Sullivan at d.osullivan@auckland.ac.nz, or George Perry at george.perry@auckland.ac.nzThis section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

half line 2
true
0
Polygon -7500403 true true 150 150 135 180 165 180 150 150
Line -7500403 true 150 165 150 300

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
NetLogo 5.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <steppedValueSet variable="vision" first="5" step="5" last="25"/>
    <enumeratedValueSet variable="p-direction-change">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-targets">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-memory">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <steppedValueSet variable="vision" first="5" step="5" last="25"/>
    <enumeratedValueSet variable="p-direction-change">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-targets">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-memory">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
