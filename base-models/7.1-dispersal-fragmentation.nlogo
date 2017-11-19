;; The MIT License (MIT)
;;
;; Copyright (c) 2011-2018 David O'Sullivan and George Perry
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

globals [
  cover
  area
  id
  start-id

  step-list
  n-released
  n-exit
  n-died
  n-success

  cardinals
  shift-list
]

patches-own [
  patch-id

  sq-blocked?
  ew-blocked?
  ns-blocked?

  habitat?
  low-quality?
]

to setup
  clear-all

 ;; r:setPlotDevice

  set cover 0
  set id 1
  set area count patches
  ask patches [
    set sq-blocked? false
    set ew-blocked? false
    set ns-blocked? false
    set habitat? false
    set low-quality? false
  ]
  let space-remaining? true
  while [space-remaining? and cover / area * 100 < percent-cover] [
    ifelse patch-shape = "square" ;; random 3 = 0
    [ make-square
      set space-remaining? any? patches with [not sq-blocked?]
    ]
    [ make-line
      set space-remaining? any? patches with [not ew-blocked? or not ns-blocked?]
    ]
    set id id + 1
  ]

  let actual-cover cover / count patches * 100
  if actual-cover < percent-cover [
    user-message (word "Actual cover is only " (precision actual-cover 1)
                       "% which is less than requested")
  ]
  set step-list []

  set n-success 0
  set n-released 0
  set n-exit 0
  set n-died 0

  set cardinals map [ x -> x * 45 ] range 8
  if correlated?
  [
    let n-opt correl-angle / 90
    set shift-list map [ x -> x - n-opt ] range ((n-opt * 2 ) + 1)
  ]

  reset-ticks
end

to search
  while [(n-success < n-walkers) and (n-released < 1e5)] [
    set n-released n-released + 1
    make-walker
    ask turtles [ walk ]
  ]

end

to make-walker
  ask one-of patches with [habitat? = true] [
    let focal-id patch-id
    sprout 1 [
      set color yellow
      set heading one-of cardinals
    ]
    ask patches in-radius (object-size + 1) with [patch-id = focal-id] [
      set start-id patch-id
      set pcolor lime
    ]
  ]
end

to walk
 pendown
 let steps 1
 let found-habitat false
 let mortality? false
 let exited? false

 while [found-habitat = false and steps <= max-steps and mortality? = false and exited? = false]
 [
  ;; mortality at step?
  ifelse random-float 1 >= survival-rate
  [
    set n-died n-died + 1
    set mortality? true
  ]
  [
    let new-hd 0
    ifelse correlated? = false
     [ set new-hd one-of cardinals ]
     [ set new-hd get-correl-direction ]

    let new-location patch-at-heading-and-distance new-hd step-length

    ifelse new-location != nobody
    [
      set heading new-hd
      fd step-length
    ]
    [
      set n-exit n-exit + 1
      set exited? true
    ]

    set found-habitat test-settlement

    set steps steps + 1
  ]
 ]

 ;; stopped with fewer than max-steps and not died or left
 if steps != max-steps and mortality? = false and exited? = false
 [
   set n-success n-success + 1
   set step-list lput steps step-list
 ]

 die
end

to-report test-settlement
  let test false
  let focal-id patch-id

  ifelse prop-low-quality = 0
  [
    if ([habitat?] of patch-here = true and focal-id != start-id)
    [ set test true ]
  ]

  [
    if ([habitat?] of patch-here = true and focal-id != start-id)
    [
      let p-set patches with [patch-id = focal-id]
      let p-quality (count p-set with [low-quality? = true]) / count p-set

      ;; empirical settling rules from R et al. 1997
      if p-quality <= 0.1 and random-float 1 <= 0.83 [set test true]
      if p-quality > 0.1 and p-quality <= 0.2 and random-float 1 <= 0.55 [set test true]
      if p-quality > 0.2 and p-quality <= 0.3 and random-float 1 <= 0.40 [set test true]
    ]
  ]
  report test
end

;; ============
;; Code to make the landscape
to make-region [x y]
  foreach range x [ d-x ->
    foreach range y [ d-y ->
      let mark-patch patch-at d-x d-y
      if mark-patch != nobody
      [
          ask mark-patch [ mark ]
      ]
    ]
  ]
end

to make-square
  ifelse any? patches with [not sq-blocked?] [
    let possible-spots patches with [not sq-blocked?]
    ask one-of possible-spots [
      make-region object-size object-size
    ]
  ]
  [ show "No patches available for square" ]
end

to make-line
  ifelse random 2 = 1
  [ make-ew-line ]
  [ make-ns-line ]
end

to make-ew-line
  ifelse any? patches with [not ew-blocked?] [
    let possible-spots patches with [not ew-blocked?]
    ask one-of possible-spots [
      make-region (object-size ^ 2) 1
    ]
  ]
  [ show "No patches available for E-W line" ]
end

to make-ns-line
  ifelse any? patches with [not ns-blocked?] [
    let possible-spots patches with [not ns-blocked?]
    ask one-of possible-spots [
      make-region 1 (object-size ^ 2)
    ]
  ]
  [ show "No patches available for N-S line" ]
end

to mark
  block-squares
  block-ew-lines
  block-ns-lines
  set pcolor white
  set cover cover + 1
  set habitat? true
  set patch-id id
  if random-float 1 <= prop-low-quality [
    set low-quality? true
    set pcolor grey
  ]
end

to block-squares
  let offsets map [ x -> x - object-size ] range (object-size + 2)
  foreach offsets [ d-x ->
    foreach offsets [ d-y ->
      let tag-patch patch-at d-x d-y
        if tag-patch != nobody [
          ask tag-patch [ set sq-blocked? true ]
        ]
     ]
  ]
end

to block-ew-lines
  foreach map [ x -> x - (object-size ^ 2) ] range ((object-size ^ 2) + 2) [ d-x ->
    foreach map [ x -> x - 1 ] range 3 [ d-y ->
      let tag-patch patch-at d-x d-y
      if tag-patch != nobody [
        ask tag-patch [ set ew-blocked? true ]
      ]
    ]
  ]
end

to block-ns-lines
  foreach map [ x -> x - (object-size ^ 2) ] range ((object-size ^ 2) + 2) [ d-y ->
    foreach map [ x -> x - 1 ] range 3 [ d-x ->
      let tag-patch patch-at d-x d-y
      if tag-patch != nobody [
        ask tag-patch [ set ns-blocked? true ]
      ]
    ]
  ]
end

to-report success-rate
  ifelse n-released != 0
   [ report n-success / (n-released - n-exit) ]
   [ report 0 ]
end

to-report mean-steps-taken
  let mn -999
  if length step-list > 0
    [ set mn mean step-list]
  report mn
end

to-report sd-steps-taken
  let sd -999
  if length step-list > 0
    [ set sd standard-deviation step-list]
  report sd
end

to set-shape-num [s]
  ifelse s = 0
  [ set patch-shape "line" ]
  [ set patch-shape "square" ]
end

to reset-to-baseline
  set prop-low-quality 0
  ;; set-shape-num 1
  set step-length 1
  set survival-rate 0.997
  set correlated? false
end

to-report get-correl-direction
  let curr-hd position heading cardinals

  let new-hd curr-hd + one-of shift-list

  if new-hd >= 8 [set new-hd new-hd - 8]
  if new-hd < 0 [set new-hd new-hd + 8]

  report item new-hd cardinals
end
@#$#@#$#@
GRAPHICS-WINDOW
225
10
633
419
-1
-1
4.0
1
10
1
1
1
0
0
0
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

BUTTON
25
10
92
43
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

SLIDER
9
99
181
132
object-size
object-size
1
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
10
59
182
92
percent-cover
percent-cover
0
30
25.0
0.1
1
NIL
HORIZONTAL

CHOOSER
30
139
168
184
patch-shape
patch-shape
"line" "square"
1

BUTTON
98
10
167
43
NIL
search
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
13
194
185
227
n-walkers
n-walkers
0
500
200.0
10
1
NIL
HORIZONTAL

MONITOR
16
276
101
321
Prop success
success-rate
3
1
11

SLIDER
17
370
189
403
survival-rate
survival-rate
0
1
0.997
.001
1
NIL
HORIZONTAL

SLIDER
16
442
188
475
prop-low-quality
prop-low-quality
0
.16
0.05
.01
1
NIL
HORIZONTAL

SLIDER
17
405
189
438
step-length
step-length
0.5
1.5
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
106
277
183
322
Mean steps
mean-steps-taken
3
1
11

MONITOR
69
324
131
369
SD steps
sd-steps-taken
3
1
11

SLIDER
14
232
186
265
max-steps
max-steps
100
1000
400.0
50
1
NIL
HORIZONTAL

SWITCH
15
482
118
515
correlated?
correlated?
1
1
-1000

SLIDER
120
482
212
515
correl-angle
correl-angle
90
270
90.0
90
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This implementats the model described in:

+    Ruckelshaus M, Hartway C and Kareiva PM 1997 Assessing the data requirements of spatially explicit dispersal models. _Conservation Biology_, **11**, 1298-1306.

which is analysed in Chapter 7 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

to demonstrate aspects of model sensitivity analysis.  You should consult the book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+    Ruckelshaus M, Hartway C and Kareiva PM 1997 Assessing the data requirements of spatially explicit dispersal models. _Conservation Biology_, **11**, 1298-1306.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-2018 David O'Sullivan and George Perry

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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-surv" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>search</go>
    <exitCondition>(n-success &gt;= n-walkers) or (n-released &gt; 5e4)</exitCondition>
    <metric>success-rate</metric>
    <enumeratedValueSet variable="correlated?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-length">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-steps">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="survival-rate">
      <value value="0.997"/>
      <value value="0.8973"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-shape">
      <value value="&quot;square&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-walkers">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-low-quality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="correl-angle">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="object-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cover">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
1
@#$#@#$#@
