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


globals [
  n-unhappy       ;; the number of unhappy turtles
  none-moved?     ;; a flag to indicate if anyone moved this tick and stop the model
  vacancies       ;; a patch-set of vacant locations (for implementation efficiency)
  color-1
  color-2
]

patches-own [
  color-1-neighbours ;; a count of neighbouring patches with color-1 occupants
  color-2-neighbours ;; a count of neighbouring patches with color-2 occupants
]

to setup
  clear-all

  set color-1 yellow
  set color-2 sky

  ask patches [
    set pcolor grey + 1
    set color-1-neighbours 0
    set color-2-neighbours 0
  ]

  if use-seeds? [ random-seed setup-seed ]

  set-default-shape turtles "circle"

  ;; create a number of turtles dictated by honouring the vacancy rate setting
  ask n-of ((1 - vacancy-rate) * count patches) patches [
    sprout 1 [
      set size 0.85
      ;; make them either color-1 or color-2 depending on the slider setting
      ifelse random-float 1 < proportion-color-1
      [
        set color color-1
        ask neighbors [ set color-1-neighbours color-1-neighbours + 1 ]
      ]
      [
        set color color-2
        ask neighbors [ set color-2-neighbours color-2-neighbours + 1 ]
      ]
    ]
  ]
  ;; initialize the vacancies patch-set
  set vacancies patches with [not any? turtles-here]
  if use-seeds? [ random-seed go-seed ]
  set none-moved? false
  reset-ticks
end


to go
  ;; check if we have hit the specified time limit
  ;; 0 is 'no limit'
  if not (time-limit = 0) [
    if ticks = time-limit [ stop ]
  ]
  ;; stop if nobody moved last tick
  ifelse none-moved? [
    set none-moved? false ;; this will allow model to start again if conditions are changed
    stop
  ]
  [ ;; reset the none-moved? flag
    set none-moved? true
    ask turtles [
      ifelse unhappy? [
        ;; get a list of acceptable new locations
        let acceptable-sites get-acceptable-sites self
        ;; move to the first of these -- this will be either random
        ;; or the nearest, depending on 'consider-distance?' setting
        if length acceptable-sites > 0 [
          relocate-to first acceptable-sites
        ]
      ]
      [ ;; there is also random background movement
        if random-float 1 < background-movement-rate [
          relocate-to one-of vacancies
        ]
      ]
    ]
  ]
  tick
end


to relocate-to [t]
  update-neighbourhood-counts color true
  set vacancies (patch-set vacancies patch-here)
  move-to t
  update-neighbourhood-counts color false
  let p patch-here
  set vacancies vacancies with [self != p]
  set none-moved? false
end


;; update patch counts
;; leaving? true indicates turtle of color col is leaving this patch
;; leaving? false indicates turtle of color col is arriving at this patch
to update-neighbourhood-counts [col leaving?]
  ifelse col = color-1 [
    ifelse leaving?
    ;; reduce neighbouring color-1-neighbours counts
    [ ask neighbors [ set color-1-neighbours color-1-neighbours - 1 ] ]
    ;; else increase it
    [ ask neighbors [ set color-1-neighbours color-1-neighbours + 1 ] ]
  ]
  [ ;; color-2 turtle
    ifelse leaving?
    ;; reduce neighbouring color-2-neighbours counts
    [ ask neighbors [ set color-2-neighbours color-2-neighbours - 1 ] ]
    ;; else increase it
    [ ask neighbors [ set color-2-neighbours color-2-neighbours + 1 ] ]
  ]
end

;; convenience reporter determining happy or not state
to-report unhappy?
  report proportion-like-me patch-here self < required-like-me
end


;; implements a variety of options for searching for possible relocation sites
to-report get-acceptable-sites [ttl]
  let ok-sites patch-set nobody
  ask ttl [
    ;; only the 'best' vacancies will do, ie. those with most similar neighbours
    if choice-mode = "best-available" [
      set ok-sites vacancies with-max [proportion-like-me self myself]
    ]
    ;; only sites with more like neighbours than the current location will do
    if choice-mode = "better-than-current" [
      let p-here proportion-like-me patch-here self
      set ok-sites vacancies with [proportion-like-me self myself > p-here ]
    ]
    ;; all vacancies better than the set threshold are considered
    if choice-mode = "better-than-required" [
      set ok-sites vacancies with [proportion-like-me self myself > required-like-me]
    ]
    if choice-mode = "any" [
      set ok-sites vacancies
    ]
  ]
  ;; if we are paying attention to distance, then sites should be sorted by
  ;; distance to the current location (i.e. distance ttl)
  ;; otherwise shuffle the list to ensure random order and make it into a list
  ifelse consider-distance?
  [ report sort-on [distance ttl] ok-sites ]
  [ report shuffle sort ok-sites ]
end


;; for the patch p and turtle t determine the proportion
;; of same coloured neighbours.  Note that t need not be
;; currently on p, so this can be used to 'look ahead'
to-report proportion-like-me [p t]
  let n color-1-neighbours + color-2-neighbours
  if n = 0 [ report 0 ]
  ifelse [color] of t = color-1
  [ report color-1-neighbours / n ]
  [ report color-2-neighbours / n ]
end


;; R plotting code - a little different so retained for ease of implementation
;;to snapshot-R
;;  r:put "wx" map [[pxcor] of ?] sort turtles with [color = color-1]
;;  r:put "wy" map [[pycor] of ?] sort turtles with [color = color-1]
;;  r:put "bx" map [[pxcor] of ?] sort turtles with [color = color-2]
;;  r:put "by" map [[pycor] of ?] sort turtles with [color = color-2]
;;
;;  r:eval("plot(wx,wy,asp=1,pch=1,axes=F,xlab='',ylab='')")
;;  r:eval("points(bx,by,pch=19)")
;;end
@#$#@#$#@
GRAPHICS-WINDOW
185
10
673
499
-1
-1
8.0
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
59
0
59
0
0
1
ticks
30.0

SLIDER
31
124
176
157
vacancy-rate
vacancy-rate
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
9
160
176
193
proportion-color-1
proportion-color-1
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
113
11
176
44
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
113
48
176
81
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
112
85
175
118
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

SLIDER
31
223
176
256
required-like-me
required-like-me
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
32
275
175
308
setup-seed
setup-seed
0
1000
501.0
1
1
NIL
HORIZONTAL

SLIDER
31
312
175
345
go-seed
go-seed
0
1000
500.0
1
1
NIL
HORIZONTAL

SWITCH
56
352
174
385
use-seeds?
use-seeds?
0
1
-1000

MONITOR
101
393
174
438
n-unhappy
count turtles with [unhappy?]
0
1
11

CHOOSER
691
13
855
58
choice-mode
choice-mode
"best-available" "better-than-current" "better-than-required" "any"
2

MONITOR
718
133
813
178
mean-prop-like
mean [proportion-like-me patch-here self] of turtles
3
1
11

SLIDER
689
357
876
390
background-movement-rate
background-movement-rate
0
0.1
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
704
394
876
427
time-limit
time-limit
0
1000
0.0
1
1
NIL
HORIZONTAL

SWITCH
690
69
865
102
consider-distance?
consider-distance?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model is a possible implementation of a 'Schelling-style' segregation model, as described variously in

+   Schelling TC 1969 Models of segregation. _American Economic Review_ **59** 488–493.
+   Schelling TC 1971 Dynamic models of segregation. _Journal of Mathematical Sociology_ **1** 143–186.
+   Schelling TC 1978 _Micromotives and Macrobehavior_. Norton, New York.

and discussed in Chapter 3 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## THINGS TO NOTICE

Schelling models are surprisingly hard to implement, considering how frequently they are reported as being among the simplest of agent models.  Part of the problem is that there is no canonical version of the model.   This implementation therefore provides a few options.

The other difficulty is that households must 'look ahead' to check potential new locations so as to be able to compare them to their existing location, and the calculations must be updated every time a household moves.  Optimizing model implementation in the face of this is fiddly, and involves some coding complications that seem unnecessary, but which can strongly affect model efficiency.

In this model, the major optimization is that each patch maintains counts of its `white-neighbours` and `black-neighbours`.  These counts are appropriately updated when a household moves as part of the `relocate-to` procedure.  All this effort is expended so that the `proportion-like-me` reporter for a patch only needs to perform the calculation

    white-neighbours / (white-neighbours + black-neighbours)

or its equivalent.  This is much faster than repeatedly invoking a

    count turtles-on neighbours with [color = white]

reporter.

Another way the model is designed to run efficiently is by maintaining a patch-set `vacancies` of those sites with no household present.  Without this feature there would be repeated need to invoke

    ask patches with [not any? turtles-here]

which would slow things down considerably.

Finally it is worth noting the use of the `proportion-like-me` _patch_ _turtle_ reporter. This allows households to look ahead when they are considering a relocation. To determine the local proportion like, we have

    let proportion-here proportion-like-me patch-here self

and to check out some other patch we have

    let proportion-there proportion-like-me some-patch self

This reporter is used in several places in the model code.

Another minor point of note is that we implement a `get-acceptable-sites` reporter. This means that the main `go` procedure has access to all the possible sites to which a household might move, arranged in a list, either from nearest to farthest, or in random order. The relocation occurs to the first site in the list. Reporting the whole list makes of acceptable sites makes it easy to add alternative selection methods for the final choice of relocation destination. **Try it!**

## THINGS TO TRY

As noted in the book, while qualitative outcomes in Schelling models are very stable, exact outcomes can be surprisingly sensitive to details of how households make decisions about their preferred locations.

A good experiment is to investigate how the final `mean-prop-like` statistic is affected by the various `choice-mode` option settings. For completeness, the choice modes are

+ `best-available` one vacancy with the highest `proportion-like-me` will be selected
+ `better-than-current` a vacancy must have a higher `proportion-like-me` than the current location to be selected
+ `better-than-required` a vacancy must have a higher `proportion-like-me` than the `required-like-me` parameter setting
+ `any` allows households to move to any available vacancy

Think about which of these settings might be expected to produce the strongest segregation most quickly, and run experiments to test your ideas.

It is instructive to see what levels of segregation result when the `any` setting is selected, because in this situation, the driver of segregation is solely moves out of unacceptable sites (households can even move to 'worse' locations, although they won't stay in them). A large component of the Schelling model's operation depends on this 'negative' driver of change, rather than 'positive choices' to move into locations with locally high proportions of like neighbours.

Finally, it is also instructive to experiment with the effect of introducing switching the `consider-distance?` setting. With this setting true, households move to the nearest acceptable site

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

You should also cite appropriate examples of the Schelling model, although it is hard to be specific about which as there are numerous versions.

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
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [proportion-like-me patch-here self] of turtles</metric>
    <enumeratedValueSet variable="choice-mode">
      <value value="&quot;best-available&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-seeds?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="required-like-me" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="proportion-white">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-limit">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background-movement-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vacancy-rate">
      <value value="0.15"/>
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
0
@#$#@#$#@
