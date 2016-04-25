; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

;;extensions [r]
__includes ["distributions.nls"]

breed [trees tree]

globals [
  ndf-at-tick
  
  update-approx
  dnorm-lookup
  grain
]

trees-own [
  nhbs  
  n-nhbs
  comp-nhb
]

to setup
  clear-all
  set ndf-at-tick []
  set dnorm-lookup []
    
  set grain 0.05
  get-dnorm-lookup
  set update-approx 10

  create-trees n-trees [
    setxy random-xcor random-ycor
    set shape "circle"
    set size 0.8
    set color green
  ]
  
  ask trees [
    set nhbs get-nhbs
    set n-nhbs count nhbs
    set comp-nhb get-competition
  ]
  
  ask trees [
    let m-comp max [comp-nhb] of trees
    set comp-nhb comp-nhb / m-comp
  ]

  ; r:setPlotDevice
  reset-ticks
end

;; Control
to go 
  if not any? trees [stop]
  one-generation
  set ndf-at-tick lput ndf-local ndf-at-tick
  tick
end

;; one generation consists of n-tree repetitions of the birth-death pair selection
to one-generation
  let bd-events 0
  let critical-ci 0  
    
  ;; Update nhbs at start of each generation
  ask trees [
    set nhbs get-nhbs
    set n-nhbs count nhbs
    if density-dependent? [set comp-nhb get-competition]
  ]

  ;; threshold for dd mortality if required - only do this *once* per generation!
  if density-dependent? and density-threshold > 0.0 [
    set critical-ci list-percentile (sort [comp-nhb] of trees) density-threshold
  ]
  
  repeat n-trees [
    set bd-events bd-events + 1
    ;; death? select tree at random or on basis of local density depending on whether density-dependence used
    ifelse density-dependent? and density-threshold > 0.0 [
      ;; at low abundance all the trees above the competition threshold may die so need to catch this
      ;; to stop empty agentset errors
      if critical-ci > max [comp-nhb] of trees [
        set critical-ci max [comp-nhb] of trees
      ]  
      ask one-of trees with [comp-nhb >= critical-ci] [die] 
    ]
    [
      ask one-of trees [die]
    ]
    ;; recruitment at a random exponential distance (rate = mean-dispersal-distance) and bearing from parent
    ask one-of trees [
      reproduce
    ]
    ;; updates the tree nhb and critical CI value periodically at a rate controlled by update-approx (the smaller the more often, but the slower)
    if (bd-events mod n-trees / update-approx = 0) and (density-dependent? and density-threshold > 0.0) [
      ask trees [
        set nhbs get-nhbs
        set n-nhbs count nhbs
        set comp-nhb get-competition
      ]
      set critical-ci list-percentile (sort [comp-nhb] of trees) density-threshold
    ]
  ] 
 
  ;; If population is changing in size add or remove the necessary individuals each generation
  if popn-change? and ticks > start-change [
    ;; population is growing
    ifelse lambda > 1 [
      let delta ceiling ((lambda - 1) * (count trees))
      repeat delta [ ask one-of trees [reproduce ] ]  
    ]
    [ ;; population is shrinking
      ask n-of ceiling ((1 - lambda) * count trees) trees [die] 
    ]      
  ]
end


;; 'hatch' a new tree at location xy with parent's properties
to reproduce
  hatch 1 [
    set heading random 360
    fd random-exponential mean-dispersal-distance
    set color green
    
    set nhbs get-nhbs
    set n-nhbs count nhbs
    set comp-nhb get-competition
  ]
end


;; Report the local NDF (as per Condit et al. 2000)
to-report ndf-local
  let a world-width * world-height
  let n 0
  
  ;; get the total number of trees in neighbourhoods (!= n-trees)
  ask trees [ set n n + n-nhbs ]
 
  let abund count trees
  let annuli pi * critical-d * critical-d
  
  ;; ndf is the relative density of trees in annuli around trees density normalised
  let ndf (n / (abund * annuli)) / (abund / A)
  report ndf
end  

;; return other trees in the neighbourhood
to-report get-nhbs
  report other trees in-radius critical-d 
end 

;; get competition index
to-report get-competition
  let ci 0
  ask nhbs [
    let d distance myself
    set ci ci + (competition-at-distance d)
  ] 
  report ci
end

;; Competition intensity is the sum of the densities of the normal dsitrbution with sd = density-strength
;; Each nhb contributes dnorm(dij, 0, sd) to the index - so trees with lots of close nhbs have higher value especially
;; for sd close to zero
to-report competition-at-distance [dist]
  let idx ceiling (dist / grain) - 1
  report item (dist / grain) dnorm-lookup
end


;; Builds a lookup list of the normal density for x from 0 to critical-d by step grain
;; Used to compute the CI index much more quickly...
to get-dnorm-lookup
  ;; r:put "sd" density-scalar
  let d 0
  
  while [d <= critical-d] [
   ;; (r:put "d" d)
    set dnorm-lookup lput (d-gaussian d 0 density-scalar ) dnorm-lookup
    ;;set dnorm-lookup lput (r:get "dnorm(x = d, mean =  0, sd = sd)") dnorm-lookup
    set d d + grain
  ]
end  

;; This returns the limit-th percentile -> used for density-dependence
;; The version which Uses th R quantile function is quicker
to-report list-percentile [ valList limit]
  let v-sort sort valList
  let d density-threshold * length valList
  
  let q item d valList
  report q
end
 

; export the point map to R 
;to map-to-r
;if (count trees = 0) [stop]
;  (r:putagent "agentlist" trees "who" "xcor" "ycor")
;  r:eval "plot(agentlist$xcor,agentlist$ycor, pch = 3, cex = 0.8, las = 1, asp = 1, xlab='x', ylab='y')"
;end
;
;;; export the NDF time series to R
;to ndf-trace-to-r
;  if (count trees = 0) [stop]
;  r:put "ndf.trace" ndf-at-tick
;  r:eval "m <- max(ndf.trace)"
;  r:eval "plot(ndf.trace, type = 'l', xlab = 'Time', ylab = expression(NDF~(Omega)), las = 1, ylim = c(0,m))"
;  r:eval "abline(h = 1, lty = 2)"
;end   

to write-xy
 file-open "xy-locations.txt"
 file-print "xcor ycor"
 ask trees [
   file-write xcor file-write " " file-print ycor
 ]  
 file-close
end
 
 

   
     
@#$#@#$#@
GRAPHICS-WINDOW
276
10
686
441
-1
-1
4.0
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
99
0
99
1
1
1
ticks
30.0

SLIDER
79
56
251
89
n-trees
n-trees
50
1000
400
10
1
NIL
HORIZONTAL

BUTTON
9
10
76
43
NIL
setup\n
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
78
10
206
43
NIL
one-generation
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
208
10
271
43
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
80
134
268
167
mean-dispersal-distance
mean-dispersal-distance
1
20
5
.5
1
NIL
HORIZONTAL

PLOT
694
10
1048
278
NDF (Aggregation)
Time
NDF
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks ndf-local"
"pen-1" 1.0 0 -7500403 true "" "plotxy ticks 1"

SLIDER
76
263
251
296
lambda
lambda
0.95
1.05
0.985
0.005
1
NIL
HORIZONTAL

MONITOR
695
284
752
329
n
count turtles
0
1
11

SLIDER
79
92
251
125
critical-d
critical-d
5
20
10
1
1
NIL
HORIZONTAL

SWITCH
90
321
266
354
density-dependent?
density-dependent?
0
1
-1000

SLIDER
90
359
267
392
density-scalar
density-scalar
0
10.0
2.5
.5
1
NIL
HORIZONTAL

MONITOR
761
284
862
329
NDF at critical-d
ndf-local
3
1
11

SLIDER
76
226
248
259
start-change
start-change
1
200
200
1
1
NIL
HORIZONTAL

SLIDER
92
398
264
431
density-threshold
density-threshold
0
1.0
0.05
.01
1
NIL
HORIZONTAL

SWITCH
95
190
229
223
popn-change?
popn-change?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model is an implementation of the model described in

+   Flügge AJ, Olhede SC and Murrell D 2012 The memory of spatial patterns – changes in local abundance and aggregation in a tropical forest. _Ecology_ *93*, 1540-1549

as referenced in Chapter 2 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   Flügge AJ, Olhede SC and Murrell D 2012 The memory of spatial patterns – changes in local abundance and aggregation in a tropical forest. _Ecology_ *93*, 1540-1549
+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-13 David O'Sullivan and George L. W. Perry

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact David O'Sullivan at d.osullivan@auckland.ac.nz, or George Perry at george.perry@auckland.ac.nz
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
NetLogo 5.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="abund-aggreg-mc" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>ndf-local</metric>
    <metric>count trees</metric>
    <enumeratedValueSet variable="density-dependent?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-threshold">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-dispersal">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="critical-d">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dd-experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>ndf-local</metric>
    <enumeratedValueSet variable="density-dependent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-threshold">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-dispersal">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="critical-d">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="delta-expt" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="400"/>
    <metric>count turtles</metric>
    <metric>ndf-local</metric>
    <enumeratedValueSet variable="density-dependent?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="1"/>
      <value value="1.01"/>
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-dispersal">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="critical-d">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-change">
      <value value="200"/>
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
