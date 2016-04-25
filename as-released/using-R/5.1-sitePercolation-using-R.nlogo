; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

extensions[r profiler]

globals [
  cluster-size        ;; list of size of each occupied cluster, length is cluster-count - 1
  log-cluster-size    ;; log cluster size
  cluster-count       ;; total number of occupied clusters
  spanning-present?   ;; flag to denote presence of spanning cluster
  mean-size           ;; mean size of occupied, non-spanning cluster
]
 
patches-own [
  cluster-id ;; cluster ID assigned during tagging
  occupied?  ;; the percolation process outcome
  spanning?  ;; patch part of a spanning cluster?
  largest?  ;; patch part of largest cluster?
  
  r-num-code   ;; code for export to R (0 = not occupied, 1 = occupied + !spanning, 1 = occupied + spanning)
]


;; initialise the lattice based on percolation threshold
to setup
  clear-all
  r:setPlotDevice
  
  ask patches [  
    set occupied? false
    set spanning? false
    set largest? false
    set cluster-id -1
    set r-num-code 0
    
    set pcolor black
    if random-float 1 <= p [
      set occupied? true
      set pcolor white
      set r-num-code 1
    ]    
  ]
  ;; initialise the globals
  set cluster-size []
  reset-ticks
end

to tag 
  set spanning-present? false
  
  identify-clusters
  tag-largest-cluster
  set mean-size typical-cluster-size
  
  set log-cluster-size map [log ? 10] cluster-size ;-occupied
  histogram log-cluster-size
end


to colour-largest
  ask patches with [largest?] [set pcolor red]
end


;; Colour spanning cluster green
to colour-spanning
  ifelse spanning-present? [
    ask patches with [spanning?] [set pcolor green]
  ]
  [
    user-message("No spanning clusters identified on the lattice.")  
  ]
end  

to tag-largest-cluster
  ifelse cluster-count = 1 [
    if p = 1 [ask patches [set largest? true]]
  ]
  [
    let biggestClusterId (position (max cluster-size) cluster-size )
    ask patches with [cluster-id = (biggestClusterId)] [set largest? true]
  ] 
end


to identify-clusters
  set cluster-count 0
  let occupied patches with [occupied?]
  let all-to-tag sort occupied with [cluster-id = -1]
  while [ length all-to-tag > 0 ] [
    set cluster-count cluster-count + 1
    ;; keep track of the current cluster for efficient testing if it is spanning at end
    let current-cluster patch-set nobody
    ;; lists are mutable so we use those here, starting with
    ;; a randomly selected untagged patch
    let patches-to-tag (patch-set one-of all-to-tag)
    
    let col random 140
    while [ any? patches-to-tag ] [
      set current-cluster (patch-set current-cluster patches-to-tag)
      ask patches-to-tag [ 
        ;; tag and assign cluster ID 
        set cluster-id cluster-count - 1 ;; 0 will be the first cluster ID
        set pcolor col  ;; this allows user to see progress
      ]
      set patches-to-tag (patch-set [neighbors4] of patches-to-tag) with [occupied? and cluster-id = -1]
    ]
    let n count current-cluster
    set cluster-size lput n cluster-size
    ;; check here whether it is a spanning cluster
    if n >= min(list world-width world-height) [
      if width current-cluster = world-width or height current-cluster = world-height [
        ask current-cluster [set spanning? true]
        set spanning-present? true
      ]  
    ] 
    set all-to-tag filter [[cluster-id] of ? = -1] all-to-tag
    tick 
  ]
  ;; restore the colours
  ask patches with [occupied?] [ set pcolor white ]
end

;; reporters for width and height of a patch-set
to-report width [p-set]
  report max [pxcor] of p-set - min [pxcor] of p-set + 1
end

to-report height [p-set]
  report max [pycor] of p-set - min [pycor] of p-set + 1
end


to-report prop-spanning
  let pf 0
  if spanning-present? [
    set pf (count patches with [spanning? = true]) / (count patches)
  ]
  report pf   
end  


to-report typical-cluster-size
  ;; mean cluster size is calcualted without the spanning cluster and without bkgd and is size weighted
  ;; basically the typical size cluster that a rnd selected site will belong too
  ;; so make a list of the cluster-size of each occupied, non-spanning cluster patch one entry per patch
  let weighted-list map [item ([cluster-id] of ?) cluster-size] sort (patches with [occupied? and not spanning?])
  ;; mean of this list is the required weighted cluster size mean
  report mean weighted-list
end  


;; send data to R to make log-log size rank plot
to r-hist-cs
  let s reverse sort cluster-size
  r:put "s" s

  let cs map [count-gte s ?] remove-duplicates s
  r:put "cs" cs
  
  r:eval("s <- unique(s)")
  r:eval("plot(x = s, y = cs, log = 'xy', las = 1, bty = 'n', xlab = 'Clusters of size s', ylab = 'No. clusters size > s')")
end

;; reports the number of items in list lst that are >= x
to-report count-gte [lst x]
  report length filter [? >= x] lst
end

;; plot lattice in R
to lattice-to-R
  r:put "nr" world-height
  r:put "nc" world-width
  
  r:put "z" map [[occupied?] of ?] sort patches
  r:eval("z <-matrix(z, nrow=nr, ncol=nc)")
  r:eval("image(z, col = c('black', 'white'), asp = 1)")
end

;; send the largest cluster to R
to largest-to-r
  code-for-r

  r:put "nr" world-height
  r:put "nc" world-width

  r:put "z" map [[r-num-code] of ?] sort patches
  r:eval("z <-matrix(z, nrow=nr, ncol=nc)")
  r:eval("image(z, col = c('black', 'white', 'red'), asp = 1)")
end

;; set the r-num-code
to code-for-r
  ask patches [
    if occupied? = false [ set r-num-code 0]
    if occupied? = true and largest? = false [set r-num-code 1]
    if occupied? = true and largest? = true [set r-num-code 2]
  ] 
end 
  
  
to profile
  setup                  ;; set up the model
  profiler:start         ;; start profiling
  tag                    ;; run something you want to measure
  profiler:stop          ;; stop profiling
  print profiler:report  ;; view the results
  profiler:reset         ;; clear the data
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
670
491
-1
-1
2.0
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
224
0
224
0
0
1
clusters-tagged
100.0

SLIDER
24
53
196
86
p
p
0
1
0.59274621
0.001
1
NIL
HORIZONTAL

BUTTON
66
333
198
369
NIL
colour-largest
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
211
446
434
466
Occupied patches in white
12
0.0
1

BUTTON
64
374
198
407
NIL
colour-spanning
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
692
11
960
228
Cluster size distribution
log10 (Cluster Size)
Frequency
0.0
20.0
0.0
10.0
true
false
"set-histogram-num-bars 100" "set-plot-x-range 0 5"
PENS
"default" 1.0 2 -16777216 true "" ""

MONITOR
99
273
197
318
clusters tagged
cluster-count
0
1
11

MONITOR
794
236
896
281
Mean Cluster Size
mean-size
1
1
11

BUTTON
829
302
923
335
R-size-dist
r-hist-cs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
902
236
959
281
pFrac
prop-spanning
3
1
11

BUTTON
91
116
194
149
setup
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
720
303
820
336
lattice-to-R
lattice-to-R
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
702
342
819
375
largest-to-r
largest-to-r
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
101
16
197
49
set p to pc
set p 0.59274621
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
35
190
200
245
Note tagging can take some time!  Clusters are coloured as they get tagged to show progress.
11
0.0
1

BUTTON
131
154
194
187
NIL
tag
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
693
231
787
291
This plot is not very useful - the R-size-dist option is better
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model is an implementation of Conway's game of life.  See:

+    Gardner M 1970 Mathematical games: the fantastic combinations of John Conway�s new solitaire game �life�. _Scientific American_ **223**, 120�123.

This is an example model referenced in Chapter 1 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.


## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
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
  <experiment name="cluster-size-vs-p" repetitions="10" runMetricsEveryStep="false">
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>mean-size</metric>
    <metric>prop-spanning</metric>
    <steppedValueSet variable="p" first="0.5" step="0.0010" last="0.7"/>
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
