; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

extensions [ gradient ]

globals [
  start-sites
  basins
  sources
  sinks
  last-invaded           ;; the most recently invaded patch
  next-to-invade         ;; list of the patches subject to invasion next
  
  max-field
  min-field
  
  ;; for drainage tracing via the FFM
  start-cell 
  end-cells
  c
  front
]

patches-own [
  invaded?               ;; patch has been invaded
  time-invaded           ;; the time step when invasion occurred
  p                      ;; U[0~1]
  possible-successors    ;; the 3 NSE neighbours
  ;;possible-predecessors  ;; the 3 NSW neighbours
  
  ;; for drainage tracing
  t1
  t2
  elastic?
  basin-set?
  basin-id
  n-occ-nbrs
]

to setup
  clear-all
  reset-ticks
 
  set max-field 0
  set min-field 1e6
  set basins []
  set sources []
  set sinks []

  ask patches [
    ifelse field-dist = "uniform"
      [ set p random-float 1] 
      [ set p random-normal 100 20 ]
    
    if p > max-field [set max-field p]
    if p < min-field [set min-field p]
    
    set invaded? false
    set time-invaded -1
    
    ifelse pxcor = max-pxcor 
    ;; no successors to sites at the far edge
    [ set possible-successors patch-set nobody ] 
    [ ifelse pxcor = min-pxcor
      ;; only one successor to sites at the source edge
      [ set possible-successors patches at-points [[1 0]] ]
      ;; all others have 3 successors N, S and E
      [ set possible-successors patches at-points [[1 0] [0 1] [0 -1]] ]
    ]
    set t1 -1
    set t2 -1
    set elastic? false
    set n-occ-nbrs 0
  ]
  repeat smoothing [ diffuse p 0.05 ]
  
  ;; tag and mark left, top and bottom edges as 'invaded'
  set start-sites patches with [pxcor = min-pxcor and pycor mod 2 = 0] 
  ask start-sites [ 
    set invaded? true
    set pcolor grey 
    ask neighbors4 [
      set n-occ-nbrs n-occ-nbrs + 1
    ]
  ]
  
  ;; every second patch on the left edge is a potential start site.
  let start-candidates (patch-set [possible-successors] of patches with [invaded?])
  set next-to-invade sort-by [[p] of ?1 < [p] of ?2] start-candidates

  set last-invaded first next-to-invade
  update-patch-states
end  



to go
  ;; only check for completion
  if stop-first? and [pxcor] of last-invaded = max-pxcor - 1 [stop]
  if length next-to-invade = 0 [stop]
   
  ask last-invaded [set pcolor white]
  
  ;; invade the next patch
  set last-invaded first next-to-invade
  update-patch-states
  
  tick 
end


;; do book keeping on patch state variables
to update-patch-states
  ask last-invaded [
    set time-invaded ticks 
    set pcolor red
    set invaded? true
    ask neighbors4 [
      set n-occ-nbrs n-occ-nbrs + 1
    ]
  ]
  ;; remove the last-invaded
  set next-to-invade but-first next-to-invade 
  ; insert the new possible sites while maintaining sort order
  ask last-invaded [
    ask possible-successors [
      set next-to-invade insert-in-order next-to-invade self
    ]
  ]
  ; discard any that are ineligible because they have > 1 occupied neighbour 
  set next-to-invade filter [[n-occ-nbrs] of ? <= 1] next-to-invade
end

;; reports a list with patch x inserted in lst of patches lst
;; while maintaining it in sorted order by p value
;; ASSUMES that lst is already sorted by p values
to-report insert-in-order [lst x]
  let posn length filter [[p] of ? < [p] of x] lst
  report (sentence (sublist lst 0 posn) x (sublist lst posn (length lst)))
end


;; Colour each patch by its p (underlying random field) value
to colour-field
  ask patches with [not invaded?] [
    set pcolor gradient:scale [ [229 245 249] [153 216 201] [44 162 95] ]  p min-field max-field
  ]
end

;; Colour each patch by the time it was invaded
to colour-by-time
  ask patches with [invaded? and (time-invaded != -1)] [
    set pcolor gradient:scale [[227 74 51] [253 187 132] [254 232 200] ]  time-invaded 0 ticks
  ]
end 


;; This identifies the drainage paths - basically the elastic backbone of each
;; of the clusters present on the lattice 
to trace-drainage
  identify-basins
  (foreach basins sources sinks [
    let this-basin ?1 
    set start-cell ?2
    set end-cells ?3
    pass-one
    ask end-cells [
      let end-cell self
      pass-two end-cell
      ask this-basin with [t1 != -1 and t2 != -1] [ 
        set elastic? true
      ]       
    ] 
  ])
  ask patches [set pcolor black]
  ask patches with [elastic? = true] [set pcolor blue + 2]
end

to pass-one
  initiate start-cell 1 
  spread 1
end

to pass-two [end-cell]
  initiate end-cell 2
  spread 2 
end

to initiate [start-location pass ]
  set c 0
  ask start-location [
    if pass = 1 [ set t1 c ]
    if pass = 2 [ set t2 c ]
    set front patch-set self               ;; a new fire-front patch-set
  ]
end


to spread [ pass ]
  while [ any? front ] ;; Stop when we run out of flaming fire-front
  [ 
    set c c + 1
    new-shell pass
  ]
end


to new-shell [pass] 
  ;; Empty set of patches for the next 'round' of the fire
  let new-front patch-set nobody     
  ask front [
    if pass = 1 [
      ask neighbors4 with [invaded? and t1 = -1] [
        set new-front ( patch-set new-front self)
        set t1 c
      ]
    ]
    if pass = 2 [
      let my-t1 t1
      ask neighbors4 with [t1 != -1 and t1 < my-t1] [
        set new-front ( patch-set new-front self)
        set t2 c
      ]
    ]
  ]
  set front new-front
end



to identify-basins
  let basin-count 0
  ;; tag all patches less than the cut-off as counted
  ask patches [ set basin-set? not invaded? ]
  
  let all-to-tag sort patches with [not basin-set?]    
  let to-recolour patch-set all-to-tag
  while [ length all-to-tag > 0 ] [
    let col random 140
    let this-basin patch-set nobody
    set basin-count basin-count + 1
    ;; start with a randomly selected untagged patch
    let patches-to-tag (patch-set one-of all-to-tag)
    
    while [ any? patches-to-tag ] [
      ask patches-to-tag [ 
        ;; tag as counted and mark for statistical purposes 
        set basin-set? true
        set basin-id basin-count
        set pcolor col ;; for progress visualization
      ]
      set this-basin (patch-set this-basin patches-to-tag)
      set patches-to-tag (patch-set [neighbors4] of patches-to-tag) with [not basin-set?]
    ]
    set basins lput this-basin basins
    set sources lput (this-basin with [member? self start-sites]) sources
    set sinks lput (this-basin with-max [pxcor]) sinks
    set all-to-tag filter [not [basin-set?] of ?] all-to-tag
  ]
  ask to-recolour [
    set pcolor white
  ]
end



  
@#$#@#$#@
GRAPHICS-WINDOW
185
10
430
491
-1
-1
1.0
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
449
0
0
1
ticks
100.0

BUTTON
87
25
150
58
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
87
62
150
95
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
87
99
150
132
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
68
214
179
247
smoothing
smoothing
0
100
0
1
1
NIL
HORIZONTAL

MONITOR
435
433
525
478
invasion-front
length next-to-invade
0
1
11

TEXTBOX
122
714
460
746
Make sure that view updates is set to continuous (otherwise the model will appear to run very slowly).\nNote the model is computationally expensive in any case so be patient!
12
0.0
1

BUTTON
433
328
554
361
colour-by-time
colour-by-time
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
435
291
549
324
NIL
colour-field
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
434
74
555
107
NIL
trace-drainage
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
68
391
173
424
stop-first?
stop-first?
0
1
-1000

CHOOSER
71
259
177
304
field-dist
field-dist
"uniform" "Gaussian"
1

TEXTBOX
72
436
171
471
Turn this on to stop things more quickly!
11
15.0
1

BUTTON
434
132
553
165
colour-basins
foreach basins [\nlet x one-of [3 5 7]\nask ? [set pcolor basin-id * 10 + x]\n]
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

This model is based on the model reported in 

+   Stark CP 1991 An invasion percolation model of drainage network evolution. _Nature_ **352** 423–425.

as discussed in in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Stark CP 1991 An invasion percolation model of drainage network evolution. _Nature_ **352** 423–425.
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
