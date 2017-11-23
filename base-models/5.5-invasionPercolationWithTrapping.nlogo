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

extensions [ palette ]

globals [
  last-invaded           ;; the most recently invaded patch
  next-to-invade         ;; list of the patches subject to invasion next
  max-x                  ;; max pxcor of invasion at each pycor
  max-y                  ;; max pycor of invasion at each pxcor
  min-y                  ;; min pycor of invasion at each pxcor

  min-field
  max-field
  color-ramp-ends
]

patches-own [
  invaded?               ;; patch has been invaded
  time-invaded           ;; the time step when invasion occurred
  p                      ;; U[0~1]
  possible-successors    ;; the 3 NSE neighbours
  possible-predecessors  ;; the 3 NSW neighbours
  NX                     ;; the four diagonal neighbours
]


to setup
  clear-all

  random-field
  let pal palette:scheme-colors "Sequential" "Blues" 3
  set color-ramp-ends (list first pal last pal)

  set min-field min [p] of patches
  set max-field max [p] of patches

  ask patches [
    set invaded? false
    set time-invaded -1
    set possible-successors (patch-set (patch-at 1 0) (patch-at 0 1) (patch-at 0 -1))
    set possible-predecessors (patch-set (patch-at -1 0) (patch-at 0 1) (patch-at 0 -1))
    set NX (patch-set (patch-at 1 1) (patch-at -1 1) (patch-at -1 -1) (patch-at 1 -1))
  ]
  repeat smoothing [ diffuse p 0.05 ]
  ;; tag and mark left, top and bottom edges as 'invaded'
  ask patches with [pxcor = min-pxcor or pycor = min-pycor or pycor = max-pycor] [
    set invaded? true
    set pcolor grey
  ]
  reset-ticks

  ;; max-x maintains for each pycor the max invaded pxcor in that row
  set max-x (sentence max-pxcor n-values (max-pycor - 1) [min-pxcor] max-pxcor)

  ;; max-y and min-y maintain for each pxcor the region inside the invading cluster
  set max-y n-values world-width [min-pycor]
  set min-y n-values world-width [max-pycor]

  set next-to-invade sort patches with [pxcor = 0 and pycor > min-pycor and pycor < max-pycor] with-min [p]

  set last-invaded first next-to-invade
  set next-to-invade but-first next-to-invade
  ask last-invaded [set pcolor violet]
  update-patch-states
end


to go
  ask last-invaded [set pcolor white]

  ;; check for completion
  if [pxcor] of last-invaded = max-pxcor [stop]

  ;; invade the next patch
  set last-invaded first next-to-invade
  ;; remove the last-invaded
  set next-to-invade but-first next-to-invade
  update-patch-states

  ; check for traps?
  if trapping? [
    tag-traps patch-set [NX with [invaded?]] of last-invaded
  ]
  tick
end


;; do book keeping on patch state variables
to update-patch-states
  ask last-invaded [
    set pcolor violet
    set invaded? true
    set time-invaded ticks
    update-extent-lists pxcor pycor
  ]
  ifelse stochastic? [
    ;; add any uninvaded possible successors of the last-invaded
    set next-to-invade (patch-set next-to-invade ([possible-successors] of last-invaded) with [not member? self next-to-invade and not invaded?])
    ;; stochastic update
    ask next-to-invade [ set p p + random-float-bnd (-1 * stochastic-shift) stochastic-shift ]
    ;; sort by the random-variable
    set next-to-invade sort-by [ [patch1 patch2] -> [p] of patch1 < [p] of patch2 ] next-to-invade
  ]
  [
    ; insert the new possible sites while maintaining sort order
    ask last-invaded [
      ask possible-successors with [not member? self next-to-invade and not invaded?] [
        set next-to-invade insert-in-order next-to-invade self
      ]
    ]
  ]
end


;; reports a list with patch x inserted in list of patches lst
;; while maintaining it in sorted order by p value
;; ASSUMES that lst is already sorted by p values
to-report insert-in-order [lst x]
  let posn length filter [ ptch -> [p] of ptch < [p] of x ] lst
  report (sentence (sublist lst 0 posn) x (sublist lst posn (length lst)))
end


;; NOTE dependency on x,y coord 0 0 at origin...
to update-extent-lists [x y]
  ;; update the max invasion lists
  if x > item y max-x [
    set max-x replace-item y max-x x
  ]
  if y = (max-pycor - 1) [
    set max-y sentence (n-values (x + 1) [max-pycor]) (sublist max-y (x + 1) world-width)
  ]
  if y > item x max-y [
    set max-y replace-item x max-y y
  ]
  if y = (min-pycor + 1) [
    set min-y sentence (n-values (x + 1) [min-pycor]) (sublist min-y (x + 1) world-width)
  ]
  if y < item x min-y [
    set min-y replace-item x min-y y
  ]
end


;; tagging the traps associated with possible closure sites
to tag-traps [poss-closes]
  if not any? poss-closes [stop]
  let trap-seeds trap-seed-patches poss-closes
  ask trap-seeds [
    let escaped? false  ;; trap is escaped to beyond invasion
    let clean-up nobody ;; all patches considered by the search, which have to be cleaned up
    let this-trap patch-set self
    while [any? this-trap and not escaped?] [
      set clean-up (patch-set clean-up this-trap)
      let next-this-trap nobody ;; a list of the next set of patches in the same trap
      ask this-trap [
        set invaded? true
      ]
      ask this-trap [
        let N4 neighbors4 with [not invaded?]
        ask N4 [
          set escaped? escaped? or (pxcor > item pycor max-x)
                                or (pycor > item pxcor max-y)
                                or (pycor < item pxcor min-y)
        ]
        set next-this-trap (patch-set N4 next-this-trap)
      ]
      set this-trap next-this-trap
    ]
    ifelse escaped? [ ;; then undo the tagging
      ask clean-up [ set invaded? false ]
    ]
    [ ;; else colour grey and remove from the next-to-invade list
      ask clean-up [ set pcolor cyan - 2 ]
      set next-to-invade filter [ ptch -> [not invaded?] of ptch ] next-to-invade
    ]
  ]
end


;; reports seed locations for possible traps arising from
;; the current last-invaded and the supplied list of neighbouring
;; invaded sites
to-report trap-seed-patches [sites]
  let trap-seeds nobody ;; initialise the seed sites to empty
  ask last-invaded [
    ;; all possibles are the uninvaded N4 of the last-invaded site
    let immed-trap-seeds neighbors4 with [not invaded?]
    ;; then for each closure site...
    ask sites [
      ;; intersect N4 with the uninvaded N4
      let new-trap-seeds neighbors4 with [member? self immed-trap-seeds]
      ;; and append to the set of seed sites
      set trap-seeds (patch-set trap-seeds new-trap-seeds)
    ]
  ]
  report trap-seeds
end


;; Colour each patch by its p (underlying random field) value
to colour-field
  ask patches with [not invaded?] [
    set pcolor palette:scale-gradient color-ramp-ends p 0 1
  ]
end


to colour-by-time
  ask patches with [time-invaded > -1] [
    set pcolor palette:scale-scheme "Sequential" "YlOrRd" 9 time-invaded ticks -1
  ]
end


to random-field
  ask patches [set p random-float 1]
end


to-report random-float-bnd [a b]
  report random-float (b - a) + a
end
@#$#@#$#@
GRAPHICS-WINDOW
79
10
727
339
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
639
0
319
0
0
1
ticks
100.0

BUTTON
8
10
71
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

BUTTON
8
47
71
80
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
8
84
71
117
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
9
121
73
154
go-10
repeat 10 [go]
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
24
211
57
361
smoothing
smoothing
0
100
0.0
1
1
NIL
VERTICAL

BUTTON
8
158
72
191
go-100
repeat 100 [go]
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
754
317
844
362
invasion-front
length next-to-invade
0
1
11

SWITCH
756
235
870
268
trapping?
trapping?
0
1
-1000

TEXTBOX
88
347
727
377
Set view updates to continuous or if it is set to 'on ticks' then make sure the speed slider is set towards the faster end of the range.  Otherwise, this model will run very slowly.
12
0.0
1

BUTTON
755
191
876
224
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

SWITCH
758
56
875
89
stochastic?
stochastic?
1
1
-1000

SLIDER
758
17
891
50
stochastic-shift
stochastic-shift
0
0.05
0.005
0.0025
1
NIL
HORIZONTAL

BUTTON
757
155
875
188
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

TEXTBOX
761
90
911
145
Note: this GREATLY slows the process, esp. if there is no trapping.  Go and get a coffee.
11
15.0
1

TEXTBOX
758
276
908
306
Will run a lot slower with trapping OFF.
11
15.0
1

@#$#@#$#@
## WHAT IS IT?

This model simulates _invasion percolation_ as discussed in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## INVASION PERCOLATION

Invasion percolation (IP) is a much less well known form of percolation than site percolation as portrayed in previou Chapter 5 models.

The process is as follows:

+ a grid is initialized with random values
+ the location with the lowest value on the left hand edge of the space is designated the *invasion front*
+ non-invaded locations adjacent to the front to the right or above and below cells in the front are invasible, and the site with the lowest value is invaded, and added to the front 

The last step is repeated until the invasion front reaches the right hand edge of the space.

One wrinkle is *trapping* when a set of grid cells are cut off from a path to the right hand edge. If trapped locations are not identified then a model run can take a very long time while trapped locations are invaded, with no prospect of the invasion front reaching the right hand edge from such starting points. This is controlled in the model with the **trapping?** switch.  It is recommended this switch generally be set **on**.

It may help to understand the process if you change the world dimensions to 160 wide by 80 high and the patch size to 4, then run the model with the update set to 'On ticks'.  If necessary slow it down.  You might also stop it occasionally and enter

    (foreach next-to-invade range length next-to-invade [[x i] -> ask x [set plabel i]])

This will label all the patches in the 'queue' to be invaded next (from lowest value to highest). You will notice that the lower ones tend to be close to the current tip of the invasion (in red). This is because other locations in the queue have been 'paths not taken' previously and hence are likely to be in the higher range of the random field values.  This dynamic is what gives the process its characteristic multiple branching structure.

## THINGS TO NOTICE

The complicated part of this model is associated with detecting traps. The basic algorithm above is operated by maintaing a queue `next-to-invade`.  This is a list of patches next to already invaded patches (strictly north, south or east of invaded patches), that is kept ordered by the randomly generated `p` values assigned during model setup. Maintenance of the queue order is managed by the `insert-in-order` procedure.  By this mains the basic operation is

    while not at right edge [
      invade first patch in next-to-invade-list
      add newly identified N/S/E neighbours of invasion to next-to-invade
    ]

The complication is trap detection.  Honestly, I have a hard time figuring it all out.  **YOU SHOULD ALWAYS DOCUMENT CODE WHEN YOU ARE WRITING IT!** But here is my thinking...

**COMING SOON...**

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
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
