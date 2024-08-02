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

;; Simple program to demonstrate effects of rescaling lattices


__includes["2.1-scale-mrc.nls"]


globals [
  num-classes   ;; the number of suitability classes
  class-FD      ;; freq distribution of classes requested
  class-CFD     ;; cumulative freq dist of classes requested
]


patches-own [
  initial-state ;; initial state for reset function
  state         ;; state of the patch
  centre?       ;; true if patch is central patch in an aggregated block
  tagged?       ;; flag to indicate patch has been tagged in the clustering process
  block-set     ;; ID of aggregated block of this patch
]


;; Setup the lattice to extent lattice-L x lattice-L
to setup
  ;; resize to requested size
  resize-world 0 (lattice-L - 1) 0 (lattice-L - 1)
  ;; set patches to an integer pixel size so that
  ;; total extent is close to 500 x 500 pixels
  set-patch-size floor (500 / lattice-L)
  ;; initialize indicator patch variables
  ask patches [
    set centre? false
    set tagged? false
    set block-set patch-set nobody
  ]
  ifelse method = "mrc" [
    ;; code for this in the .nls file
    setup-using-mrc-method
  ]
  [
    ;; random initialization
    ask patches [
      set state random n-types
    ]
    ;; voter method initialization is described in detail
    ;; in model 3.8
    if method = "voter" [
      repeat voter-iterations [
        ask patches [
          set state [state] of one-of neighbors4
        ]
      ]
    ]
  ]
  ;; record the initial state
  ask patches [ set initial-state state ]
  color-patches
end


;; reset lattice to original state
to reset-to-initial-state
  ask patches [
    set centre? false
    set block-set nobody
    set state initial-state
  ]
  color-patches
end


;; we store state as a number
;; this method colors patches
;; accordingly
to color-patches
  ask patches [
    set pcolor item state base-colors + 1
  ]
end

;; rescale the grid using a modal filter
;; in neighbourhood of specified size
to change-grain
  ;; reset to the original
  reset-to-initial-state

  ;; number of blocks in each direction is maximum we can fit
  let n-blocks ceiling (world-width / new-grain)
  ;; edge offset is half the space required by a set of blocks one narrower
  let offset floor ((world-width - (n-blocks - 1) * new-grain) / 2)
  let center-coords n-values n-blocks [ i -> i * new-grain + offset ]

  ;; build the blocks around each centre and apply modal filter
  foreach center-coords [ cx ->
    foreach center-coords [ cy ->
      ask patch cx cy [
        build-blocks new-grain
        set centre? true
        let modal-state one-of modes [state] of block-set
        ask block-set [
          set state modal-state
        ]
      ]
    ]
  ]
  color-patches
  ;; colour nhb centres if desired
  if show-block-centre? [
    ask patches with [centre?] [ set pcolor white ]
  ]
end


;; build block of width  around patches
;; this is a patch procedure
to build-blocks [width]
  set block-set patch-set nobody
  ;; make a list from -half the width to +half width
  let offsets n-values width [ x -> x - floor (width / 2) ]
  ;; loop over offsets in both x and y directions
  ;; adding patch at that offset to the block set
  foreach offsets [ offset-x ->
    foreach offsets [ offset-y ->
      set block-set (patch-set block-set patch-at offset-x offset-y)
    ]
  ]
end


to trim-extent
  reset-to-initial-state
  if new-extent > lattice-L [set new-extent lattice-L]
  let trim (lattice-L - new-extent) / 2
  ask patches with [pxcor < trim or pxcor > lattice-L - trim or pycor < trim or pycor > lattice-L - trim] [
    set pcolor black
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
197
10
705
519
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
124
0
124
1
1
1
ticks
30.0

BUTTON
17
114
114
147
change-grain
change-grain
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
102
60
187
93
reset
reset-to-initial-state
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
16
254
188
287
new-grain
new-grain
3
15
9.0
2
1
NIL
HORIZONTAL

BUTTON
102
12
188
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

SLIDER
16
200
188
233
n-types
n-types
2
10
6.0
1
1
NIL
HORIZONTAL

SLIDER
16
161
188
194
lattice-L
lattice-L
24
249
125.0
1
1
NIL
HORIZONTAL

SWITCH
16
341
185
374
show-block-centre?
show-block-centre?
0
1
-1000

SLIDER
15
291
187
324
new-extent
new-extent
29
249
84.0
1
1
NIL
HORIZONTAL

BUTTON
120
114
186
147
trim
trim-extent
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
710
13
848
58
method
method
"random" "mrc" "voter"
0

INPUTBOX
710
61
848
121
class-file
2.1-classes.txt
1
0
String

SLIDER
710
176
849
209
p
p
0
.6
0.41
.01
1
NIL
HORIZONTAL

SWITCH
710
219
900
252
remove-singleton-patches?
remove-singleton-patches?
0
1
-1000

TEXTBOX
712
256
896
286
MRC can be slow - be patient!
12
15.0
1

TEXTBOX
714
126
891
170
For MRC, the class-file needs to have as many lines in it as there are n-types.
12
15.0
1

SLIDER
710
291
882
324
voter-iterations
voter-iterations
0
100
30.0
1
1
NIL
HORIZONTAL

TEXTBOX
709
333
859
361
Only used by the voter initialization method
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model is a simple demonstration of some of the effects on spatial pattern of aggregation and study area definition as discussed in Chapter 2 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## THINGS TO TRY

This model's overall size is set to a square with the number of patches specified by the `lattice-L` variable. Patch pixel size will adjust so that the overall world width is close to 500 pixels.

The initial model state is set by one of three different methods selected from the `method` drop-down list:

+ `random` sets each patch to one of the specified number `n-types` of states completely at random

+ `mrc` uses the _modified random clustering_ process of Saura and Martínez-Milán (2000). This method is implemented in the [`2.1-scale-mrc.nls`](https://github.com/DOSull/model-zoo/blob/master/base-models/2.1-scale-mrc.nls) code file.

+ `voter` uses a simple voter model, as presented in more detail in [model 3.8](https://github.com/DOSull/model-zoo/blob/master/base-models/3.8-voter-model.nlogo).

The **change-grain** button causes an array of square 'blocks' of the size specified to be created each with its state set to the _modal_ state of the patches included in the block, i.e., the state that is most numerous among the included patches.  If two or more states tie then one will be chosen at random.

The **trim** button causes the study area to be limited to the specified extent.

These two operations correspond with those discussed in Chapter 2 of the book.

The **reset** button restores the world to the state when it was first initialized.

## THINGS TO NOTICE

The code here uses the `n-values` operation to create lists of coordinates for the aggregated block centres, in the `change-grain` procedure. The line

    let center-coords n-values n-blocks [i -> i * new-grain + offset]

is an example. This is a useful NetLogo function worth getting to know. Here is makes a list with `n-blocks` elements, each of them a multiple (by a factor of `new-grain`) of its index postion in the list, with an `offset` added. For example, with `offset` 5 and `new-grain` set to 9, this would produce a list `[5 14 23 32 ...]` and so on, up to the requested number of elements. We then use nested `foreach` loops to step through this list and set patch centres for the aggregated blocks.

This technique is more elegant than having nested loops such as the following

    let x-coord offset
    repeat n-blocks [
      set x-coord x-coord + new-grain
      let y-coord offset
      repeat n-blocks [
        set y-coord y-coord + new-grain
        ## do stuff with x-coord y-coord
      ]
    ]

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

See also

+ Saura S and Martínez-Milán J 2000 [Landscape pattern simulation with a modified random clusters method](http://dx.doi.org/10.1023/A:1008107902848). _Landscape Ecology_ **15** 661–677.

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
