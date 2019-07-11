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

extensions [matrix]

globals [
  neighbor-offsets ;; a list of the x-y coord offsets of a Moore neighbourhood
  current-scale    ;; the scale-factor from lattice-coords to world-coords
  centre-xy        ;; two member list of the centre lattice x y coords of the CA configuration
]

;; breeds containing the current and potential next generation of cells
breed [cells cell]
breed [new-cells new-cell]

turtles-own [
  lattice-site  ;; the actual lattice coords of the cell stored as a 2-member list
]

to setup
  clear-all
  ask patches [set pcolor white]
  set-default-shape turtles "square"
  set current-scale 1
  set centre-xy [0 0]
  ;; lattice coord offsets stored as a list of two-member lists
  set neighbor-offsets (list [-1 1]  [0 1]  [1 1]
                             [-1 0]         [1 0]
                             [-1 -1] [0 -1] [1 -1])
  ask patches [
    if random-float 1 < 0.35 [
      sprout-cells 1 [
        set color black
        set size current-scale
        set lattice-site (list pxcor pycor)
      ]
    ]
  ]
  reset-ticks
end

to zoom-out
  zoom -1
end

to zoom-in
  zoom 1
end

to zoom [factor]
  let zoom-factor 2 ^ (factor / 2)
  set centre-xy mean-centre [lattice-site] of cells
  set current-scale zoom-factor * current-scale
  ask turtles [
    set-world-xy
    set size max (list current-scale 0.24)
  ]
end

to pan
  if mouse-inside? and mouse-down? [
    let x mouse-xcor
    let y mouse-ycor
    while [ mouse-inside? and mouse-down? ] [
      let new-x mouse-xcor
      let new-y mouse-ycor
      ask turtles [ ;; cells and new-cells
        setxy xcor + new-x - x ycor + new-y - y
      ]
      set centre-xy (list (item 0 centre-xy + (new-x - x) / current-scale)
                          (item 1 centre-xy + (new-y - y) / current-scale))
      display
      set x new-x
      set y new-y
    ]
    wait 0.2
  ]
end

;; TURTLE-REPORTER
;; convenience reporter which returns the x coord of the lattice site
to-report lattice-x
  report item 0 lattice-site
end

;; TURTLE-REPORTER
;; convenience reporter which returns the y coord of the lattice site
to-report lattice-y
  report item 1 lattice-site
end

;; convenience reporter for mean-xy of a list of coords
to-report mean-centre [list-of-coords]
  report map [ x -> mean x ] transpose list-of-coords
end

;; converts a list of coord pair lists
;; to a list of lists of each coord
;; uses the matrix extension
to-report transpose [list-of-lists]
  let IJ matrix:from-row-list list-of-lists
  let JI matrix:transpose IJ
  report matrix:to-row-list JI
end

;; runs each generation
to go
  if not any? turtles [ setup ]
  rescale
  create-next-gen
  weed-new-cells
  flip-cells
  tick
end

;; check if rescale is needed
;; and do it if so
to rescale
  let needed? true
  while [needed?] [
    ;; determine current max min lattice coords potentially needed
    ;; i.e. one lattice site more than the current generation occupies
    let max-x max [lattice-x] of cells + 1
    let max-y max [lattice-y] of cells + 1
    let min-x min [lattice-x] of cells - 1
    let min-y min [lattice-y] of cells - 1
    ;; determine max and min available given current scale and centres
    let top-right lattice-coords (list max-pxcor max-pycor)
    let bottom-left lattice-coords (list min-pxcor min-pycor)
    ;; zoom out if required
    ifelse (max-x > item 0 top-right) or (max-y > item 1 top-right) or
       (min-x < item 0 bottom-left) or (min-y < item 1 bottom-left) [
       zoom-out
    ]
    [
      set needed? false
    ]
  ]
end

;; converts a lattice-coord pair to Netlogo world coords
;; at current scale
to-report world-coords [xy]
  report (map [ [pxy cxy] -> current-scale * (pxy - cxy) ] xy centre-xy)
end

;; converts a world coordinate to a lattice coord
;; at current scale
to-report lattice-coords [pxpy]
  report (map [ [pxy cxy] -> round (pxy / current-scale) + cxy ] pxpy centre-xy)
end

;; TURTLE-PROCEDURE
;; convenience method to setxy of a cell
;; based on current scale and its lattice-site coords
to set-world-xy
  let wxwy world-coords lattice-site
  setxy item 0 wxwy item 1 wxwy
end

;; hatch next gen turtles on every possible new location
to create-next-gen
  foreach get-candidate-spots [ c-spot ->
    create-new-cells 1 [
      set color gray ;; this means you can see them when update is continuous
      set lattice-site c-spot
      set-world-xy
      set size max (list current-scale 0.24)
    ]
  ]
end

;; kill off next-gen turtles that don't meet the rule criteria
;; if currently live 2 or 3 live neighbours
;; if currently dead 3 live neighbours
to weed-new-cells
  ask new-cells [
    ;; make a list of the coord-pairs of the lattice neighbours
    let nghbrs neighbouring-lattice-sites

    ;; create a turtle-set of only those cells in
    ;; current generation, that are somewhere nearby
    ;; this speeds up the query looking for immediate
    ;; neighbors on the lattice
    let nearby-cells (turtle-set cells-here cells-on neighbors)

    ;; count which nearby cells are in the neighbourhood
    ;; temporarily store lattice coords for later comparison
    let xy lattice-site
    let live-n count nearby-cells with [member? lattice-site nghbrs]
    let alive? any? nearby-cells with [xy = lattice-site]
    ifelse alive?
    [ if not (live-n = 2 or live-n = 3) [die] ]
    [ if live-n != 3 [die] ]
  ]
end

;; update state by killing the current generation of live cells
;; and making the 'next' generation current
to flip-cells
  ask cells [ die ]
  ask new-cells [
    set color black
    set breed cells
  ]
end

;; TURTLE-REPORTER
;; report a list of lattice coord pairs neighbouring the supplied pair
to-report neighbouring-lattice-sites; [coord-pair]
  ; retrieve lattice-x and y once for repeated use below
  let x lattice-x
  let y lattice-y
  report map [ xy -> list (x + item 0 xy) (y + item 1 xy) ] neighbor-offsets
end

;; report a list of lattice coordinates to test
;; for life at the next step.  These are effectively
;; a one-cell buffer around all currently live cells
to-report get-candidate-spots
  let neighbouring-sites [neighbouring-lattice-sites] of cells
  ;; make list of lists into a single list of coord pairs
  set neighbouring-sites reduce [ [n1 n2] -> sentence n1 n2 ] neighbouring-sites
  ;; join to the current spots and remove duplicates
  report remove-duplicates sentence neighbouring-sites [lattice-site] of cells
end
@#$#@#$#@
GRAPHICS-WINDOW
98
16
535
454
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
60.0

BUTTON
23
21
86
54
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
23
60
86
93
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
24
101
87
134
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
544
185
618
218
NIL
zoom-in
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
544
224
619
257
NIL
zoom-out
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
542
20
681
120
Netlogo will display cells outside the current zoomed area by 'wrapping' their coordinates.  This accounts for display glitches when zoomed in.
11
0.0
1

MONITOR
544
132
630
177
NIL
current-scale
4
1
11

TEXTBOX
544
278
677
330
Zoom buttons only work when execution is stopped.
11
0.0
1

BUTTON
542
344
605
377
NIL
pan
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
546
385
659
455
Panning while go button is pressed will probably cause model to restart.
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model is an implementation of Conway's game of life.  See:

+    Gardner M 1970 Mathematical games: the fantastic combinations of John Conway's new solitaire game 'life'. _Scientific American_ **223**, 120-123.

which allows for an 'infinitely extensible' world as discussed in Chapter 6 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

## HOW IT WORKS

The Game of Life CA is implemented using turtles ('cells') to represent live cells in the CA.  This means that although the model starts with one turtle per patch, we can easily rescale when the extent of the live pattern extends to the point where at the current scale a cell would have to move beyond the world edge.

## NETLOGO FEATURES

This model uses some tricky methods to provide the world extensibility feature, and these should repay close study.

This model demonstrates how a model space can be extended effectively by recording only the locations of interest.  In Life, only locations that are currently alive or are adjacent to locations that are currently alive can possible be alive in the next time step.  This means that most of the space in a typical Life model where the density of live cells is likely to be less than 25% is being used to store information that is not useful (i.e., lots of dead cells).

Here, by storing the live cells (as `cell` turtles) we avoid this problem, and also enable the model scale to vary as required.

This approach requires that the `cell` breed turtles store their lattice location (as `lattice-site`, a list of two items, the x and y lattice site coordinates, since this information is not the same as their Netlogo `pxcor` and `pycor`.  Convenience turtle reporters `lattice-x` and `lattice-y` hide the list from general use, so that the x or y coordinate can be easily retrieved using the corresponding reporter.

Conversion from Netlogo world coordinates to lattice coordinates is done by the world-coords reporter

    to-report world-coords [xy]
      report (map [ [pxy cxy] -> current-scale * (pxy - cxy) ] xy centre-xy)
    end

where the `map` function applies the scaling operation to each item in the supplied `xy` coord pair list after subtracting the current `centre-xy` coord pair global.  A convenience procedure `set-world-xy` is provided to allow a turtle's location to be set according to its current `lattice-site` and `current scale` of the model.

Rescaling happens when required in the `rescale` procedure which runs at the start of each model tick.  In determining the mean lattice coordinates of the current `cells` use is made of the matrix extension transpose operation.

Because cell locations are stored as the `lattice-site` list it is necessary to have special code to retrieve the Moore neighbourhood lattice coordinates.  This is done in the `neighbouring-lattice-sites` reporter which, by applying a map function between the `lattice-site` coordinate pair and the global `neighbor-offsets` list:

    to-report neighbouring-lattice-sites; [coord-pair]
      ; retrieve lattice-x and y once for repeated use below
      let x lattice-x
      let y lattice-y
      report map [ xy -> list (x + item 0 xy) (y + item 1 xy) ] neighbor-offsets
    end

The model works by first identifying all the possible new cell locations, which are the existing cell locations plus lattice locations neighbouring these.  This code is worth close inspection:

    to-report get-candidate-spots
      let neighbouring-sites [neighbouring-lattice-sites] of cells
      ;; make list of lists into a single list of coord pairs
      set neighbouring-sites reduce [ [n1 n2] -> sentence n1 n2 ] neighbouring-sites
      ;; join to the current spots and remove duplicates
      report remove-duplicates sentence neighbouring-sites [lattice-site] of cells
    end

This reporter first retrieves a list of all the lattice site locations that neighbour current live `cells`.  A `reduce` operation 'flattens' this list of lists of lists of 8 neighbouring lattice sites to a list of all the potential lattice site coordinates of interest.  The reported result joins this list to a list of live `cell` lattice sites and removes any duplicates due to overlaps between Moore neighbourhoods (there will be many of these).

In the `create-next-gen` procedure `new-cell` turtles are hatched on these lattice sites and then applying the Game of Life rules only those which meet the requirements for live cells survive the `weed-new-cells` procedure and are switched in the `flip-cells` procedure to become the next generation of `cells`.  If you change the update mode to continuous you can see the candidate new cells briefly appear in gray before most of them are killed to form the next generation of live `cells`.

## RELATED MODELS

The turtle implementation of Life is similar to that in the Netlogo models library.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-2018 David O'Sullivan and George Perry

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to  permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.See Chapter 6, O'Sullivan, D. and Perry, G. L. W. 2013. _Spatial Simulation: Exploring Pattern and Process_, Wiley, Chichester, England.
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
NetLogo 6.1.0
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
