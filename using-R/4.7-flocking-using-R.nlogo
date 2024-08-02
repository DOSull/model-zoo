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
;;

extensions [sr table]

breed [flockers flocker]  ;; the flocking individuals
breed [tails tail]        ;; turtles to form a tail showing recent locations

flockers-own [
  new-heading
  my-tail      ;; maintain a tail of recently visited locations
               ;; such that first turtle in my-tail at the previous location
  v-x          ;; x component of current velocity
  v-y          ;; y component
  new-v-x      ;; x component of the calculated new velocity
  new-v-y      ;; y component
  speed
  flock-mates  ;; the 'reference group' that the flocker adjusts its movement to
]

tails-own [
  speed
]

;; this simplifies the search by only considering movers on the current
;; and neighbouring patches
patches-own [
  locale         ;; store the patch and its neighbours in a patch-set for efficiency
  local-flockers ;; flockers on the locale - stored for efficiency
]

to setup
  clear-all

  sr:setup
  sr:run "library(deldir)"

  ask patches [
    set pcolor white
    set locale (patch-set self (patches in-radius near-range))
  ]
  set-default-shape turtles "default"
  set-default-shape tails "line"
  create-flockers ceiling (density * count patches) [
    setxy random-xcor random-ycor
    set color red
    set size 0.5
    create-initial-tail
  ]
  update-local-flockers
  reset-ticks
end

;; initialise the tail turtles
;; do this by moving the flocker the required number of times
;; and calling update-tail
to create-initial-tail
  set my-tail []
  repeat tail-length [
    update-tail
    update-movement-variables
    jump speed
  ]
end

;; adds the current location to the tail
;; and deletes oldest consistent with the currently set tail-length
to update-tail
    let latest nobody
    hatch-tails 1 [
      set size [speed] of myself
      set speed size
      set latest self
    ]
    set my-tail fput latest my-tail
    ;; now kill any tail turtles surplus to the current tail-length requirement
    foreach sublist my-tail (min list tail-length length my-tail) (length my-tail) [ t ->
      ask t [ die ]
    ]
    ;; finally update my-tail
    ;; this avoids it filling up with dead tail turtles
    set my-tail sublist my-tail 0 (min list tail-length length my-tail)
end

;; updates stored set of local flockers for each patch
to update-local-flockers
  ask patches [
    set local-flockers flockers-on locale
  ]
  let max-density max [count local-flockers] of patches
  ask patches [
    ifelse show-density-map?
    [ set pcolor scale-color grey count local-flockers (-2 *  max-density) (max-density) ]
    [ set pcolor white ]
  ]
end

to go
  ask flockers [
    update-tail
  ]
  ask flockers [
    set-flock-mates
  ]
  ask links [die]
  if show-links? [
    ask flockers [
      create-links-with flock-mates
    ]
  ]
  ask flockers [
    ifelse use-alignment-effect? [
      set new-v-x mean [v-x] of (turtle-set self flock-mates)
      set new-v-y mean [v-y] of (turtle-set self flock-mates)
    ]
    [
      set new-v-x v-x
      set new-v-y v-y
    ]
    if use-body-force? and any? flock-mates [
      set new-v-x new-v-x - rel-body-force * mean [force-x myself] of flock-mates
      set new-v-y new-v-y - rel-body-force * mean [force-y myself] of flock-mates
    ]
  ]
  ask flockers [
    update-movement-variables
  ]
  ask flockers [
    jump speed ;speed
  ]
  update-local-flockers
  tick
end

to set-flock-mates
  if flock-mates-method = "near" [
    set flock-mates reference-flockers
  ]
  if flock-mates-method = "delaunay" [
    triangulate
  ]
  if flock-mates-method = "lattice" [
    set flock-mates other local-flockers
  ]
end

;; reports the x-component of a force of attraction/repulsion
;; between the asking flocker and flocker t
to-report force-x [t]
  report (sin towards t) * (distance t - preferred-distance)
end

;; reports the y-component of a force of attraction/repulsion
;; between the asking flocker and flocker t
to-report force-y [t]
  report (cos towards t) * (distance t - preferred-distance)
end

;; finalises any change in heading by updating from new-v-x, new-v-y
;; and adding any random perturbation
to update-movement-variables
  set new-heading heading
  if new-v-x != 0 or new-v-y != 0 [
    set new-heading atan new-v-x new-v-y
  ]
  set heading new-heading + ((random-float 2 - 1) * directional-noise)
  set speed sqrt (new-v-x ^ 2 + new-v-y ^ 2)
  set speed min (list max-speed (max (list speed min-speed)))
  set v-x speed * dx
  set v-y speed * dy
end

;; retrieves the set of flockers to be used for adjusting movement
;; this reported could be changed to alter behaviour substantially
to-report reference-flockers
  report turtle-set inner-ring
end

;; selects only the closest other turtle in each 'pie-slice' sector
;; around the asking turtle
to-report inner-ring
  let ring []
  let pie-angle 360 / sectors-to-check
  let range-headings list 0 pie-angle
  let in-cone-candidates other local-flockers with [in-field-of-view? myself self near-range (view-angle / 2)]
  repeat sectors-to-check [
    let candidates in-cone-candidates with [between? range-headings norm-heading (180 + (pie-angle / 2) + towards myself)]
    if any? candidates [
      set ring fput (first sort-on [distance myself] candidates) ring
    ]
    set range-headings map [ rh -> rh + pie-angle ] range-headings
  ]
  report ring
end

to-report between? [min-max x]
  report x >= first min-max and x < last min-max
end

;; this appears a quicker way to emulate a cone-angle
;; than the built-in in-cone reporter
to-report in-field-of-view? [t1 t2 d dh]
  let dist 0
  ask t1 [ set dist distance t2 ]
  report (dist < d) and (in-cone-angle? t1 t2 dh)
end

;; uses the heading-difference reporter and towards to
;; determine if t2 is in the forward-facing cone of angle dh of t1
to-report in-cone-angle? [t1 t2 dh]
  report heading-difference ([towards t2] of t1) [heading] of t1 < dh
end

;; calculates a difference in two headings as the absolute
;; angle between them, accounting for the 0-360 issue
;; does not care about the order, i.e. the turn that would be required
;; to change h1 to h2
to-report heading-difference [h1 h2]
  let h1-n norm-heading h1
  let h2-n norm-heading h2
  let dh abs (h1-n - h2-n)
  if dh > 180 [
    report 360 - dh
  ]
  report dh
end

;; reports a heading 'normalised' to 0 to 360
to-report norm-heading [x]
  report x mod 360
end

;; sends a snapshot to R of the current flock
to snapshot-R
  sr:set "mx" map [ f -> [xcor] of f ] sort flockers
  sr:set "my" map [ f -> [ycor] of f ] sort flockers
  sr:set "x_min" min-pxcor
  sr:set "x_max" max-pxcor
  sr:set "y_min" min-pycor
  sr:set "y_max" max-pycor

  sr:set-plot-device
  sr:run "par(mar = rep(1, 4))"
  (sr:run "plot(mx, my, asp = 1,pch = 20, cex = 2, lwd = 0.5, axes = F, "
               "xlab = '', ylab = '', xlim = c(x_min, x_max), ylim = c(y_min, y_max))")
  sr:run "box()"

  ask flockers [
    let memory-x (list xcor)
    let memory-y (list ycor)
    foreach my-tail [ t ->
      let x-inc [speed * dx] of t
      let y-inc [speed * dy] of t
      set memory-x lput (last memory-x - x-inc) memory-x
      set memory-y lput (last memory-y - y-inc) memory-y
    ]
    sr:set "x" memory-x
    sr:set "y" memory-y
    sr:run "lines(x, y)"
  ]
  user-message "Plot will close when you close this dialog." 
  sr:run "dev.off()"  
end

;; delegates construction of a delaunay triangulation
;; to R - assumes deldir is installed
;;
;; NOTE: the wonder of this is not that it works well, but that it works at all
to triangulate
  let sorted-flockers sort flockers
  ;; make up sets of flockers in each N/S/E/W 'guard' area of the space
  ;; to allow triangulation to handle the toroidal wrapping
  let north filter [f -> [in-wrapped-zone? "N"] of f] sorted-flockers
  let south filter [f -> [in-wrapped-zone? "S"] of f] sorted-flockers
  let east  filter [f -> [in-wrapped-zone? "E"] of f] sorted-flockers
  let west  filter [f -> [in-wrapped-zone? "W"] of f] sorted-flockers
  let wrapped-flockers (sentence sorted-flockers north south east west)
  ;; apply offsets to the x and y coords of each group to make up x y lists
  let x (sentence map [f -> [xcor] of f] sorted-flockers
                  map [f -> [xcor] of f] north
                  map [f -> [xcor] of f] south
                  map [f -> [xcor] of f + world-width] east
                  map [f -> [xcor] of f - world-width] west)
  let y (sentence map [f -> [ycor] of f] sorted-flockers
                  map [f -> [ycor] of f + world-height] north
                  map [f -> [ycor] of f - world-height] south
                  map [f -> [ycor] of f] east
                  map [f -> [ycor] of f] west)
  sr:set "x" x
  sr:set "y" y
  let deldir table:from-list sr:runresult "deldir(x = x, y = y)$delsgs"
  ; note R results are 1-indexed so have to get i-1th entries in wrapped-flockers
  let froms map [i -> item (i - 1) wrapped-flockers] table:get deldir "ind1"
  let tos map [i -> item (i - 1) wrapped-flockers] table:get deldir "ind2"
  ask flockers [ set flock-mates (turtle-set nobody) ]
  (foreach froms tos [ [f t] ->
    if f != t [
      ask f [
        if distance t < near-range [
          set flock-mates (turtle-set flock-mates t)
          ask t [ set flock-mates (turtle-set flock-mates f) ]
        ]
      ]
    ]
  ])
end

;; reports true if turtle is in a zone such that it can be considered as a
;; potential near neighbour due to toroidal wrapping of the netlogo world
;; note that 'N' zone is turtles near the bottom of the world - which when
;; wrapped, are at top...
to-report in-wrapped-zone? [zone]
  (ifelse
    zone = "N" [ report ycor < min-pycor + near-range ]
    zone = "S" [ report ycor > max-pycor - near-range ]
    zone = "E" [ report xcor < min-pxcor + near-range ]
    zone = "W" [ report xcor > max-pxcor - near-range ]
    [ print "***WARNING*** Invalid zone identifier in in-wrapped-zone? reporter"
      report false ])
end
@#$#@#$#@
GRAPHICS-WINDOW
185
10
618
444
-1
-1
17.0
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
24
0
24
1
1
1
ticks
100.0

SLIDER
4
118
176
151
density
density
0.1
2
0.55
0.05
1
NIL
HORIZONTAL

SLIDER
4
152
176
185
directional-noise
directional-noise
0
30
2.5
2.5
1
NIL
HORIZONTAL

SLIDER
4
194
176
227
min-speed
min-speed
0
0.5
0.2
0.05
1
NIL
HORIZONTAL

BUTTON
23
18
87
51
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
93
19
156
52
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
93
59
155
92
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
631
223
802
256
tail-length
tail-length
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
629
28
801
61
view-angle
view-angle
5
360
205.0
5
1
NIL
HORIZONTAL

SLIDER
629
66
801
99
sectors-to-check
sectors-to-check
2
12
6.0
1
1
NIL
HORIZONTAL

TEXTBOX
629
10
779
28
Near search criteria
11
0.0
1

SLIDER
4
376
176
409
rel-body-force
rel-body-force
0
2
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
4
414
176
447
preferred-distance
preferred-distance
0
2
0.6
0.05
1
NIL
HORIZONTAL

TEXTBOX
7
101
157
119
Basic setup parameters
11
0.0
1

TEXTBOX
8
356
177
375
Attraction-repulsion ('body force')
11
0.0
1

SWITCH
5
315
153
348
use-body-force?
use-body-force?
0
1
-1000

TEXTBOX
809
169
966
214
Note that some settings may produce unexpected effects and slow the model!
11
15.0
1

SWITCH
5
278
153
311
use-alignment-effect?
use-alignment-effect?
0
1
-1000

SWITCH
644
184
792
217
show-density-map?
show-density-map?
1
1
-1000

PLOT
630
263
879
464
flocker speeds
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range (precision (min-speed - 0.025) 3) (precision (max-speed + 0.025) 3)\nset-histogram-num-bars 10\nhistogram [speed] of flockers"

SLIDER
4
230
176
263
max-speed
max-speed
min-speed
2
0.2
0.05
1
NIL
HORIZONTAL

SWITCH
644
145
792
178
show-links?
show-links?
0
1
-1000

CHOOSER
807
27
958
72
flock-mates-method
flock-mates-method
"near" "lattice" "delaunay"
0

TEXTBOX
810
77
975
167
\"near\" uses local 'pie slice' method\n\"lattice\" uses the patch 'locale'\n\"delaunay\" uses delaunay triangulation (requires R-deldir)\n
11
0.0
1

SLIDER
630
106
802
139
near-range
near-range
preferred-distance
5
1.5
0.1
1
NIL
HORIZONTAL

BUTTON
843
219
949
252
NIL
snapshot-R
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

This model incorporates ideas from

+   Czirók A and Vicsek T 2000 Collective behavior of interacting self-propelled particles. _Physica A: Statistical Mechanics and its Applications_ **281** 17–29.
+   Grégoire G, Chaté H and Tu Y 2003 Moving and staying together without a leader. _Physica D: Nonlinear Phenomena_ **181** 157–170.
+   Vicsek T, Czirók, A. A, Ben-Jacob E, Cohen I and Sochet O 1995 Novel type of phase transition in a system of self-driven particles. _Physical Review Letters_ **75** 1226–1229.

which are discussed in detail in Chapters 4 and 6 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

This version requires the Simple R extension including the `deldir` library, if you wish to use `delaunay` as the `flock-mates-method` option, although it is important to note that this option may be unreliable, and will run _slowly_.

An alternative version of the model that does not require the R-netlogo extension is available.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

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
