;; The MIT License (MIT)
;;
;; Copyright (c) 2011-2016 David O'Sullivan and George Perry
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

extensions [nw]

breed [nodes node]

patches-own [
  time-tag-1
  time-tag-2
  time-tag-3
  occupied?
  loop-count
  cluster-id
  cluster-set?
  spanning?  ;; patch part of a spanning cluster?
  elastic?   ;; patch part of elastic backbone?
  backbone?  ;; patch part of percolation backbone?
]

globals [
  fire-front
  spanning-present?   ;; bool flag to denote presence of spanning cluster
  hit-bone
  horizontal?
  vertical?
  progress
]


to setup
  clear-all
  set progress "setup"
  ask patches [ set-default-state ]
  ask n-of (p * count patches) patches [
    set occupied? true
  ]
  ask patches with [occupied?] [set pcolor white]
  set spanning-present? false
  set horizontal? false
  set vertical? false
  reset-ticks
end

to set-default-state
  set cluster-id -1
  set loop-count 0
  set occupied? false
  set spanning? false
  set elastic? false
  set backbone? false
  set time-tag-1 -1
  set time-tag-2 -1
  set time-tag-3 -1
end

to tag
  set spanning-present? false
  identify-clusters
  set progress "clusters identified"
  tick
end

to-report is-horizontal-spanning? [p-set]
  report (min [pxcor] of p-set = min-pxcor) and (max [pxcor] of p-set = max-pxcor)
end

to-report is-vertical-spanning? [p-set]
  report (min [pycor] of p-set = min-pycor) and (max [pycor] of p-set = max-pycor)
end

to find-backbone
  ifelse spanning-present? [
    ;; burn the spanning cluster starting near a corner
    run-burn-pass-1
;    run-burn get-start-cell 1
    set progress "pass one finished"

    ;; burn the spanning cluster starting from the other end
    run-burn-pass-2
;    run-burn get-end-cell 2
    set progress "pass two finished"

    ;; burn starting from all cells with a loop count greater than one
    run-burn-pass-3
;    run-burn patches with [loop-count >= 1] 3
    set progress "pass three finished"
  ]
  [
    user-message("No spanning cluster - stopping.")
  ]
end

;; reports a cell near one end of one of the edges
to-report get-start-cell
  let c nobody
  ifelse vertical?
  [ set c one-of patches with [spanning? and pycor = max-pycor] with-min [pxcor] ]
  [ set c one-of patches with [spanning? and pxcor = max-pxcor] with-min [pycor] ]
  ask c [ mark-with-x green ]
  report c
end

;; reports a cell on the opposite edge from the start cell,
;; at maximum distance by fire-spread steps
to-report get-end-cell
  let c nobody
  ifelse vertical?
  [ set c one-of patches with [pycor = min-pycor] with-max [time-tag-1] with-max [pxcor] ]
  [ set c one-of patches with [pxcor = min-pxcor] with-min [pxcor] with-max [time-tag-1] with-max [pycor] ]
  ask c [ mark-with-x blue ]
  report c
end

to mark-with-x [col]
  sprout 1 [
    set shape "X"
    set size 2.5
    set color col
  ]
end

;; forward search to tag sites in spanning cluster with distance from a source
;; selected near a corner on one of the edges
to run-burn-pass-1
  let clock 0
  ask get-start-cell [
    set time-tag-1 clock
    set fire-front patch-set self
  ]
  while [ any? fire-front ] [
    set clock clock + 1
    ;; Empty set of patches for next 'round' of fire
    let new-fire-front patch-set nobody
    ask fire-front [
      ;; here we only need to burn the spanning cluster
      ;; also detect 'loops'
      ask neighbors4 with [ spanning? ] [
        ifelse time-tag-1 = -1 [
          ;; this is the first arrival
          set time-tag-1 clock
          set new-fire-front (patch-set new-fire-front self)
        ]
        [ ;; this is a second or later path, so increment loop-count
          set loop-count loop-count + 1
        ]
      ]
    ]
    set fire-front new-fire-front
  ]
end

;; search back from end cell to find elastic backbone
to run-burn-pass-2
  let clock 0
  ask get-end-cell [
    set time-tag-2 clock
    set fire-front patch-set self
    set elastic? true
    set backbone? true
  ]
  while [ any? fire-front ] [
    set clock clock + 1
    ;; Empty set of patches for next 'round' of fire
    let new-fire-front patch-set nobody
    ask fire-front [
      ;; this time we only burn cells with lower time-tag in pass 1 than ourself
      let my-time-tag-1 time-tag-1
      ask neighbors4 with [ spanning? and time-tag-1 < my-time-tag-1 ] [
        set elastic? true
        set backbone? true
        set time-tag-2 clock
        set new-fire-front ( patch-set new-fire-front self)
      ]
    ]
    set fire-front new-fire-front
  ]
end

;; final pass checks out the loops
;; using the nw extension
;; THIS ONE IS UNCLEAR
to run-burn-pass-3
  nw:set-context nodes links
  ask patches with [time-tag-1 > -1] [
    sprout-nodes 1 [
      set size 0.5
      set color grey
    ]
  ]
  ask nodes [
    ask nodes-on neighbors4 [
      create-link-with myself []
    ]
  ]
  ask nodes-on patches with [backbone?] [set color red]
  let candidate-links (link-set [my-links] of nodes-on patches with [not backbone?])
  while [any? candidate-links] [
    let my-nodes nobody
    ask one-of candidate-links [
      set my-nodes both-ends
      die
    ]
    let wcc nw:weak-component-clusters
    ifelse length wcc > 1 [
      ;; breaks the cluster, so 'dangling'
      foreach wcc [
        ask ? [
          if length remove-duplicates [color] of ? = 1 [
            ask ? [ die ]
          ]
        ]
      ]
    ]
    [ ;; restore link
      let ns sort my-nodes
      ask first ns [
        create-link-with last ns [
          set color black
        ]
      ]
    ]
    set candidate-links (link-set [my-links with [color != black]] of nodes-on patches with [not backbone?])
  ]
  ask nodes [
    set backbone? true
    set pcolor pink
    die
  ]
end

to-report two-colors?
  report length remove-duplicates map [[color] of ?] sort both-ends > 1
end

to colour-cluster
  ifelse progress = "pass three finished" [
    ask patches with [occupied? and spanning? and not backbone?] [
      set pcolor yellow
    ]
    ask patches with [backbone?] [
      set pcolor orange + 1
    ]
    ask patches with [elastic?] [
      set pcolor red
    ]
  ]
  [
    user-message "You must identify the backbone first - STOPPING"
  ]
end


;; similar to the cluster identification in model 5.1
to identify-clusters
  let cluster-count 0
  ask patches [
    set cluster-set? true
  ]
  let occupied-sites patches with [ occupied? ]
  ask occupied-sites [
    set cluster-set? false
  ]
  while [any? occupied-sites with [ not cluster-set? ]] [
    ;; lists are mutable so we use those here, starting with
    ;; a randomly selected untagged patch
    let patches-to-tag (patch-set one-of occupied-sites with [ not cluster-set? ])
    while [any? patches-to-tag] [
      let next-patches-to-tag patch-set nobody ;; a list of the next set of patches in the same cluster
      ask patches-to-tag [
        ;; tag as counted and mark for statistical purposes
        set cluster-set? true
        set cluster-id cluster-count
      ]
      ask patches-to-tag [
        ;; add untagged neighbours to the set to be tagged next
        ask neighbors4 with [ not cluster-set? ] [
          set next-patches-to-tag (patch-set self next-patches-to-tag)
        ]
      ]
      set patches-to-tag next-patches-to-tag
    ]
    ;; check here whether it is a spanning cluster
    let focal-cluster patches with [cluster-id = cluster-count]
    if count focal-cluster >= min (list world-width world-height) [
      let h? is-horizontal-spanning? focal-cluster
      let v? is-vertical-spanning? focal-cluster
      if h? or v? [
        ask focal-cluster [
          set spanning? true
        ]
        set spanning-present? true
        set horizontal? h?
        set vertical? v?
      ]
    ]
    set cluster-count cluster-count + 1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
224
10
634
441
-1
-1
8.0
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
49
0
49
0
0
0
ticks
100.0

BUTTON
48
15
115
48
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
121
15
184
48
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

SLIDER
31
134
203
167
p
p
0.5
0.7
0.594
.001
1
NIL
HORIZONTAL

BUTTON
48
54
185
87
find-backbone
find-backbone
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
41
238
191
268
Note - cluster tagging can be slow - be pat	ient!
11
0.0
1

BUTTON
48
93
184
126
NIL
colour-cluster
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
58
182
182
227
NIL
progress
0
1
11

TEXTBOX
646
15
833
73
White = occupied, not spanning\nYellow = spanning\nOrange = backbone\nRed = elastic backbone
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model demonstrates the concept of the _backbone_ of a percolation cluster.  See:

+   Herrmann HJ, Hong DC and Stanley HE 1984 Backbone and elastic backbone of percolation clusters obtained by the new method of ‘burning’. _Journal of Physics A: Mathematical and General_ **17** L261–L266

and the discussion in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   Herrmann HJ, Hong DC and Stanley HE 1984 Backbone and elastic backbone of percolation clusters obtained by the new method of ‘burning’. _Journal of Physics A: Mathematical and General_ **17** L261–L266
+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-2016 David O'Sullivan and George Perry

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
NetLogo 5.3
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
