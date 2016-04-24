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

extensions [gradient r]

globals
[
  overall-lambda
]

patches-own
[
  lambda
  nn-distance
  xP
  yP
]

turtles-own
[
  x
  y
  component
]


; Note that the simulations are assumed to take place on the unit square
; the resolution is purely graphical (to display intensity)
to setup

  clear-all
  resize-world 0 (resolution - 1) 0 (resolution - 1)
  set-patch-size  ( 100 / resolution ) * 4
  reset-ticks

end

to go

  setup

  build-cluster


  ask turtles
  [
    ;; set size 1 * ( resolution / 100)
    set nn-distance (distance min-one-of other turtles [distance myself])
    set x xcor / resolution
    set y ycor / resolution
  ]

end

to plot-intensity

 ask patches [
   set lambda count turtles-here
 ]

 repeat smooth [ diffuse lambda 0.9 ]

 let max-lambda max [lambda] of patches

ask turtles [
   set color black
 ]

 ask patches [
   set pcolor gradient:scale [[239 138 98] [247 247 247] [103 169 207] ] lambda max-lambda 0
 ]


end


to build-cluster

  ;; 1. Generate the parent clusters (homog Poisson process)
  create-turtles n-parent-clusters
  [
    setxy random-xcor random-ycor
    format-turtle "parent"
  ]

  ;; let mean-offspring n / n-clusters-g1

  ;; 2. Disperse 'offspring' around the parent clusters and ...
  ;; first generation
  let mean-n-g1  n / n-parent-clusters

  ask turtles with [component = "parent"]
  [

    let n-offspring random-poisson mean-n-g1
    let sd-px sd-displacement-g1 * resolution

    hatch n-offspring
    [

      let xy-check false
      let cand-x [xcor] of myself
      let cand-y [ycor] of myself

      ;; bounds checking to stop points being placed outside the world
      while [xy-check = false]
      [
        set cand-x random-normal [xcor] of myself sd-px
        set cand-y random-normal [ycor] of myself sd-px

        if xy-bounds-check cand-x cand-y [ set xy-check true]
      ]


      setxy cand-x cand-y
      format-turtle "offspring-1"
    ]
  ]

  ;; second generation
  let mean-n-g2 n / (count turtles with [component = "offspring-1"])

  ask turtles with [component = "offspring-1"]
  [


      let n-offspring-2 random-poisson mean-n-g2
      let sd-px sd-displacement-g2 * resolution

      hatch n-offspring-2
      [

        let xy-check false
        let cand-x [xcor] of myself
        let cand-y [ycor] of myself

        ;; bounds checking to stop points being placed outside the world
        while [xy-check = false]
        [
          set cand-x random-normal [xcor] of myself sd-px
          Set cand-y random-normal [ycor] of myself sd-px

        if xy-bounds-check cand-x cand-y [ set xy-check true]
      ]


      setxy cand-x cand-y
        format-turtle "offspring-2"
      ]
  ]

  ;; 3. Thin the pattern to ensure the exact number of points specified is produced
  ;let excess count turtles - overall-lambda
 ;
  ;if excess > 0
  ;[
  ;  ask n-of excess turtles [die]
  ;]

end

to format-turtle [comp]

  set color white
  set shape "circle"
  set size 1.0 * (resolution / 100)
  set component comp
  set x xcor / resolution
  set y ycor / resolution

end

to colour-by-type

  ask turtles
  [
    if component = "parent" [
      set size 3
       set color red ]
    if component = "offspring-1" [set size 2
       set color orange ]
    if component = "offspring-2" [set color yellow ]
  ]

end

to plot-K
;; modified from the example R extension code

  r:eval "library(spatstat)"

  ;; send agent variables into an R data-frame
  (r:putagentdf "agentset" turtles "who" "x" "y")

  ;; create point pattern with vectors of x- and y-coordinates of turtles and the dimension of the window/world
  let revalstring (word "agppp <- ppp(agentset$x, agentset$y)")  ; don't need a window on the unit square
  r:eval revalstring

  ;; calculate K
  r:eval "k <- Kest(agppp, method = 'c')"

  ;; get results from R
  let k r:get "k$iso"
  let r r:get "k$r"
  let theo r:get "k$theo"


;  ;; combine results into a multidimensional list for plotting
  let ripley (map [(list ?1 ?2 ?3)] r k theo)
;
;  ;; plot the results
  clear-plot
  foreach ripley
  [
    set-current-plot "Ripley's K"
    set-current-plot-pen "K(r)"
    plotxy (item 0 ?) (item 1 ?)
    set-current-plot-pen "theo"
    plotxy (item 0 ?) (item 2 ?)
  ]

end

to-report xy-bounds-check [cx cy]

  report cx > 0 and cx <= max-pxcor and cy > 0 and cy <= max-pycor

end

to dump-to-file

  file-open "doubleThomas.txt"
  file-print "x y type"

  ask turtles
  [
    file-type xcor  file-type " " file-type ycor file-type " " file-print component
  ]

  file-close
end


@#$#@#$#@
GRAPHICS-WINDOW
210
10
620
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
1
1
1
ticks
30.0

BUTTON
34
332
169
366
generate pattern
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

SLIDER
19
225
191
258
resolution
resolution
1
200
50
1
1
NIL
HORIZONTAL

TEXTBOX
27
261
177
321
Controls grain of underlying grid - pixel size will need to be manually  adjusted in the World View options.
11
0.0
1

BUTTON
45
372
155
405
plot intensity
plot-intensity
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
755
89
905
137
Note this model uses the gradient extension and the R library spatstat
12
0.0
1

SLIDER
21
92
193
125
sd-displacement-g1
sd-displacement-g1
0
1.0
0.05
.01
1
NIL
HORIZONTAL

PLOT
752
154
1071
386
Ripley's K
r
K(r)
0.0
0.2
0.0
0.2
true
true
"" ""
PENS
"K(r)" 1.0 0 -16777216 true "" ""
"theo" 1.0 0 -2674135 true "" ""

BUTTON
913
101
1034
134
Plot K
plot-k
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
23
47
195
80
n-parent-clusters
n-parent-clusters
0
100
20
1
1
NIL
HORIZONTAL

TEXTBOX
33
191
183
219
Scaled to the unit square not Netlogo world dimensions
11
0.0
1

BUTTON
26
410
172
443
identify components
colour-by-type
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
4
450
96
483
hide points
ask turtles [set hidden? true]
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
104
448
201
481
show points
ask turtles [set hidden? false]
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
17
502
189
535
smooth
smooth
1
50
50
1
1
NIL
HORIZONTAL

SLIDER
22
131
194
164
sd-displacement-g2
sd-displacement-g2
0
1
0.01
.005
1
NIL
HORIZONTAL

SLIDER
23
11
195
44
n
n
0
2000
300
5
1
NIL
HORIZONTAL

BUTTON
312
557
436
590
remove 'parents'
ask turtles with [component != \"offspring-2\"] \n [die]
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
441
557
549
590
pattern to file
dump-to-file
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

This model is an implementation of Conway's game of life.  See:

+    Gardner M 1970 Mathematical games: the fantastic combinations of John Conway�s new solitaire game �life�. _Scientific American_ **223**, 120�123.

This is an example model referenced in Chapter 1 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.


## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

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
