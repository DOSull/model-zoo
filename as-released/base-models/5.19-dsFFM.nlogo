; Copyright (c) 2011-24 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

;; extensions [r]

globals 
[
  burned-trees-total
  burned-trees-fire    ;; how many have burned so far
  max-area
  max-total-area
  plot-interval
  count-fires
  fire-front
  
  prop-trees
]

to setup
  
  clear-all 
  set-default-shape turtles "square"
  ask patches [ set pcolor brown ]
  
  set count-fires 0
  
  set prop-trees []
  
  set burned-trees-total []
  set max-total-area (world-width * world-height)
  set plot-interval round (max-total-area / 50)
  
 
  ask n-of (initial-trees * max-total-area) patches [set pcolor green]
  
  ;; r:setPlotDevice
  
  reset-ticks
  
end

to go

  set burned-trees-fire 0
  set prop-trees lput (count patches with [pcolor = green] / max-total-area) prop-trees
  
  ask patches with [ pcolor = red ] [ set pcolor brown ]
  
  ;; grow the forest
  ask n-of (int (1 / spark-freq)) patches with [pcolor != green] [set pcolor green]

  spark

  set count-fires count-fires + 1
  if count-fires mod plot-refresh = 0
  [
   plotting
  ]
  
  tick
end


to spark ;; modified spark procedure
  set burned-trees-fire 0
  ask one-of patches [
    if pcolor = green 
    [ 
      set burned-trees-fire burned-trees-fire + 1
      set fire-front patch-set self ;; a new fire-front
      spread
    ]
  ]
  
  ;; update fire history data (fire size list and max fire size)
  if burned-trees-fire > 0 [set burned-trees-total lput burned-trees-fire burned-trees-total]
  if not empty? burned-trees-total [ set max-area max burned-trees-total ]
  set burned-trees-fire 0
  
end


to spread 
  
  while [ any? fire-front ] [ ;; stop when we run out of fire-front
    let new-fire-front patch-set nobody ;; empty set of patches for the next 'round' of the fire
    ask fire-front 
    [
      set pcolor red 

      let N neighbors4

      set N N with [ pcolor = green ]                 ;; only green patches
      set new-fire-front (patch-set new-fire-front N) ;; extend the next round front (assume *all* combustible nhbs burned)
      set burned-trees-fire burned-trees-fire + count N
    ]
    set fire-front new-fire-front
  ]

end


to plotting

  
  ;; following James and Plank (2007)                                                   
  set-current-plot "Freq-Area Distribution (log-log)"
  ;; set-current-plot-pen "fire_size_"
    clear-plot
    let log-burned-trees-total map [ln ?] reverse sort burned-trees-total
    let rank n-values (length log-burned-trees-total) [? + 1]
    let N length log-burned-trees-total
    
    (foreach rank log-burned-trees-total [
      plotxy ln (?1 / N) ?2
    ])
 
 set-current-plot "Prop Trees"
  plot count patches with [pcolor = green ] / max-total-area
end

to-report mle-exponent [size-list xmin]
  let b 1
  let trunc-size-list filter [? >= xmin] size-list
  let mle-est map [(log ? 2) / xmin] size-list
  set b 1 + ((sum mle-est) ^ -1) 
  report b
end

;; R plotting code
;to plot-size-freq-r
; r:put "size" burned-trees-total
; r:eval("size <- unique(sort(size, decreasing = TRUE))")
; r:eval("N <- length(size)")
; r:eval("rnk <- 1:N")
; 
; r:eval("plot(log(rnk/N) ~ log(size), las = 1, xlab = 'ln Rank', ylab = 'ln Size', pch = 19, xlim = c(0,10), ylim = c(-7, 0))")
; ;; log in R is ln in Netlogo.
; 
;end  
;
;to plot-prop-trees
;  r:eval("have.zoo <- 'package:zoo' %in% search()")
;  show r:get "have.zoo"
;  r:eval("if(have.zoo) {library(zoo)}")
;  
;  r:put "p" prop-trees
;  r:eval("plot(p, type = 'l', las = 1, xlab = 'Time', ylab = 'Prop. trees', ylim = c(0,1))")
;  r:eval("if(have.zoo) {lines(rollmean(p, 101), col = 'red')}")
;end  
;
to sizes-to-file
  file-open "size-freq.txt"

  foreach burned-trees-total
     [ file-print ? ]
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
249
10
659
441
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
199
0
199
1
1
1
ticks
100.0

BUTTON
106
15
169
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
174
15
237
48
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
0

SLIDER
65
59
237
92
spark-freq
spark-freq
0
1
0.0055
0.0001
1
NIL
HORIZONTAL

PLOT
8
144
242
361
Freq-Area Distribution (log-log)
ln (Fire Size)
ln (Rank)
0.0
4.5
0.0
3.0
true
false
"" ""
PENS
"default" 0.1 2 -16777216 true "" ""

SLIDER
64
96
236
129
initial-trees
initial-trees
0
1
0.4
0.01
1
NIL
HORIZONTAL

PLOT
677
22
952
205
Prop Trees
Time
Prop Trees
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks count patches with [pcolor = green ] / max-total-area"

MONITOR
679
254
736
299
beta
mle-exponent burned-trees-total size-min
3
1
11

SLIDER
753
258
925
291
size-min
size-min
1
1000
1
1
1
NIL
HORIZONTAL

SLIDER
13
368
185
401
plot-refresh
plot-refresh
1
100
100
1
1
NIL
HORIZONTAL

TEXTBOX
13
405
207
436
This controls how often (nth fires) histogram is updated (speeds up model)
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model is an implementation of 

+   Bak P and Chen K 1990 A forest-fire model and some thoughts on turbulence. _Physics Letters A_ **147** 297–300.

as referenced in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   Bak P and Chen K 1990 A forest-fire model and some thoughts on turbulence. _Physics Letters A_ **147** 297–300.
+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-24 David O'Sullivan and George L. W. Perry

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
