; Copyright (c) 2011-24 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

extensions [gradient]

globals [
  fire-size
  fire-size-list
  
  world-size
  const
  fFreq
  fire-front
  
  elapsed-time
]     

patches-own [
  age
  flammability
]

to setup
  clear-all
  
  ifelse fireFrequency > 0
      [set fFreq 1 / fireFrequency]
      [set fFreq 0]
  set world-size world-height * world-width
  ask patches [ 
    set age 1
    set flammability get-flamm age
  ]
  set fire-size-list[]
  
  ;; r:setPlotDevice
  reset-ticks
end


to go
  if length fire-size-list > 300 [stop]
  let next-fire random-poisson fireFrequency

  ask patches [
    set age age + next-fire
  ]
  let max-age max [age] of patches
  
  ask patches [  
    set pcolor gradient:scale [[229 245 249] [153 216 201] [44 162 95]] age 0 max-age
    if flamm-model != "constant" [set flammability get-flamm age]
  ]
  spark  
  update-histo
  
  set elapsed-time elapsed-time + next-fire
  tick
end


to spark ;; modified spark procedure
  set fire-size 0
  
  ask one-of patches [
      set age 0
      set fire-front patch-set self ;; a new fire-front
      set fire-size fire-size + 1      
      spread
  ]
  ;; update fire history data (fire size list and max fire size)
  set fire-size-list lput fire-size fire-size-list
end


to spread 
  while [ any? fire-front ]  ;; stop when we run out of fire-front
  [
    let new-fire-front patch-set nobody ;; empty set of patches for the next 'round' of the fire
    ask fire-front [
      set pcolor red 

      ask neighbors with [ age > 0] [
        if random-float 1 <= flammability [
          set age 0
          set new-fire-front (patch-set new-fire-front self) ;; extend the next round front
          set fire-size fire-size + 1
        ]
      ]    
      set fire-front new-fire-front
    ]
  ]  
end


to-report get-flamm [t]
  let f 0
  ifelse flamm-model = "constant"
  [ set f 0.23]
  [ set f 0.025 + 0.0003 * (age / 10) ^ 2 ]
  report f
end


to update-histo
  set-current-plot "Fire size-frequency"
  clear-plot
  let log-fire-size-list map [ln ?] reverse sort fire-size-list
  let rank n-values (length log-fire-size-list) [? + 1]
  let N length log-fire-size-list
  
  (foreach log-fire-size-list rank [
    plotxy ?1 ln (?2 / N)
  ])
end


to-report mle-exponent [size-list xmin]
  let b 1
  let trunc-size-list filter [? >= xmin] size-list
  let mle-est map [(log ? 2) / xmin] size-list
  set b 1 + ((sum mle-est) ^ -1) 
  report b
end

;; R plotting code - version of the netlogo plots
;;to plot-mosaic-r
;;  r:put "nr" world-height
;;  r:put "nc" world-width
;;  
;;  r:eval("library(matlab)")
;;  
;;  r:put "z" map [[age] of ?] reverse sort patches
;;  
;;  r:eval("grey.ramp <- grey((15:0)/16)")
;;  r:eval("z <-matrix(z, nrow=nr, ncol=nc, byrow = TRUE)")
;;  r:eval("z <- rot90(z)")
;;    
;;  r:eval("z <- ifelse(z < 0, NA, z)")
;;  
;;  r:eval("image(z, col = grey.ramp, asp = 1, xaxt = 'n', yaxt = 'n')")
;;end
;;
;;to plot-size-freq-r
;; r:put "size" fire-size-list
;; r:eval("size <- unique(sort(size, decreasing = TRUE))")
;; r:eval("N <- length(size)")
;; r:eval("rnk <- 1:N")
;; 
;; r:eval("plot(log(rnk/N) ~ log(size), las = 1, xlab = 'ln Size', ylab = 'ln Prop. rank', pch = 19)")
;; ;; log in R is ln in Netlogo.
;;end
@#$#@#$#@
GRAPHICS-WINDOW
260
13
730
504
-1
-1
1.8
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
255
0
255
1
1
1
ticks
30.0

BUTTON
18
26
82
59
Setup
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
89
26
152
59
Go
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
25
76
202
109
fireFrequency
fireFrequency
0
200
10
5
1
NIL
HORIZONTAL

MONITOR
167
24
240
69
Year
elapsed-time
0
1
11

PLOT
25
212
225
362
Landscape age
Time
Ave. Age (Yrs)
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"type4" 1.0 0 -16777216 true "" "plotxy elapsed-time (mean [age] of patches)"

SLIDER
24
114
201
147
largestFireSize
largestFireSize
0.01
1
1
0.01
1
NIL
HORIZONTAL

MONITOR
816
13
887
58
Largest Fire
max fire-size-list
0
1
11

MONITOR
894
14
971
59
Ave. Fire Size
mean fire-size-list
1
1
11

PLOT
743
62
970
208
Age structure
Vegetation age
Frequency
1.0
1.0
0.0
15000.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range 0 (max [age] of patches + 1)\nset-histogram-num-bars 20\nhistogram [age] of patches"

PLOT
743
210
969
365
Fire size-frequency
ln (Size)
ln (Prop. rank)
0.0
2.0
0.0
0.0
true
false
"" ""
PENS
"default" 250.0 2 -16777216 true "" ""

TEXTBOX
266
510
592
538
Colour represents vegetation age from light (young) to dark (old).
11
0.0
0

MONITOR
742
12
810
57
No. of fires
length fire-size-list
0
1
11

PLOT
25
365
225
515
Old-growth abundance
Time
Abundance
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy elapsed-time ((count patches with [age > 300]) / world-size)"

PLOT
743
368
967
518
Average fire size
Time
Ave size
0.0
10.0
0.0
1.0
true
false
"" "if ticks > 0 \n[plotxy elapsed-time mean fire-size-list]"
PENS
"default" 1.0 0 -16777216 true "" ""

CHOOSER
51
155
189
200
flamm-model
flamm-model
"constant" "increasing"
0

MONITOR
997
15
1088
60
mle exponent
mle-exponent fire-size-list min-size
3
1
11

SLIDER
982
162
1104
195
min-size
min-size
1
1000
2
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is a forest fire model with memory effects, such that the flammability of forest is a function of the time since the last fire.  It draws on ideas in

+   Kitzberger T, Ar´aoz E, Gowda JN, Mermoz M and Morales J 2012 Decreases in fire spread probability with forest age promotes alternative community states, reduced resilience to climate variability and large fire regime shifts. _Ecosystems_ **15** 97-112.
+   Peterson GD 2002 Contagious disturbance, ecological memory, and the emergence of landscape pattern. _Ecosystems_ **5** 329-338.

This model is discussed in more detail in Chapter 5 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

## HOW IT WORKS

The 'world' is represented as a grid of cells (pixels).  Each cell contains vegetation, and is described by the time since it was last burned.

1. Forest succession  
Succession is modelled very simply as the 'time since a patch was last disturbed'. The green colours represent different aged vegetation from young (lightest green) to old (darkest green). Five vegetation age classes are represented: 1 = 0-100, 2 = 101-200, 3 = 201-300, 4 = 301-400 and 5 = 400+.  When a patch is burned its age is reset to zero, and so the 'successional clock is reset'. 

2. Fire spread  
Occasionally a fire starts in the landscape.  This occurs at a frequency that the user sets (via the fireFrequency slider).  Patches of vegetation neighbouring those currently on fire have a certain risk of burning too.  Fires continue to spread until no new patches of vegetation are set alight.  

In the 'real' world flammability varies between different vegetation types.  In this model there are two alternatives: (i) the flammability of all vegetation types is the same (i.e. it's constant), and (ii) the flammability of the vegetation increases with age (due to the increasing amounts of litter of dead wood on the forest floor).  You can set these options via the constantFlammability? switch button.

## HOW TO USE IT

You can manipulate the fire regime in three ways in this model:   
1. Fire frequency (fireFrequency slider)  
The frequency component of the disturbance regime describes how often an event happens.  In this case you can set the average time between disturbance events; thus, if you set the slide to twenty you can expect on average there to be a fire every 20 years. Note these are average values so you can  get periods of time with more fires than you'd expect and periods of time with fewer fires that the average predicts.

2. Fire size (largestFireSize slider)  
The size component of the disturbance regime describes the magnitude of events.  This slider lets you set the maximum proportion of the landscaope that can be burned in a single fire.  Note that this does not mean that fires can go out at much smaller sizes. A proportion of zero (0.0) means that there will never be any fires, and a proportion of one  (1.0) means that a single fire might burn the entire landscape.

3. Flammability (constantFlammability? switch)  
This switch lets you set how flammability is calculated.  If the switch is set to 'on' then all vegetation irrespective of age has the same risk of burning.  If the switch is set to 'off' the the likelihood of fire spreading from a burning cell into a unburned neighbouring cell is a function of that neighbouring cells age; flammability increases with age.

You are provided with other information in the form of graphs:   
1. Landscape age - this is a graph of the average age of patches in the landscape plotted vs. time.  
2. Landscape diversity - this is a graph of the diversity the landscape plotted vs. time. Diversity values range from 0 to 1; if diversity is low (cloes to one) this means that the landscape is dominated by a single age class, whereas if diversity is high (cloes to 1) each of the five vegetation age classes are present in roughly equal amounts.  
3. Age structure - this is a bar plot of the abundance of each of the five vegetation age classes in the landscape.  
4. Fire size-frequency - this is a histogram showing the frequency with which fires of different sizes occur.

and 'monitors':  
1. Largest fire size - largest recorded fire size.  
2. Average fire size - average size of fires.  
3. Number of fires - numebr of fire events that have occured.


## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   Kitzberger T, Ar´aoz E, Gowda JN, Mermoz M and Morales J 2012 Decreases in fire spread probability with forest age promotes alternative community states, reduced resilience to climate variability and large fire regime shifts. _Ecosystems_ **15** 97-112.
+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Peterson GD 2002 Contagious disturbance, ecological memory, and the emergence of landscape pattern. _Ecosystems_ **5** 329-338.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-24 David O'Sullivan and George L. W. Perry

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact David O'Sullivan at d.osullivan@auckland.ac.nz, or George Perry at george.perry@auckland.ac.nzRatz, A. 1995. Long-term spatial patterns created by fire: a model oriented towards boreal forests. International Journal of Wildland Fire 5: 25-34.
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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
