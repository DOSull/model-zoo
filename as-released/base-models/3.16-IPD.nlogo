; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

globals [
  rewards ;; a list of the returns to DD CD DC CC from
          ;; perspective of the first player
  palette
]

patches-own [
  s-number
  s-list ;; a list of the next choice to DD DC CD CC
           ;; encoded as
           ;; 1 = cooperate
           ;; 0 = defect
  previous-outcome
  recent-scores
  N5
]


to-report strategy-from-number [x]
  let s []
  repeat 4 [
    set s fput (x mod 2) s
    set x floor (x / 2)
  ]
  report s
end

to-report number-from-strategy [s]
  report sum (map [?1 * ?2] s (reverse n-values 4 [2 ^ ?]))
end


to setup
  clear-all
  setup-palette
  set rewards (list C-C C-D D-C D-D)
  ask patches [
    set s-number random 16
    set s-list strategy-from-number s-number
    set previous-outcome random 4 
    set recent-scores []
    set N5 (patch-set self neighbors4)
  ]
  update-display
  reset-ticks
end

to setup-palette
  set palette (list 
    black (violet + 3) (violet - 2) (red + 3)
    (red - 2) orange (yellow - 3) (green - 3) 
    green (green + 2) (blue - 2) (blue + 3)
    brown grey yellow white
  )
end


to update-display 
  ask patches [
    set pcolor item s-number palette
    set plabel s-number
    ifelse pcolor mod 10 >= 5
    [ set plabel-color black ]
    [ set plabel-color white ] 
  ]
end



to go 
  ask patches [
    let opponent nobody
    ifelse spatial?
    [ set opponent one-of neighbors4 ]
    [ set opponent one-of patches ]
    let my-go choice
    let their-go [choice] of opponent
    set previous-outcome 2 * my-go + their-go
    update-score
    ask opponent [
      set previous-outcome 2 * their-go + my-go
      update-score
    ]
  ]
  let top-scores patches with-max [mean recent-scores]
  ask patches [
    ifelse random-float 1 < (1 - p-mutate) [
      ifelse spatial?
      [ set s-number [s-number] of one-of N5 with-max [mean recent-scores] ]
      [ set s-number [s-number] of one-of top-scores ]
      set s-list strategy-from-number s-number
    ]
    [
      mutate-strategy
    ]
  ]
  update-display
  tick
end


to mutate-strategy
  let i random 4
  let new-val 1 - item i s-list
  set s-list replace-item i s-list new-val
  set s-number number-from-strategy s-list
end


to-report choice
  report item previous-outcome s-list
end


to update-score
  set recent-scores fput (item previous-outcome rewards) recent-scores
  if length recent-scores > score-memory [
    set recent-scores but-last recent-scores
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
670
491
-1
-1
9.0
1
6
1
1
1
0
1
1
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
7
10
70
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
7
48
70
81
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
86
71
119
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
20
184
192
217
p-mutate
p-mutate
0
0.01
0.01
0.001
1
NIL
HORIZONTAL

TEXTBOX
10
233
205
494
     me-them previous round\n#      C-C C-D D-C D-D\n0        C     C     C     C   always-C\n1        C     C     C     D\n2        C     C     D     C\n3        C     C     D     D   stubborn\n4        C     D     C     C\n5        C     D     C     D   tit-for-tat\n6        C     D     D     C   win-stay-lose-shift\n7        C     D     D     D   retaliator\n8        D     C     C     C\n9        D     C     C     D\n10      D     C     D     C   bully\n11      D     C     D     D\n12      D     D     C     C   fickle\n13      D     D     C     D\n14      D     D     D     C\n15      D     D     D     D  all-D
10
0.0
1

INPUTBOX
712
77
772
137
C-C
4
1
0
Number

INPUTBOX
776
76
832
136
C-D
0
1
0
Number

INPUTBOX
712
141
772
201
D-C
5
1
0
Number

INPUTBOX
777
140
831
200
D-D
2
1
0
Number

SLIDER
21
145
193
178
score-memory
score-memory
1
5
1
1
1
NIL
HORIZONTAL

TEXTBOX
704
55
824
73
Pay off matrix, me-them
11
0.0
1

TEXTBOX
702
84
717
102
R
11
0.0
1

TEXTBOX
837
84
852
102
S
11
0.0
1

TEXTBOX
701
149
716
167
T
11
0.0
1

TEXTBOX
836
147
851
165
P
11
0.0
1

TEXTBOX
704
224
854
266
For prisoner's dilemma\nT > R > P > S and\n2R > S + T
11
0.0
1

BUTTON
731
272
813
305
setup-PD
set C-C 4\nset C-D 0\nset D-C 5\nset D-D 2
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
707
330
857
358
For chicken games\nT > R > S > P
11
0.0
1

BUTTON
733
367
841
400
setup-chicken
set D-C 3\nset C-C 2\nset C-D 1\nset D-D 0
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
722
453
845
486
spatial?
spatial?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This is an implementation of a generic spatial evolutionary game 'arena' in which patches enact the strategies detailed in the table presented in the interface, and adapt their strategies to match those of the most successful other patches in the system.  When the `spatial?` switch is turned on, the new strategy is chosen from the most successful neighbouring strategies (including the current one) otherwise it is chosen based on the most successful strategy in the whole system.  Success is measured by defined by the mean pay-off achieved over the most recent `score-memory` rounds of play.

See Chapter 3 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

for a brief discussion. 

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-13 David O'Sullivan and George L. W. Perry

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact David O'Sullivan at d.osullivan@auckland.ac.nz, or George Perry at george.perry@auckland.ac.nz
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
