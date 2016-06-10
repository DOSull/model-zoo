; Copyright (c) 2011-13 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

;; leGUME - the Grand Unified Model of Everything
;; v1.3 (patch-memory)
;;
;; David O'Sullivan and George Perry
;; see Spatial Simulation: Exploring Pattern & Process, Chapter 8
;; 
;; v1.3.1 fix to bug in relocate - 11 / 5 / 2012
;; v1.4.1 another attempted bug fix to relocate 12 / 5 / 2012
;; v1.4.1 provision made for non-square islands with the-island, the-shore, the-sea patch-sets and on-island? flag
;; v1.5 mods to hunting kill rate
;; v1.5.1 some tidy ups while documenting in chapter 8
;; v1.6.0 population growth to stochastic exponential, irregular island option (had to modify pick-by-unfamiliarity)


__includes[ "8.1-simmap_code_v1.8.nls" 
            "8.1-landscape-demography_v1.8.nls" 
            "8.1-hunting-and-gathering_v1.8.nls" 
            "8.1-human-demography_v1.8.nls" 
            "8.1-utilities_v1.8.nls" ]

globals [ 
  clusters          ;; a list of patch-sets of patches in each cluster
  the-shore
  the-island
  the-sea

  ;; evalutation variables
  potted-history
  max-acquired
  mean-acquired
  max-pop
  max-groups
  prop-acquired
  total-resource-start
  t50
]

breed [ groups group ]
breed [ spots spot ] ;; these mark hunting locations and record their yield at last visit

groups-own [
  pop               ;; total population of group
  home-camp         ;; current camp location
  my-hunting-spots  ;; memory about high-value resource i.e hunting success - an agent-set of spots
  
  search-tortuosity ;; chance of changing direction each step during search 
                    ;; 1: pure random walk 
                    ;; 0: straight lines
                    ;; in-between correlated RW
  hunting-trips     ;; number of hunting trips this month - affects local resource collection
                    ;; recalculated each month, but recorded to allow passing info on to later code
  
  hunt-kill-to-date ;; this year
  hunt-take-to-date ;; this year
  local-collected   ;; this year
  total-collected   ;; total resource collected this YEAR
]

spots-own [
  yield   ;; the total take from this spot last visit
  owner   ;; the owning group - spots are 'exclusive' not shared... which would be a lot more complicated to code
]

patches-own [ 
  class             ;; used to label initial clusters and to store final class
  in-perc-cluster?  ;; true if patch is in initial percolation cluster
  
  nearby            ;; patches nearby for gathering activity
  
  low-k             ;; capacity and 
  low-value-resource  ;; current level of low value resource
  high-k            ;; capacity and  
  high-value-resource ;; current level of high value resource
  
  mh-d
  on-island?
  edge-shell        ;; used by irregular island code
]


to setup
  clear-all

  set potted-history []
  
  ;; landscape initialisation
  with-local-randomness [
    if isl-seed? [ random-seed isl-seed ]
    make-the-island
    initialise-patch-variables
    setup-mrc the-island ;; in simmap_code.nls
    initialise-resources
  ]
    
  if use-seed? [ random-seed rng-seed ]
  
  ;; initialise the founder group
  initialise-groups
  
  ;; for analysis
  set max-acquired 0
  set max-pop 0
  set max-groups 0
  set prop-acquired 0
  set mean-acquired 0
  
  set total-resource-start sum [high-value-resource] of patches
  set t50 -1
  
  update-display
  reset-ticks
end


;; housekeeping really
to initialise-patch-variables
  ask patches [
    set class -1
    set in-perc-cluster? false
    ifelse on-island? [
      set nearby (patch-set self (patches in-radius nearby-range) with [on-island?])
      set mh-d 1000
    ]
    [
      set mh-d 10000
    ]
  ]
end


to make-the-island
  ask patches [ set on-island? true ]
  ask patches with [pxcor = max-pxcor or pxcor = min-pxcor or pycor = max-pycor or pycor = min-pycor] [
    set on-island? false
  ]
  set the-sea patches with [not on-island?]
  set the-island patches with [on-island?]
  set the-shore the-island with [any? neighbors4 with [not on-island?]]
  
  ask the-sea [
    set pcolor cyan
  ]
  
  ask the-shore [
    ask neighbors4 with [not on-island?] [
      set pcolor yellow
    ]
  ]
end


to initialise-groups
;  ask one-of the-shore with [any? neighbors with [high-k > 0] and high-k = 0] [
;  let mean-low-k mean [low-k] of the-island
  ask one-of the-shore with [any? patches in-radius settlement-buffer-radius with [high-k > 0] and high-k = 0] [
    sprout-groups 1 [
      set color red
      set shape "person"
      set size 3
      set pop random-poisson (max-group-size / 2)
      set home-camp patch-here
      set my-hunting-spots turtle-set nobody
      set search-tortuosity initial-search-tortuosity
    ]
  ]
end


to go
  if count groups = 0 [
    tick ;; this lets the go-year go-decade buttons work
    print potted-history
    set prop-acquired 1 - ((sum [high-value-resource] of patches) / total-resource-start)  
    set mean-acquired mean-acquired / ticks 
    stop
  ]
  
  clear-drawing ;; clears the paths so we can see what's happening
  if ticks > 0 [ ;; none of this required in month 0
    if ticks > 11 [ ;; don't move in the first year
      assess-situation-and-relocate
    ]
    ;; reproduction and group-splitting is annual
    if ticks mod 12 = 0 [ ;; human reproduction is annual
      reproduce
      split
      regrow-resources
    ]
  ]
  go-hunting
  gather-local-resources
  update-display
  update-evaluation-variables

  tick
end


to update-evaluation-variables
  ;; update evaluation variables
  let high-res-avail (sum [high-value-resource] of patches)
  let sum-collected sum [total-collected] of groups
  
  set prop-acquired 1 - (high-res-avail / total-resource-start)   
  set mean-acquired sum-collected + mean-acquired
  
  if max-acquired < sum-collected
    [ set max-acquired sum-collected]
  
  if (high-res-avail <= total-resource-start * 0.5) and t50 = -1
  [
    set t50 ticks
  ]  
  if ticks mod 12 = 0 [ 
    if max-groups < count groups
    [ set max-groups count groups ]
    if max-pop < sum [pop] of groups [ set max-pop sum [pop] of groups ] 
  ]
end  

to update-display
  ask the-island [
    ifelse high-k = 0 or high-value-resource = 0
    [ set pcolor scale-color green low-value-resource (max-low-k * 2) (0 - max-low-k) ]
    [ set pcolor scale-color black high-value-resource (max-high-k * 2) (0 - max-high-k) ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
225
10
622
428
64
64
3.0
1
3
1
1
1
0
0
0
1
-64
64
-64
64
1
1
1
month
100.0

SWITCH
20
155
115
188
isl-seed?
isl-seed?
1
1
-1000

SLIDER
122
155
214
188
isl-seed
isl-seed
0
1000
275
1
1
NIL
HORIZONTAL

SLIDER
25
305
198
338
percolation-threshold
percolation-threshold
0.4
0.6
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
31
11
94
44
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
645
227
817
260
r-high
r-high
0
0.5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
644
375
816
408
r-low
r-low
0
1
0.2
0.01
1
NIL
HORIZONTAL

BUTTON
108
10
171
43
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
30
50
93
83
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

SWITCH
24
385
194
418
no-singleton-patches?
no-singleton-patches?
0
1
-1000

BUTTON
106
49
185
82
go-year
go \nwhile [(ticks + 1) mod 12 != 0] [go]
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
644
93
816
126
r-humans
r-humans
0
0.05
0.013
0.001
1
NIL
HORIZONTAL

MONITOR
1080
437
1159
482
NIL
count groups
0
1
11

TEXTBOX
630
10
780
29
Parameters
14
0.0
1

TEXTBOX
636
17
1201
35
_________________________________________________________________________________________
12
0.0
1

SLIDER
644
55
816
88
max-group-size
max-group-size
20
50
30
1
1
NIL
HORIZONTAL

TEXTBOX
646
35
796
53
Human demography
12
0.0
1

TEXTBOX
644
207
794
225
Resource demography
12
0.0
1

TEXTBOX
30
135
180
153
Landscape initialisation
12
0.0
1

SLIDER
24
341
198
374
proportion-high-resource
proportion-high-resource
0
1
0.15
0.01
1
NIL
HORIZONTAL

TEXTBOX
823
35
973
53
Resource exploitation
12
0.0
1

SLIDER
821
93
995
126
hunt-kill-per-head
hunt-kill-per-head
1
10
5
0.1
1
NIL
HORIZONTAL

SLIDER
644
297
816
330
max-high-K
max-high-K
0.1
10
3
0.1
1
NIL
HORIZONTAL

SLIDER
643
447
816
480
max-low-K
max-low-K
0.01
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
821
167
993
200
hunt-party-size
hunt-party-size
5
20
6
1
1
NIL
HORIZONTAL

SLIDER
821
204
993
237
hunt-range
hunt-range
10
60
16
1
1
NIL
HORIZONTAL

SLIDER
820
367
992
400
nearby-range
nearby-range
1
10
2.3
0.1
1
NIL
HORIZONTAL

TEXTBOX
822
401
994
457
Applied on initialisation - how far from camp gathering is done.
10
15.0
1

SLIDER
819
432
991
465
diffusion-rate
diffusion-rate
0
0.5
0.1
0.01
1
NIL
HORIZONTAL

TEXTBOX
823
463
1012
481
Low value resource diffuses
10
15.0
1

TEXTBOX
29
286
179
304
SIMMAP parameters
12
0.0
1

SLIDER
822
56
994
89
resource-per-head
resource-per-head
0.1
2
1
0.1
1
NIL
HORIZONTAL

TEXTBOX
1006
34
1156
52
Search behaviour
12
0.0
1

SLIDER
1002
53
1173
86
initial-search-tortuosity
initial-search-tortuosity
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
1002
90
1174
123
search-adjust
search-adjust
0
0.1
0.05
0.001
1
NIL
HORIZONTAL

SLIDER
1002
126
1174
159
max-tortuosity
max-tortuosity
0.5
1
0.95
0.01
1
NIL
HORIZONTAL

SLIDER
1002
164
1174
197
min-tortuosity
min-tortuosity
0
0.5
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
644
262
816
295
r-hi-sd
r-hi-sd
0
0.5
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
643
411
816
444
r-lo-sd
r-lo-sd
0
0.5
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
820
277
991
310
hunt-memory-length
hunt-memory-length
1
50
15
1
1
NIL
HORIZONTAL

SLIDER
820
240
991
273
max-hunts-per-month
max-hunts-per-month
1
10
4
1
1
NIL
HORIZONTAL

TEXTBOX
825
310
885
328
for a person
9
15.0
1

BUTTON
104
88
183
121
go-decade
go \nwhile [(ticks + 1) mod 120 != 0] [go]
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
999
211
1173
244
relocate-near-hunting?
relocate-near-hunting?
0
1
-1000

PLOT
1000
250
1207
434
pop and res-collection
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Pop" 1.0 0 -4539718 true "" "plot sum [pop] of groups"
"Take" 1.0 0 -16777216 true "" "if ticks mod 12 = 0 [\n  plotxy ticks sum [hunt-take-to-date] of groups\n]"
"Kill" 1.0 0 -2674135 true "" "if ticks mod 12 = 0 [\nplotxy ticks sum [hunt-kill-to-date] of groups]"
"Local" 1.0 0 -12087248 true "" "if ticks mod 12 = 0 [ \nplotxy ticks sum [local-collected] of groups ]"

SLIDER
643
167
815
200
max-birth-rate-multiple
max-birth-rate-multiple
1
5
3
0.1
1
NIL
HORIZONTAL

SLIDER
644
130
816
163
r-humans-sd
r-humans-sd
0
1
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
1001
437
1072
482
population
sum [pop] of groups
0
1
11

CHOOSER
37
434
175
479
island-type
island-type
"square" "circle" "irregular"
2

SLIDER
821
130
994
163
hunt-take-per-head
hunt-take-per-head
0.01
0.5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
820
331
992
364
gather-per-head
gather-per-head
0
0.1
0.05
0.001
1
NIL
HORIZONTAL

SLIDER
645
333
817
366
min-sustainable-h
min-sustainable-h
0
0.1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
406
433
578
466
min-viable-human-pop
min-viable-human-pop
0
10
5
1
1
NIL
HORIZONTAL

SWITCH
17
210
122
243
use-seed?
use-seed?
0
1
-1000

SLIDER
125
210
217
243
rng-seed
rng-seed
0
1000
50
1
1
NIL
HORIZONTAL

TEXTBOX
25
195
175
213
Initial settlement location
12
0.0
1

SLIDER
15
245
222
278
settlement-buffer-radius
settlement-buffer-radius
1
10
2.3
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is described in detail in Chapter 8 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

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
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="resource-sweep" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>max-pop</metric>
    <metric>max-groups</metric>
    <metric>max-acquired</metric>
    <metric>prop-acquired</metric>
    <metric>t50</metric>
    <steppedValueSet variable="rel-hunting-efficiency" first="1" step="1" last="10"/>
    <steppedValueSet variable="r-high" first="0.05" step="0.025" last="0.15"/>
    <steppedValueSet variable="max-high-K" first="10" step="10" last="100"/>
  </experiment>
  <experiment name="base" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>t50</metric>
    <enumeratedValueSet variable="island-type">
      <value value="&quot;irregular&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
1
@#$#@#$#@
