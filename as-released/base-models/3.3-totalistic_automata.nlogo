; Copyright (c) 2011-24 David O'Sullivan and George Perry
; Licensed under the Creative Commons 
; Attribution-NonCommercial-ShareAlike 3.0 License 
; See Info tab for full copyright and license information
;;

;; extensions [r]

globals [
  ; list encoding of the rule in format
  ; [[0 0 0 1 0 0 0 0 0] [0 0 1 1 0 0 0 0 0]]
  ; first list is the outcome if cell state is 0 for each possible count of neighbours in state 1
  ; second list is the outcomes if cell state is 1
  ; above list would be game of life rule
  rule       
  rule-id
  rule-desc
]

patches-own [
  state      ; integer 0 or 1
  next-state ; the next state of the patch
  nbhd       ; the neighbourhood used in the CA - not necessarily the same as the netlogo builtins
  changed?   
]

; setup model so that a proportion of the patches
; equal to the density are initially alive
to setup
  clear-all
  ;; r:setPlotDevice
  
  setup-rule
  set-rule-id
  set-rule-desc
  
  output-print rule-desc
  setup-patch-neighbourhoods
  
  initialise-states
  update-patch-display
  ask patch 0 0 [
    if state = 0 [ set pcolor red ]
    ask nbhd [
      if state = 0 [ set pcolor red ]
    ]
  ]
  reset-ticks
end

;; =============================================
;; Patch neighbourhoods code
;; =============================================
to setup-patch-neighbourhoods 
  let n-offsets n-offset-coords
  ask patches [
    set nbhd patch-set map [patch-at (item 0 ?) (item 1 ?)] n-offsets
  ]
end

;; reports a list of tuples of [Dx Dy] for the neighbourhood range and type
;; e.g. range = 2, orthogonal would give a list containing
;;                 [0 2]
;;         [-1 1]  [0 1]  [1 1]
;; [-2 0]  [-1 0]         [1 0]  [2 0]
;;         [-1 -1] [0 -1] [1 -1] 
;;                 [0 -2]
;; note that [0 0] is not included
;; note that there are cleaner ways of doing this using distance reporters
;; and patches with [distance-reporter] but they will are slower because they
;; have to calculate those distances repeatedly - because we know the offsets
;; we want, we only calculate once this way
to-report n-offset-coords
  let x-range n-values (2 * (floor range) + 1) [? - floor range]
  let y-range n-values (2 * (floor range) + 1) [? - floor range]
  let full-array []
  foreach x-range [
    let x-o ?
    foreach y-range [
      set full-array lput (list x-o ?) full-array
    ]
  ]
  set full-array remove (list 0 0) full-array
  if neighbourhood-style != "orth+diag" [
    ifelse neighbourhood-style = "orthogonal" 
    [ set full-array filter [((abs item 0 ?) + (abs item 1 ?)) <= range] full-array ]
    [ set full-array filter [(sqrt((item 0 ?) ^ 2 + (item 1 ?) ^ 2)) <= range] full-array ]
  ]
  report full-array
end

to-report n-size
  report length n-offset-coords
end


;;
to initialise-states
  if use-seed? [random-seed seed-value]
  ;; simple random setup
  if init-method = "random" [
    ask patches [
      ifelse (random-float 1 < density)
      [ set state 1 ]
      [ set state 0 ]
    ]
  ]
  ;; single 'live' cell at centre
  if init-method = "single site at centre" [
    ask patches [ set state 0 ]
    ask patch 0 0 [ set state 1 ]
  ]
  ;; 9 x 9 region in the centre
  if init-method = "small central region" [
    ask patches [ set state 0 ]
    ask patches with [pxcor > -5 and pxcor < 5 and pycor > -5 and pycor < 5] [
      if random-float 1 < density [
        set state 1
      ]
    ] 
  ]
  ask patches [
    set changed? true
  ]
end

to update-patch-display 
  ask patches [
    ifelse state = 1
    [ set pcolor black ]
    [ set pcolor white ]
  ]
end


to go
  if not any? patches with [changed?] [stop]
  update-patch-states
  update-patch-display
  tick
end


to update-patch-states
  ask patches [
    ; count how many live neighbours of the patch
    let n sum [state] of nbhd
    set next-state item n (item state rule)
    set changed? (state != next-state)
  ]
  ask patches [
    set state next-state
  ]
end


;; ============================================
;; Rule setup procedures
;; ============================================

;; setup from sliders or from the Wolfram code
to setup-rule
  ifelse setup-using-sliders?
  [ setup-rule-from-sliders ]
  [ setup-rule-from-id ]
end
  
;; from sliders is easier to follow
to setup-rule-from-sliders
  set rule []
  let on-sums []
  ;; birth cases
  ifelse birth-invert?
  [ set on-sums map [? < birth-min or ? > birth-max] (n-values (n-size + 1) [?]) ]
  [ set on-sums map [? >= birth-min and ? <= birth-max] (n-values (n-size + 1) [?]) ]
  set rule lput (map [boolean-as-int ?] on-sums) rule
  ;; surv cases
  ifelse survival-invert?
  [ set on-sums map [? < surv-min or ? > surv-max] (n-values (n-size + 1) [?]) ]
  [ set on-sums map [? >= surv-min and ? <= surv-max] (n-values (n-size + 1) [?]) ]
  set rule lput (map [boolean-as-int ?] on-sums) rule
end

to setup-rule-from-id 
  let binary-list get-binary-list Wolfram-code
  ;; pack with 0s if needed
  let reqd-len (n-size + 1) * 2
  ifelse (length binary-list) < reqd-len [
    let cur-len length binary-list
    repeat (reqd-len - cur-len) [
      set binary-list lput 0 binary-list
    ]
  ]
  [ ;; or trim to length
    if length binary-list > reqd-len [
      set binary-list sublist binary-list 0 reqd-len
    ]
  ]
  ;; convert evens and odds to the necessary lists for the rule
  let births map [item ? binary-list] (filter [? mod 2 = 0] n-values reqd-len [?])
  let survivals map [item ? binary-list] (filter [? mod 2 = 1] n-values reqd-len [?])
  set rule (list births survivals)
end

;; calculate the Wolfram code of the rule if it hasn't been supplied
to set-rule-id
  set rule-id 0
  foreach [0 1] [
    let d0 ?
    foreach n-values (n-size + 1) [?] [
      if (item ? (item d0 rule)) = 1 [
         set rule-id rule-id + 2 ^ (2 * ? + d0)
      ]
    ]
  ]
end

to-report boolean-as-int [b]
  report ifelse-value b [1] [0]
end

to-report non-zero-as-int [x]
  report ifelse-value (x > 0) [1] [0]
end

;; converts number x to a list of binary bits
;; e.g. 10 -> [0 1 0 1]
to-report get-binary-list [x]
  let result []
  let d x
  while [d > 0] [
    set result lput (d mod 2) result
    set d floor (d / 2) 
  ]
  report result
end

;; show the rule in the output window
to set-rule-desc
  set rule-desc "\tCell state\nN-sum\t0\t1\n___________________\n"
  foreach n-values (length (item 0 rule)) [?] [
    set rule-desc (word rule-desc ? 
                             "\t" (item ? (item 0 rule))
                             "\t" (item ? (item 1 rule))
                             "\n") 
  ]
end

;; R plotting code
;;to r-plot-world
;;  r:put "s" map [[state] of ?] sort patches
;;  r:put "nr" world-height
;;  r:put "nc" world-width
;;  r:eval("map <- matrix(s, ncol=nc, nrow=nr)")
;;  r:eval("image(map, asp=1, col=c('white','black'), axes=F)")
;;end
@#$#@#$#@
GRAPHICS-WINDOW
134
10
548
445
50
50
4.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
60.0

BUTTON
61
23
124
56
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
60
101
123
134
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

BUTTON
60
141
123
174
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
10
264
123
297
density
density
0
1
0.5
0.01
1
NIL
HORIZONTAL

SWITCH
10
306
122
339
use-seed?
use-seed?
1
1
-1000

SWITCH
551
134
659
167
birth-invert?
birth-invert?
1
1
-1000

SLIDER
565
171
658
204
birth-min
birth-min
0
n-size
4
1
1
NIL
HORIZONTAL

SLIDER
565
207
658
240
birth-max
birth-max
birth-min
n-size
5
1
1
NIL
HORIZONTAL

CHOOSER
552
32
696
77
neighbourhood-style
neighbourhood-style
"orthogonal" "orth+diag" "Euclidean distance"
2

SLIDER
552
82
644
115
range
range
1
5
2
0.1
1
NIL
HORIZONTAL

MONITOR
704
32
761
77
NIL
n-size
0
1
11

SWITCH
661
133
789
166
survival-invert?
survival-invert?
1
1
-1000

SLIDER
662
171
760
204
surv-min
surv-min
0
n-size
2
1
1
NIL
HORIZONTAL

SLIDER
662
207
760
240
surv-max
surv-max
surv-min
n-size
5
1
1
NIL
HORIZONTAL

BUTTON
565
250
658
283
setup-life-like
;set neighbourhood-style \"orth+diag\"\n;set range 1\nset birth-invert? false\nset survival-invert? false\nset birth-min round (0.3125 * n-size)\nset birth-max round (0.4374 * n-size)\nset surv-min round (0.1875 * n-size)\nset surv-max round (0.4374 * n-size)\nset setup-using-sliders? true\nsetup
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
664
251
758
284
setup-majority
set birth-invert? false\nset survival-invert? false\nset birth-min floor (n-size / 2) + 1\nset birth-max n-size\nset surv-min floor (n-size / 2) \nset surv-max n-size\nset setup-using-sliders? true\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
552
364
666
424
Wolfram-code
4.7223661075689604E21
1
0
Number

SWITCH
566
302
754
335
setup-using-sliders?
setup-using-sliders?
0
1
-1000

TEXTBOX
768
188
860
222
Rule setup using sliders
13
0.0
1

TEXTBOX
557
345
786
364
Rule setup using Wolfram code number
13
0.0
1

MONITOR
679
369
840
422
NIL
rule-id
19
1
13

TEXTBOX
679
426
838
468
This will differ from the code if  neighbourhood not large enough to accommodate code
10
0.0
1

OUTPUT
852
36
1005
455
10

TEXTBOX
853
12
1003
30
Transition rule lookup
13
0.0
1

CHOOSER
17
198
124
243
init-method
init-method
"random" "single site at centre" "small central region"
0

TEXTBOX
559
10
709
28
Specify neighbourhood
13
0.0
1

TEXTBOX
768
10
846
100
You can't control n-size: it's what results from the neighbourhood settings
11
0.0
1

TEXTBOX
555
427
678
473
These IDs become useless for large neighbourhood sizes
10
0.0
1

BUTTON
60
61
123
94
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

SLIDER
10
347
120
380
seed-value
seed-value
0
100
50
1
1
NIL
HORIZONTAL

BUTTON
759
304
839
337
random-rule
set Wolfram-code (2 * (random (2 ^ ((n-size * 2) + 1))))\nsetup
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

This model allows you to simulate a wide variety of two-state _outer totalistic automata_, that is cellular automata whose rules refer only to the current state of each cell and to the number of neighbouring cells in each of the two available states.

These are discussed in Chapter 3 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of the model.

## HOW TO USE IT

It is necessary to set up the local neighbourhood appropriately, which is done with the `range` and `neighbourhood-style` controls.  These should be self-explanatory.  The neighbourhood of the central patch is shown in red after initial setup, if you are uncertain.

Sliders allow you to configure the maximum and minimum number of 'live' (i.e. black) neighbours for survival of a live cell or birth of a dead cell.  Switching on the `birth-invert?` or `survival-invert?` switches will invert the sense of the min-to-max range.  So, for example the game of life is setup by setting `birth-invert?` and `survival-invert?` off, `birth-min` = `birth-max` = 3, `survival-min` = 2, and `survival-max` = 3.  

Together the neighboourhood and birth / survival controls allow exploration of the _Larger than Life_ (LtL) class of automata, see

+   Evans KM 2003 Larger than Life: threshold-range scaling of Life’s coherent structures. _Physica D: Nonlinear Phenomena_ **183**, 45–67

Other totalistic automata will require use of the Wolfram code setup.

**Wolfram codes**
One way to set up the system is by entering a _Wolfram code_ in the appropriate input box with the `setup-using-sliders?` switch off.  These codes are decimal versions of binary numbers that encode the desired rule as follows:

<table align=center>
<tr><td></td><td colspan=10><i>Total of Nbhd states</i></td></tr>
<tr><td align=center><i>State</i></td><td>&nbsp;0&nbsp;</td><td>&nbsp;1&nbsp;</td><td>&nbsp;2&nbsp;</td><td>&nbsp;3&nbsp;</td><td>&nbsp;4&nbsp;</td><td>&nbsp;5&nbsp;</td><td>&nbsp;6&nbsp;</td><td>&nbsp;7&nbsp;</td><td>&nbsp;8&nbsp;</td><td>...</td>
</tr>
<tr><td>0</td><td>b<sub>0</sub></td><td>b<sub>2</sub></td><td>b<sub>4</sub></td><td>b<sub>6</sub></td><td>...</td>
</tr>
<tr><td>1</td><td>b<sub>1</sub></td><td>b<sub>3</sub></td><td>b<sub>5</sub></td><td>b<sub>7</sub></td><td>...</td>
</tr>
</table>

This gives a binary number b<sub>2n+1</sub>b<sub>2n</sub>...b<sub>3</sub>b<sub>2</sub>b<sub>1</sub>b<sub>0</sub>. Conversion to a decimal number yields the Wolfram code.

For example, Conway's life using this scheme is

<table align=center>
<tr><td></td><td colspan=9><i>Total of Nbhd states</i></td></tr>
<tr><td align=center><i>State</i></td><td>&nbsp;0&nbsp;</td><td>&nbsp;1&nbsp;</td><td>&nbsp;2&nbsp;</td><td>&nbsp;3&nbsp;</td><td>&nbsp;4&nbsp;</td><td>&nbsp;5&nbsp;</td><td>&nbsp;6&nbsp;</td><td>&nbsp;7&nbsp;</td><td>&nbsp;8&nbsp;</td>
</tr>
<tr><td>0</td><td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;1</td><td>&nbsp;0<td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;0</td>
</tr>
<tr><td>1</td><td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;1</td><td>&nbsp;1</td><td>&nbsp;0<td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;0</td><td>&nbsp;0</td>
</tr>
</table>

Giving the binary number 00 0000 0000 1110 0000, which is decimal 224.

Some useful Wolfram codes (neighbourhood configurations must also be set appropriately) are set out below.

**Conway's Life** (orthogonal + diagonal 8 neighbours)
    
    Normal                                224
    Inverse                            254975

**Majority rules**

    orth+diag 8                        261632 (261120 small but signficant difference)
    orthogonal 4                          992 (960 ditto)
    orth+diag 24             1125899873288192
    orthogonal 12                    67100672
    Distance d=2.3              4398044413952

**Vichniac twisted majority** (annealing)

    orth+diag 8                        260480
    orthogonal 4                          920

**Life-like**

    higher-life (N8)                     4320
    LtL {1,1,6,2,7}                     16380
    LtL {4,22,31,24,38} 4.7223661075689604E21

**Parity rules** (odd-even)

    orth+diag 8                        157286
    orthogonal 4                          614                

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo  

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 2011-24 David O'Sullivan and George L. W. Perry

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
<experiments>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count patches with [state = 1] / 10201</metric>
    <enumeratedValueSet variable="neighbourhood-style">
      <value value="&quot;orth+diag&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-method">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="density" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="range">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolfram-code">
      <value value="1125899873288192"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="setup-using-sliders?">
      <value value="false"/>
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
0
@#$#@#$#@
