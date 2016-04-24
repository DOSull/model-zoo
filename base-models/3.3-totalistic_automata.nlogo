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


extensions [r]

globals [
  ; list encoding of the rule in format
  ; [[0 0 0 1 0 0 0 0 0] [0 0 1 1 0 0 0 0 0]]
  ; first list is the outcome if cell state is 0 for each possible count of neighbours in state 1
  ; second list is the outcomes if cell state is 1
  ; above list would be game of life rule
  ; this means the next state of a patch with n live neighbors is given by
  ; item n (item state rule)
  ; which allows for a very general implementation
  rule
  rule-desc ; a text table representation of the rule for presenting in the output box
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
  r:setPlotDevice

  setup-rule
  show-rule-details

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
    set nbhd patches-at n-offsets
  ]
end

to-report patches-at [offsets]
  report patch-set map [patch-at (item 0 ?) (item 1 ?)] offsets
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
  ;; the possible x- and y-ranges are from -range to +range
  let x-range n-values (2 * (floor range) + 1) [? - floor range]
  let y-range n-values (2 * (floor range) + 1) [? - floor range]
  ;; make the full array of possible offsets within range in either x or y directions
  let array []
  foreach x-range [
    let x-o ?
    foreach y-range [
      set array lput (list x-o ?) array
    ]
  ]
  ;; remove the [0 0] offset entry
  set array remove (list 0 0) array
  ;; now, if nbhd is not orth+diag we have to remove some entries
  if neighbourhood-style != "orth+diag" [
    ifelse neighbourhood-style = "orthogonal"
    ;; in this case use Manhattan distance, i.e. dx + dy
    [ set array filter [manhattan-distance item 0 ? item 1 ? <= range] array ] ;; [((abs item 0 ?) + (abs item 1 ?)) <= range] array ]
    ;; otherwise use Euclidean distance
    [ set array filter [euclidean-distance item 0 ? item 1 ? <= range] array ] ;; [(sqrt((item 0 ?) ^ 2 + (item 1 ?) ^ 2)) <= range] array ]
  ]
  report array
end

;; Manhattan distance
to-report manhattan-distance [diff-x diff-y]
  report (abs diff-x) + (abs diff-y)
end

;; Euclidean distance
to-report euclidean-distance [diff-x diff-y]
  report sqrt (diff-x ^ 2 + diff-y ^ 2)
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
    ask patches [
      ifelse pxcor > -5 and pxcor < 5 and pycor > -5 and pycor < 5
      [ set state 1 ]
      [ set state 0 ]
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
  ;; if not any? patches with [changed?] [stop]
  update-patch-states
  update-patch-display
  tick
end

to update-patch-states
  ;; note that we determine all next-states first
  ask patches [
    ; count how many live neighbours of the patch
    let n sum [state] of nbhd
    ;; now we take advantage of the rule format (see comment on the global variables)
    set next-state item n (item state rule)
    set changed? (state != next-state)
  ]
  ;; then update all patches
  ask patches [
    set state next-state
  ]
end

;; ============================================
;; Rule setup procedures
;; ============================================

;; setup from sliders or from the Wolfram code
to setup-rule
  ifelse setup-from-ranges?
  [ setup-rule-from-sliders ]
  [ setup-rule-from-code ]
end

;; make the required two 0/1 lists
;; and combine into the rule list as described in the globals comments
to setup-rule-from-sliders
  let birth-on-sums get-on-sums birth-min birth-max birth-invert?
  let survival-on-sums get-on-sums surv-min surv-max survival-invert?
  set rule (list birth-on-sums survival-on-sums)
end

;; this reports a list of output states based on a min and max sum
;; and whether or not to invert the result
to-report get-on-sums [min-sum max-sum invert?]
  let zero-to-n n-values (n-size + 1) [?]
  let on-sums map [? >= min-sum and ? <= max-sum] zero-to-n
  if invert? [ set on-sums map [not ?] on-sums ]
  report map [boolean-as-int ?] on-sums
end

to-report boolean-as-int [b]
  report ifelse-value b [1] [0]
end

;; an explanation of Wolfram codes is found in the INFO tab
;; this code will make more sense if you read that explanation
to setup-rule-from-code
  let binary-list get-binary-list Wolfram-code
  ;; pack with 0s if needed
  let reqd-len (n-size + 1) * 2
  ifelse (length binary-list) < reqd-len [
    let cur-len length binary-list
    let packing n-values (reqd-len - cur-len) [0]
    set binary-list sentence binary-list packing
  ]
  [ ;; or trim to length
    if length binary-list > reqd-len [
      set binary-list sublist binary-list 0 reqd-len
    ]
  ]
  ;; convert evens and odds to the necessary lists for the rule
  let even-indices filter [? mod 2 = 0] n-values reqd-len [?]
  let odd-indices filter [? mod 2 = 1] n-values reqd-len [?]
  let births map [item ? binary-list] even-indices
  let survivals map [item ? binary-list] odd-indices
  set rule (list births survivals)
end

;; converts number x to a list of binary bits
;; e.g. 10 -> [0 1 0 1]
;; uses repeated division by 2 putting remainder in the list
to-report get-binary-list [x]
  let result []
  let d x
  while [d > 0] [
    set result lput (d mod 2) result
    set d floor (d / 2)
  ]
  report result
end

;; show the rule in the output window and listings
to show-rule-details
  set rule-desc "      State\nN-sum  0  1\n___________\n"
  foreach n-values (length (item 0 rule)) [?] [
    let on-sum pack-string ? 2
    set rule-desc (word rule-desc " " on-sum
                             "    " (item ? (item 0 rule))
                             "  " (item ? (item 1 rule))
                             "\n")
  ]
  set list-0 reduce [word ?1 ?2] (sentence "" item 0 rule)
  set list-1 reduce [word ?1 ?2] (sentence "" item 1 rule)
  set-Wolfram-code-from-lists
end

to-report pack-string [s len]
  ifelse length word "" s < len
  [ report word " " s ]
  [ report s ]
end

;; set the Wolfram code from the list inputs
to set-Wolfram-code-from-lists
  let list-pair (list force-list-length list-0 force-list-length list-1)
  let wc 0
  foreach [0 1] [
    let d0 ?
    let lst item d0 list-pair
    foreach n-values (n-size + 1) [?] [
      if (item ? lst) = "1" [
        set wc wc + 2 ^ (2 * ? + d0)
      ]
    ]
  ]
  set Wolfram-code wc
end

to-report force-list-length [L]
  if length L > n-size + 1 [
    report substring L 0 (n-size + 2)
  ]
  if length L < n-size + 1 [
    while [length L < n-size + 1] [
      set L word L "0"
    ]
    report L
  ]
  report L
end


to r-plot-world
  r:put "s" map [[state] of ?] sort patches
  r:put "nr" world-height
  r:put "nc" world-width
  r:eval("map <- matrix(s, ncol=nc, nrow=nr)")
  r:eval("image(map, asp=1, col=c('white','black'), axes=F)")
end
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
0.21
0.01
1
NIL
HORIZONTAL

BUTTON
24
401
120
434
NIL
r-plot-world
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
3
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
3
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
1

SLIDER
552
82
644
115
range
range
1
5
1
0.1
1
NIL
HORIZONTAL

MONITOR
704
33
761
78
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
3
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
;set neighbourhood-style \"orth+diag\"\n;set range 1\nset birth-invert? false\nset survival-invert? false\nset birth-min round (0.3125 * n-size)\nset birth-max round (0.4374 * n-size)\nset surv-min round (0.1875 * n-size)\nset surv-max round (0.4374 * n-size)\nset setup-from-ranges? true\nsetup
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
set birth-invert? false\nset survival-invert? false\nset birth-min floor (n-size / 2) + 1\nset birth-max n-size\nset surv-min floor (n-size / 2) \nset surv-max n-size\nset setup-from-ranges? true\nsetup
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
224
1
0
Number

TEXTBOX
768
175
814
257
Rule setup using ranges
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

OUTPUT
845
34
980
327
11

TEXTBOX
859
10
967
28
Rule description
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
652
82
778
120
You can't control n-size: it's what results from the neighbourhood settings
10
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
49
1
1
NIL
HORIZONTAL

BUTTON
672
420
834
453
random-Wolfram-code
set Wolfram-code (2 * (random (2 ^ ((n-size * 2) + 1))))\nset setup-from-ranges? false\nsetup
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
842
334
980
394
list-0
000100000
1
0
String

INPUTBOX
843
401
980
461
list-1
001100000
1
0
String

BUTTON
672
381
836
414
<-- set-Wolfram-code <--
set-Wolfram-code-from-lists\nset setup-from-ranges? false\nsetup\n\n
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
567
293
752
326
setup-from-ranges?
setup-from-ranges?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model allows you to simulate a wide variety of two-state _outer totalistic automata_, that is cellular automata whose rules refer only to the current state of each cell and to the number of neighbouring cells in each of the two available states.

These are discussed in Chapter 3 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

You should consult that book for more information and details of what to expect from the model.

## HOW TO USE IT

It is necessary to set up the local neighbourhood appropriately, which is done with the `range` and `neighbourhood-style` controls.  These should be self-explanatory.  The neighbourhood of the central patch is shown in red after initial setup, if you are uncertain.

Sliders allow you to configure the maximum and minimum number of 'live' (i.e. black) neighbours for survival of a live cell or birth of a dead cell.  Switching on the `birth-invert?` or `survival-invert?` switches will invert the sense of the min-to-max range.  So, for example the game of life is setup by setting `birth-invert?` and `survival-invert?` off, `birth-min` = `birth-max` = 3, `survival-min` = 2, and `survival-max` = 3.

Together the neighboourhood and birth / survival controls allow exploration of the _Larger than Life_ (LtL) class of automata, see

+   Evans KM 2003 Larger than Life: threshold-range scaling of Life’s coherent structures. _Physica D: Nonlinear Phenomena_ **183**, 45–67

Other totalistic automata will require use of the wolfram code setup.

**Wolfram codes**
Another way to setup the system is by entering a _Wolfram code_ in the appropriate input box with the `setup-from-ranges?` switch off.  The (accurate but rather concise) definition of Wolfram codes is presented in

+ Wolfram S 1984 Universality and Complexity in Cellular Automata. _Physica D: Nonlinear Phenomena_ __10__, 1–35

More details are provided below.  When a model is setup using the min-max ranges method its Wolfram code will be output, a detailed transition table will be shown, and `list-0` and `list-1` will show lists of the next state for each input state.  By comparing the transition table and list outputs, you should be able to see how these are related.  You can also edit the list input boxes to make minor changes to a rule, and then hit the `<-- set-Wolfram-code <--` button to convert it to the appropriate code and set the model up that way.  This is tricky for larger neighbourhood sizes: the model will force the lists to be the appropriate length for the current neighbourhood size, by removing any extra entries, or packing the list with trailing 0s. If you want particular results for large neightbourhood sizes, you should probably write the lists in another program and paste them into the list input boxes.  You can also setup random rules with the `random-rule` button.

Wolfram codes are decimal versions of binary numbers that encode the desired rule as follows:

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

## THINGS TO NOTICE

This is a complicated model, with extensive use of lists, and `map` and `filter` operations in the code, although we managed to steer clear of `reduce`!  The commenting is extensive, so should provide all the information you need.

The key thing to realize is how rules have been encoded as a list of two lists, the first for the next state outcomes when the current cell state is 0, the second for the next state outcomes when the current state is 1.  The next state outcome lists are ordered by the number of neighbouring cells that are in state 1.  So, for a majority rule CA, with 4 neighbors, the two lists are `[0 0 0 1 1]` and `[0 0 1 1 1]` and the `rule` global variable is

    (list [0 0 0 1 1] [0 0 1 1 1])

This makes finding the next state a simple matter of retrieving the appropriate item from the appropriate list:

    set next-state item (sum [state] of nbhd) (item state rule)

The first `item` reporter uses the number of neighbouring live cells as its index value, while the second uses the current state of the cell (0 or 1).  Doing things this way makes it possible to simulate a wide range of CA in a single model.

## THINGS TO TRY

It's reasonably diverting to set a geometry (i.e. a neighbourhood definition) up, then set things running (`go`) and to keep clicking `random-rule` to search (not very efficiently) for interesting examples.  This will give you some idea of why people get excited about the Game of Life: its behaviour is really rather unusual.

While doing this, it is also interesting to experiment with the model speed slider.  Sometimes, behaviour that is interesting at one speed appears much less so at a different speed (and vice-versa) generally due to periodicities in the model.

For any given CA it is also worth experimenting with the density of the original configurations to see under what conditions interesting outcomes occur (try this for the Game of Life or simple majority rule automata, for example).

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
