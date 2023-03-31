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


extensions [palette]

globals [
  rewards ;; a list of the returns to CC CD DC DD from
          ;; perspective of the first player i.e.
          ;; item 0 is return when I cooperate (C) You cooperate (C)
          ;; item 1 is return to C v D
          ;; item 2 is return to D v C
          ;; item 3 is return to D v D
]

;; each game is a 'link' between neighboring pairs of players
;; this is convenient for remembering previous outcome
undirected-link-breed [games game]
games-own [
  player-1
  player-2
  previous-plays
]

;; possible mutations are 'links' from one strategy-turtle to another
undirected-link-breed [mutations mutation]

;; for naming purposes we make a breed of 'player' turtles
;; who will take part in the games
breed [players player]
players-own [
  my-strategy ;; a strategy turtle containing this players choices
  this-round-payoff ;; what I scored this round
  recent-payoffs ;; total scores in recent rounds
  N5 ;; neighboring orthogonal players plus the player itself
]

;; store strategies as turtles (invisible turtles)
;; this is a convenient way to bundle colors etc, rather than
;; maintaining multiple linked lists of colors, etc
breed [strategies strategy]
strategies-own [
  choices ;; a list of the next choice to CC CD DC DD
           ;; encoded as
           ;; 1 = defect
           ;; 0 = cooperate
]


to setup
  clear-all

  ;; make strategies and set up reward matrix
  make-strategies
  set rewards (list C-C C-D D-C D-D)

  ;; create players
  ask patches [
    sprout-players 1 [
      set shape "big-square"
      ifelse cage-fight!? [
        set my-strategy ifelse-value (pxcor <= min-pxcor + world-width / 2) [strategy strat-1] [strategy strat-2]
      ]
      [ ;; just pick a strategy at random
        set my-strategy one-of strategies
      ]
      set this-round-payoff 0
      set recent-payoffs []
    ]
  ]
  ;; create pairwise games between neighbors
  ask players [
    set N5 (turtle-set self (players-on neighbors4)) ;; since no one ever moves
    ask other N5 [
      create-game-with myself [
        ; establish order of the game based on player whos
        set player-1 first sort both-ends
        set player-2 last sort both-ends
        initialize-game-previous-plays
        set hidden? true
      ]
    ]
  ]
  update-display
  reset-ticks
end


;; sets up 16 invisible strategy turtles as containers for
;; lists of choices, which also store color, text-color,
;; and (in the 'who') an index number
to make-strategies
  let strategy-palette (sentence palette:scheme-colors "Qualitative" "Set1" 9
                                 palette:scheme-colors "Qualitative" "Pastel1" 7)
  ;; sort from darkest to lightest
  set strategy-palette sort-by [ [c1 c2] -> sum c1 < sum c2 ] strategy-palette
  ;; text colors are black or white depending in brightness of the base color
  let text-palette map [ c -> ifelse-value (mean c < 160) [white] [black] ] strategy-palette

  ;; now make 16 strategies
  create-strategies 16 [
    set choices strategy-from-number who
    set color item who strategy-palette
    set label-color item who text-palette
    set hidden? true
  ]
  ask strategies [
    ;; mutations are possible between strategies that differ in one 'bit'
    ask other strategies with [bits-difference [choices] of myself choices = 1] [
      create-mutation-with myself [
        set hidden? true
      ]
    ]
  ]
end

;; converts number to 4 bit list 0 1 in binary code, e.g.
;; 0 -> [0 0 0 0]
;; 5 -> [0 1 0 1]
;; 12 -> [1 1 0 0] etc
to-report strategy-from-number [x]
  let s []
  repeat 4 [
    set s fput (x mod 2) s
    set x floor (x / 2)
  ]
  report s
end

;; report the number of bits different between two bit lists
to-report bits-difference [c1 c2]
  report sum (map [ [b1 b2] -> abs (b1 - b2) ] c1 c2)
end


;; color and label turtles based on their current strategy
to update-display
  ask players [
    ifelse random 10 < 3 and any? (other N5 with [ not (my-strategy = [my-strategy] of myself) ] ) [
      set label [who] of my-strategy
      set label-color [label-color] of my-strategy
    ]
    [ set label "" ]
    ;; color is that of the player's strategy
    set color [color] of my-strategy
  ]
end


to initialize-game-previous-plays
  set previous-plays (list (one-of [choices] of [my-strategy] of player-1)
                           (one-of [choices] of [my-strategy] of player-2))
end


;; main loop
to go
  ;; every game gets played every round
  ask games [ play-game ]
  ;; update payoffs and strategies based on outcomes
  ask players [
    update-payoffs
    update-strategy
  ]
  update-display
  tick
end


;; play game between this game's players basing moves on previous round plays
to play-game
  let prev-mv1 first previous-plays
  let prev-mv2 last previous-plays
  ;; based on previous plays pick this round plays
  let mv1 [get-play prev-mv1 prev-mv2] of player-1
  let mv2 [get-play prev-mv2 prev-mv1] of player-2
  ;; determine payoffs and add to current payoffs
  ask player-1 [ set this-round-payoff this-round-payoff + get-payoff mv1 mv2 ]
  ask player-2 [ set this-round-payoff this-round-payoff + get-payoff mv2 mv1 ]
  ;; report this rounds plays so previous play can be updated
  set previous-plays (list mv1 mv2)
end

to-report get-play [prev1 prev2]
  report [item (two-bits-as-decimal prev1 prev2) choices] of my-strategy
end

to-report get-payoff [m1 m2]
  report item (two-bits-as-decimal m1 m2) rewards
end

;; 0 0 -> 0
;; 0 1 -> 1
;; 1 0 -> 2
;; 1 1 -> 3
to-report two-bits-as-decimal [b1 b0]
  report 2 * b1 + b0
end


;; add this round payoff to record of previous payoffs
;; adjust list size to limit, if required
to update-payoffs
  set recent-payoffs fput this-round-payoff recent-payoffs
  if length recent-payoffs > payoff-memory [
    set recent-payoffs but-last recent-payoffs
  ]
  set this-round-payoff 0
end


;; update strategy to that of the locally best strategy
;; or, with probability p-mutate, mutate the strategy
to update-strategy
  ifelse random-float 1 < p-mutate
  [ mutate ]
  [ set my-strategy [my-strategy] of max-one-of N5 [sum recent-payoffs] ]
end

;; change to a linked strategy (i.e. one that is a single bit different
to mutate
  set my-strategy one-of [link-neighbors] of my-strategy
end
@#$#@#$#@
GRAPHICS-WINDOW
304
10
952
659
-1
-1
8.0
1
8
1
1
1
0
1
1
1
0
79
0
79
1
1
1
ticks
100.0

BUTTON
224
20
287
53
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
224
58
287
91
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
224
96
287
129
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
113
598
285
631
p-mutate
p-mutate
0
0.05
0.0
0.001
1
NIL
HORIZONTAL

TEXTBOX
13
244
300
548
     me-them previous round\n#     C-C C-D D-C D-D\n0        C     C     C     C   always-C\n1        C     C     C     D\n2        C     C     D     C\n3        C     C     D     D   stubborn\n4        C     D     C     C\n5        C     D     C     D   tit-for-tat\n6        C     D     D     C   win-stay-lose-shift\n7        C     D     D     D   retaliator\n8        D     C     C     C\n9        D     C     C     D\n10      D     C     D     C   bully\n11      D     C     D     D\n12      D     D     C     C   fickle\n13      D     D     C     D\n14      D     D     D     C\n15      D     D     D     D  always-D
13
0.0
1

INPUTBOX
1037
32
1087
92
C-C
4.0
1
0
Number

INPUTBOX
1091
31
1141
91
C-D
0.0
1
0
Number

INPUTBOX
1037
95
1087
155
D-C
5.0
1
0
Number

INPUTBOX
1092
97
1142
157
D-D
2.0
1
0
Number

SLIDER
114
559
286
592
payoff-memory
payoff-memory
1
5
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
1019
10
1178
28
Pay off matrix, me-them
11
0.0
1

TEXTBOX
985
39
1038
57
(R)eward
11
0.0
1

TEXTBOX
1147
38
1198
57
(S)ucker
11
0.0
1

TEXTBOX
961
101
1033
120
(T)emptation
11
0.0
1

TEXTBOX
1146
101
1223
119
(P)unishment
11
0.0
1

TEXTBOX
971
185
1121
227
For prisoner's dilemma\nT > R > P > S and\n2R > S + T
11
0.0
1

BUTTON
998
233
1080
266
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
971
274
1121
302
For chicken games\nT > R > S > P
11
0.0
1

BUTTON
997
305
1105
338
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

BUTTON
965
371
1106
404
setup-hawk-dove
set D-C 2\nset C-C 1\nset C-D 0\nset D-D 0
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
113
141
146
cage-fight!?
cage-fight!?
1
1
-1000

SLIDER
117
157
289
190
strat-1
strat-1
0
15
15.0
1
1
NIL
HORIZONTAL

SLIDER
117
194
289
227
strat-2
strat-2
0
15
2.0
1
1
NIL
HORIZONTAL

BUTTON
982
631
1133
664
color-cooperativity
ask players [\n  set color palette:scale-scheme \"Divergent\" \"PRGn\" 5 sum [choices] of my-strategy 4 0\n  ]
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

This is an implementation of a generic spatial evolutionary game 'arena' in which players enact the strategies detailed in the table presented in the interface, and switch their strategies to match those of the most successful neighboring players in the system.  Success is defined by the total payoff achieved over the most recent `playoff-memory` rounds of play.

See Chapter 3 of

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.

for a brief discussion.

***NOTE*** that this model is substantially different from the one released at first publication. In particular, here players compete with all four neighbors every round, and it is the one-to-one games which 'remember' the previous play. The original model is available at the [github repository] (http://github.com/DOSull/model-zoo/blob/master/as-released/base-models/3.16-IPD.nlogo).

## THINGS TO NOTICE

This is a complicated model, which makes use of some NetLogo features in unusual ways.

### Turtle breeds

Two turtle breeds are used in the model. `players` are the turtles that engage in the game in question, and are straightforward.

`strategies` are more interesting. These are invisible or 'virtual' turtles, which are used as a convenient way to store (in the `choices` variable) the choice that will be made for a particular strategy based on the two plays made in the previous game by each participant. Each `player` turtle has a `my-strategy` variable, which is an instance of one of the `strategy` turtles. This makes managing the color and labeling of `players` easier, since each `strategy` has a fixed color, enabling lines of code such as

    set color [color] of my-strategy
    set label [who] of my-strategy
    set label-color [label-color] of my-strategy

The alternative to this approach involves maintaining global lists of strategies, and associated colors, which is arguably less elegant. `strategy` turtles make use of `mutation` links in an interesting way to manage random mutation of strategies in a simple way (see below for more details).

### Links

Two different `link` breeds `games` and `mutations` are used in the model.

First, `games` are linked pairs of neighboring turtles that will repeatedly engage in the game. This is convenient as a way to store the previous round of the game outcome in the `previous-plays` variable. This is important because the previous plays are used by each player to decide their next play based on their current strategy.

Second, `mutations` are links between `strategy` turtles which can mutate into one another. Mutation is considered a change in a strategy where only one of the move choices is changed, so for example, the strategy `[0 0 1 0]` can change to any of `[1 0 1 0]`, `[0 1 1 0]`, `[0 0 0 0]` or `[0 0 1 1]` but not to any other strategies.  This is implemented in the code in the `make-strategies` procedure

    ask other strategies with [choice-diff [choices] of myself choices = 1] [
      create-mutation-with myself [
        set hidden? true
      ]
    ]

and in the line

    set my-strategy one-of [link-neighbors] of my-strategy

which allows the `my-strategy` to transition only to those other strategies linked with the current one (and which therefore differ from it in only one choice).

## THINGS TO TRY

This is a very open-ended model, which can be used to explore many different scenarios. As it stands, any simple 'cooperate-defect' game-theoretic situation with a fixed payoff matrix can easily be experimented with. It is worth spending time exploring what combinations of strategies in any given game can coexist. In particular using the `cage-fight!?` option you can set up 'fights' between pairs of strategies to develop a feel for which strategies prosper against one another. In many cases, you will find the results surprising: try to predict which strategy will win these contests.

It may also be worth investigating how differently things play out with different initial conditions, such as ones where particular strategies are missing from the original mix, or where blocks of players with particular strategies exist at the outset. These changes will require that you understand the existing code and modify it accordingly.

## HOW TO CITE

If you mention this model in a publication, please include these citations for the model itself and for NetLogo

+   O'Sullivan D and Perry GLW 2013 _Spatial Simulation: Exploring Pattern and Process_. Wiley, Chichester, England.
+   Wilensky U 1999 NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

The MIT License (MIT)

Copyright &copy; 2011-2018 David O'Sullivan and George Perry

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

big-square
false
0
Rectangle -7500403 true true 0 0 300 300

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
NetLogo 6.2.0
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
