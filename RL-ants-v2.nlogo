extensions[qlearningextension]

globals [
  episode-end
  last-episode-ticks
  episode-ticks
  current-episode
  previous-episodes
]

patches-own [
  chemical             ;; amount of chemical on this patch
  food                 ;; amount of food on this patch (0, 1, or 2)
  nest?                ;; true on nest patches, false elsewhere
  local-nest-scent     ;; number that is higher closer to the nest
  food-source-number   ;; number (1, 2, or 3) to identify the food sources
]

turtles-own [
  isFoodPatch          ;; needed by qlearningextension:state-def that can only report turtle variables
  hasNotFood           ;; needed by qlearningextension:state-def that can only report turtle variables
  lastAction           ;; needed for reward function SM: DOES IT MAKE SENSE FROM RL STANDPOINT??
  reward-list          ;; for plots
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-ticks
  clear-turtles
  clear-patches
  clear-drawing
  clear-output
  set-current-plot "Food in each pile"
  clear-plot
  set-current-plot "Ants status"
  clear-plot
  set-default-shape turtles "bug"

  create-turtles population
  [ set isFoodPatch false
    set hasNotFood true
    set size 2
    set color red  ]                                           ;; red = not carrying food

  set-current-plot "Ave Reward Per Episode"
  set-plot-y-range -10 10

  ask turtles [
    qlearningextension:state-def["isFoodPatch" "hasNotFood"]   ;; SM: ARE "xcor" "ycor"  NEEDED??
    (qlearningextension:actions [pick-food] [dont-pick-food])  ;; SM: WHAT ABOUT OTHER (not learnt) ACTIONS??
    qlearningextension:reward [rewardFunc]
    qlearningextension:end-episode [isEndState] resetEpisode
    qlearningextension:action-selection "e-greedy" [0.5 0.95]
    qlearningextension:learning-rate 0.95
    qlearningextension:discount-factor 0.75
    ; used to create the plot
    set-current-plot "Ave Reward Per Episode"
    create-temporary-plot-pen (word who)
    set-plot-pen-color color
    set reward-list []
  ]

  setup-patches
  reset-ticks
end

to setup-patches
  let food-size max-food-size
  if (rng-food-size)
  [ set food-size random max-food-size + 1 ]
  ask patches
  [ setup-nest
    setup-food food-size
    recolor-patch ]
end

to setup-nest  ;; patch procedure
  ;; set nest? variable to true inside the nest, false elsewhere
  set nest? (distancexy 0 0) < nest-size  ;; SM Reports the distance from this agent to the point (x, y). 0,0 is center, hence we build nest as circle with 5 radius
  ;; spread a local-nest-scent over the whole world -- stronger near the nest
  set local-nest-scent nest-scent - distancexy 0 0  ;; SM remember that each patch is asked to run this code
end

to setup-food [food-size]  ;; patch procedure
  ;; setup food source one on the right
  if (distancexy (0.6 * max-pxcor) 0) < food-size  ;; SM "p" stands for patches, hence max x coord admissible for patches
  [ set food-source-number 1 ]
  ;; setup food source two on the lower-left
  if (distancexy (-0.6 * max-pxcor) (-0.6 * max-pycor)) < food-size * 1.25
  [ set food-source-number 2 ]
  ;; setup food source three on the upper-left
  if (distancexy (-0.8 * max-pxcor) (0.8 * max-pycor)) < food-size * 1.5
  [ set food-source-number 3 ]
  if food-source-number > 0
  [ set food one-of [1] ]  ;; SM draws random from list or agentset, removed 2 to lower randomness
end

to recolor-patch  ;; patch procedure
  ifelse nest?
  [ set pcolor violet ]  ;; SM true path
  [ ifelse food > 0  ;; SM false path
    [ if food-source-number = 1 [ set pcolor cyan ]
      if food-source-number = 2 [ set pcolor sky  ]
      if food-source-number = 3 [ set pcolor blue ] ]
    ;; scale color to show chemical concentration
    [ set pcolor scale-color yellow chemical 0.1 5 ] ]  ;; SM scale <color> by <number> within <range1>,<range2>. When <range1> <= <range2>, then the larger <number>, the lighter the shade.
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEARNING procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to learn-for
  if (current-episode = episodes)
  [ set episode-end 0
    ;set previous-episodes previous-episodes + episodes
    stop ]  ;; SM This agent exits immediately from the enclosing procedure, ask, or ask-like construct
  learn
  if (episode-end = 1)
  [ set current-episode current-episode + 1
    ;set last-episode-ticks ticks
    ;set episode-ticks lput last-episode-ticks episode-ticks
    set episode-end 0
    setup-learning ]
end

to learn  ;; same as 'go' but doesn't stop when food depleted and all ants red
  ask turtles
  [ if who >= ticks [ stop ] ;; delay initial departure SM <who> is turtle ID starting at 0, <ticks> is simulation step. Basically each turtle starts sequentially based on its ID
    ifelse food > 0
      [ set isFoodPatch true ]
      [ set isFoodPatch false ]
    qlearningextension:learning
    print(qlearningextension:get-qtable)
    ifelse color = red  ;; SM red ants have no food, green ants have food
    [
      ;pick-food              ;; SM: I THINK THIS DOES NOT BELONG HERE, AS THE ANT SHOULD LEARN ITSELF TO DO THIS
      ifelse (chemical >= chemical-threshold)
        [ follow-pheromone ]
        [ random-walk ]
    ]
    [
      drop-food
      drop-pheromone
      follow-nest
    ]
    do-move
    ]
  diffuse chemical (diffusion-rate / 100)  ;; SM tells each patch to share <patch-variable> by (<number> * 100)% to its 8 neighboring patches. <number> \in [0, 1].
  ask patches
  [ set chemical chemical * (100 - evaporation-rate) / 100  ;; slowly evaporate chemical
    recolor-patch ]
  tick
end

to-report rewardFunc
  let reward -1
  if (pcolor = cyan or pcolor = sky or pcolor = blue) and not hasNotFood and lastAction = "pick-food"
    [ set reward 10 ]
  set reward-list lput reward reward-list
  report reward
end

to-report isEndState
  if count patches with [pcolor = cyan or pcolor = sky or pcolor = blue] <= 0 [
    report true
  ]
  report false
end

to resetEpisode
  ; used to update the plot
  let rew-sum 0
  let length-rew 0
  foreach reward-list [ r ->
    set rew-sum rew-sum + r
    set length-rew length-rew + 1
  ]
  let avg-rew rew-sum / length-rew

  set-current-plot "Ave Reward Per Episode"
  set-current-plot-pen (word who)
  plot avg-rew

  set reward-list []
end

to setup-learning
  clear-ticks
  clear-patches
  clear-drawing
  clear-output
  set-current-plot "Food in each pile"
  clear-plot
  set-current-plot "Ants status"
  clear-plot

  setup-patches
  reset-ticks
end

;;;;;;;;;;;;;;;;;;
;;; RL actions ;;;
;;;;;;;;;;;;;;;;;;

to drop-food
  if nest?
  [ ;; drop food and head out again
    set color red
    set hasNotFood true
    rt 180 ]  ;; SM alias for <right>, to turn of X degrees
end

to drop-pheromone
  if not nest?
  [ set chemical chemical + chemical-droplet ]  ;; drop some chemical SM remember that turtles can access variables of patch they are in
end

to follow-nest
  if not nest?
  [ uphill-nest-scent ]  ;; head toward the greatest value of local-nest-scent
end

to pick-food
  if food > 0 [
    set color green  ;; pick up food
    set hasNotFood false
    set food food - 1  ;; and reduce the food source
    rt 180  ;; and turn around SM: IS THIS TOO MUCH FOR LEARNING? (learning to head back to nest is easier with this)
  ]
end

to dont-pick-food

end

;; go in the direction where the chemical smell is strongest
to follow-pheromone
  if (chemical >= chemical-threshold)
  [ uphill-chemical-v2 ]
end

to random-walk  ;; turtle procedure
  rt random 45
  lt random 45
end

;;;;;;;;;;;;;;;;;;;;;
;;; Go procedures ;;;
;;;;;;;;;;;;;;;;;;;;;

to go  ;; forever button
  if (all? patches [food = 0] and all? turtles [color = red])
  [ set episode-end 1
    stop ]
  ask turtles
  [ if who >= ticks [ stop ] ;; delay initial departure SM <who> is turtle ID starting at 0, <ticks> is simulation step. Basically each turtle starts sequentially based on its ID
    ifelse food > 0
      [ set isFoodPatch true ]
      [ set isFoodPatch false ]
    ifelse color = red  ;; SM red ants have no food, green ants have food
    [
      pick-food
      ifelse (chemical >= chemical-threshold)
        [ follow-pheromone ]
        [ random-walk ]
    ]
    [
      drop-food
      drop-pheromone
      follow-nest
    ]
    do-move
    ]
  diffuse chemical (diffusion-rate / 100)  ;; SM tells each patch to share <patch-variable> by (<number> * 100)% to its 8 neighboring patches. <number> \in [0, 1].
  ask patches
  [ set chemical chemical * (100 - evaporation-rate) / 100  ;; slowly evaporate chemical
    recolor-patch ]
  tick
end

to go-for
  if (current-episode = iters)
  [ set episode-end 0
    set previous-episodes previous-episodes + iters
    stop ]  ;; SM This agent exits immediately from the enclosing procedure, ask, or ask-like construct
  go
  if (episode-end = 1)
  [ set current-episode current-episode + 1
    set last-episode-ticks ticks
    set episode-ticks lput last-episode-ticks episode-ticks
    set episode-end 0
    setup ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to do-move
  if not can-move? 1 [ rt 180 ]  ;; SM Reports true if this turtle can move distance in the direction it is facing without violating the topology (wrapping is set via interface settings)
  fd 1  ;; SM alias for <forward> (move ahead 1 step)
end

;; SM improved uphill-chemical
to uphill-chemical-v2  ;; turtle procedure
  let scent-ahead chemical-scent-at-angle   0
  let scent-right chemical-scent-at-angle  45
  let scent-left  chemical-scent-at-angle -45
  let scent-ahead-n nest-scent-at-angle   0
  let scent-right-n nest-scent-at-angle  45
  let scent-left-n  nest-scent-at-angle -45
  ifelse (scent-right > scent-ahead) or (scent-left > scent-ahead)
  [ ifelse scent-right > scent-left
    [ ifelse scent-right-n < local-nest-scent
      [ rt 45 ]
      [ lt 45 ] ]
    [ ifelse scent-left-n < local-nest-scent
      [ lt 45 ]
      [ rt 45 ] ] ]
  [ if scent-ahead-n > local-nest-scent
    [ ifelse scent-right > scent-left
      [ lt 90 ]
      [ rt 90 ] ] ]
end

;; sniff left and right, and go where the strongest smell is
to uphill-nest-scent  ;; turtle procedure
  let scent-ahead nest-scent-at-angle   0
  let scent-right nest-scent-at-angle  45
  let scent-left  nest-scent-at-angle -45
  if (scent-right > scent-ahead) or (scent-left > scent-ahead)
  [ ifelse scent-right > scent-left
    [ rt 45 ]
    [ lt 45 ] ]
end

to-report nest-scent-at-angle [angle]
  let p patch-right-and-ahead angle 1  ;; SM reports the patch at right <angle> and <distance> from this turtle
  if p = nobody [ report 0 ]  ;; SM Immediately exits from the current to-report procedure and reports value
  report [local-nest-scent] of p
end

to-report chemical-scent-at-angle [angle]
  let p patch-right-and-ahead angle 1
  if p = nobody [ report 0 ]
  report [chemical] of p
end


; Copyright 2022 Stefano Mariani.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
257
10
762
516
-1
-1
7.0
1
10
1
1
1
0
0
0
1
-35
35
-35
35
1
1
1
ticks
30.0

BUTTON
46
71
126
104
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
31
106
221
139
diffusion-rate
diffusion-rate
0.0
99.0
25.0
1.0
1
NIL
HORIZONTAL

SLIDER
31
141
221
174
evaporation-rate
evaporation-rate
0.0
99.0
25.0
1.0
1
NIL
HORIZONTAL

BUTTON
136
71
211
104
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
31
36
221
69
population
population
0.0
200.0
1.0
1.0
1
NIL
HORIZONTAL

PLOT
5
197
248
404
Food in each pile
time
food
0.0
50.0
0.0
120.0
true
false
"" ""
PENS
"food-in-pile1" 1.0 0 -11221820 true "" "plotxy ticks sum [food] of patches with [pcolor = cyan]"
"food-in-pile2" 1.0 0 -13791810 true "" "plotxy ticks sum [food] of patches with [pcolor = sky]"
"food-in-pile3" 1.0 0 -13345367 true "" "plotxy ticks sum [food] of patches with [pcolor = blue]"

PLOT
775
12
1144
278
Ants status
time
count
0.0
0.0
0.0
100.0
true
true
"" ""
PENS
"ants-in-trail-%" 1.0 0 -16777216 true "" "plot ( ((count turtles with [chemical > 0.05]) * 100) / (count turtles) )"
"ants-in-trail" 1.0 0 -4079321 true "" "plot count turtles with [chemical > 0.05]"
"ants-with-food" 1.0 0 -10899396 true "" "plot count turtles with [color = green]"

SLIDER
776
287
925
320
max-food-size
max-food-size
1
8
5.0
1
1
NIL
HORIZONTAL

SLIDER
776
327
926
360
nest-size
nest-size
1
9
5.0
1
1
NIL
HORIZONTAL

SLIDER
984
370
1159
403
nest-scent
nest-scent
100
500
250.0
10
1
NIL
HORIZONTAL

SLIDER
984
410
1156
443
chemical-droplet
chemical-droplet
10
100
50.0
10
1
NIL
HORIZONTAL

SLIDER
985
450
1159
483
chemical-threshold
chemical-threshold
0.1
10
5.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
1165
368
1387
554
These parameters are likely to be highly correlated:\n - high chemical-threshold makes ants less able to recognise trails (hence, to follow-them)\n - high chemical-droplet makes easier to form trails (as more pheromone is left)\n - high nest-scent makes easier to return to nest
12
0.0
1

BUTTON
7
429
80
462
go-for
go-for
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
88
415
155
475
iters
2.0
1
0
Number

MONITOR
8
487
134
532
NIL
last-episode-ticks
0
1
11

SWITCH
931
287
1077
320
rng-food-size
rng-food-size
0
1
-1000

MONITOR
9
541
701
586
NIL
episode-ticks
0
1
11

BUTTON
711
547
861
580
clear-globals
set last-episode-ticks -1\nset episode-ticks []\nset episode-end 0\nset current-episode 0\nset previous-episodes 0\nset-current-plot \"Episodes ticks\"\nclear-plot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
777
367
977
517
Episodes ticks
# episode
# ticks
0.0
10.0
0.0
100.0
true
false
"plotxy previous-episodes + current-episode last-episode-ticks" ""
PENS
"default" 1.0 1 -16777216 true "" ""

BUTTON
869
547
961
580
keep-plot
set episode-end 0\nset current-episode 0
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
140
487
248
532
NIL
previous-episodes
17
1
11

PLOT
1152
13
1425
277
Ave Reward Per Episode
episode
ave reward
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

BUTTON
1153
286
1241
319
NIL
learn-for
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1249
287
1316
347
episodes
2
1
0
String

@#$#@#$#@
## WHAT IS IT?

In this project, a colony of ants forages for food. Though each ant follows a set of simple rules, the colony as a whole acts in a sophisticated way.

## HOW IT WORKS

When an ant finds a piece of food, it carries the food back to the nest, dropping a chemical as it moves. When other ants "sniff" the chemical, they follow the chemical toward the food. As more ants carry food to the nest, they reinforce the chemical trail.

## HOW TO USE IT

Click the SETUP button to set up the ant nest (in violet, at center) and three piles of food. Click the GO button to start the simulation. The chemical is shown in a green-to-white gradient.

The EVAPORATION-RATE slider controls the evaporation rate of the chemical. The DIFFUSION-RATE slider controls the diffusion rate of the chemical.

If you want to change the number of ants, move the POPULATION slider before pressing SETUP.

## THINGS TO NOTICE

The ant colony generally exploits the food source in order, starting with the food closest to the nest, and finishing with the food most distant from the nest. It is more difficult for the ants to form a stable trail to the more distant food, since the chemical trail has more time to evaporate and diffuse before being reinforced.

Once the colony finishes collecting the closest food, the chemical trail to that food naturally disappears, freeing up ants to help collect the other food sources. The more distant food sources require a larger "critical number" of ants to form a stable trail.

The consumption of the food is shown in a plot.  The line colors in the plot match the colors of the food piles.

## EXTENDING THE MODEL

Try different placements for the food sources. What happens if two food sources are equidistant from the nest? When that happens in the real world, ant colonies typically exploit one source then the other (not at the same time).

In this project, the ants use a "trick" to find their way back to the nest: they follow the "nest scent." Real ants use a variety of different approaches to find their way back to the nest. Try to implement some alternative strategies.

The ants only respond to chemical levels between 0.05 and 2.  The lower limit is used so the ants aren't infinitely sensitive.  Try removing the upper limit.  What happens?  Why?

In the `uphill-chemical` procedure, the ant "follows the gradient" of the chemical. That is, it "sniffs" in three directions, then turns in the direction where the chemical is strongest. You might want to try variants of the `uphill-chemical` procedure, changing the number and placement of "ant sniffs."

## NETLOGO FEATURES

The built-in `diffuse` primitive lets us diffuse the chemical easily without complicated code.

The primitive `patch-right-and-ahead` is used to make the ants smell in different directions without actually turning.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1997).  NetLogo Ants model.  http://ccl.northwestern.edu/netlogo/models/Ants.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1997 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was developed at the MIT Media Lab using CM StarLogo.  See Resnick, M. (1994) "Turtles, Termites and Traffic Jams: Explorations in Massively Parallel Microworlds."  Cambridge, MA: MIT Press.  Adapted to StarLogoT, 1997, as part of the Connected Mathematics Project.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 1998.

<!-- 1997 1998 MIT -->
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
NetLogo 6.2.1
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
