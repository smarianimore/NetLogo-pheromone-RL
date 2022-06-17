;; CHECK ESPECIALLY CAREFULLY COMMENTS WITH "NB" OR "WARNING"
;; 1) Explictly modify experiment name in procedure setup-learning
;; 2) Configure RL stuff within "ask Learners [..." in porcedure setup-learning
;; 3) Explicitly modify lines "e-greedy", "ACION SPACE", "OBSERVATION SPACE", and "REWARD" in procedure log-params at the very end of file

extensions[qlearningextension]

globals [
  filename              ;; the file where to report simulation results (automatically appended with a timestamp)
  g-reward-list         ;; list with one entry for each turtle, that is the average reward got so far by such turtle
  episode               ;; progressive number of the currently running episode (hence number of episodes run)
  is-there-cluster]     ;; is there at least one cluser in the whole environment? (boolean)

patches-own [chemical]  ;; amount of pheromone in the patch

Breed[Learners Learner] ;; turtles that are learning (shown in red)

Learners-own [
  chemical-here         ;; whether there is pheromone on the patch-here (boolean)
  p-chemical            ;; amount of pheromone on the patch-here
  reward-list           ;; list of rewards got so far
]

turtles-own [           ;; NB these variables are also inherited by learners
  ticks-in-cluster      ;; how many ticks the turtle has stayed within a cluster
  cluster               ;; number of turtles within cluster-radius
  in-cluster            ;; whether the turtle is within a cluster (boolean = cluster > cluster-threshold)
]

;;;;;;;;;;;;;;;;;;;;;;
;; SETUP procedures ;;
;;;;;;;;;;;;;;;;;;;;;;

to setup                           ;; NO RL here (some RL variables are initialised anyway to avoid errors)
  clear-all

  create-turtles population
  [ set color blue
    set size 2
    setxy random-xcor random-ycor
    set ticks-in-cluster 0
    set cluster 0
    set in-cluster false
    if label?
      [ set label who ] ]

  ask patches [ set chemical 0 ]
  set is-there-cluster false

  reset-ticks
  setup-global-plot "Average cluster size in # of turtles within cluster-radius" "# of turtles" 0
end

to setup-learning                  ;; RL
  setup
  set filename (word "experiment_8-" date-and-time ".txt")  ;; NB CHANGE NAME OF EXPERIMENT HERE
  print filename
  file-open filename
  log-params
  set g-reward-list []
  set episode 0

  create-Learners learning-turtles
  [ set color red
    set size 2
    setxy random-xcor random-ycor
    set chemical-here false
    set p-chemical 0
    set reward-list []
    if label?
      [ set label who ] ]

  ask Learners [
    ;qlearningextension:state-def ["p-chemical" "cluster"] reporter                    ;; reporter could report variables that the agent does not own
    qlearningextension:state-def ["chemical-here" "in-cluster"]                        ;; WARNING non-boolean state variables make the Q-table explode in size, hence Netlogo crashes 'cause out of memory!
    ;(qlearningextension:actions [move-toward-chemical] [random-walk] [drop-chemical]) ;; admissible actions to be learned in policy WARNING: be sure to not use explicitly these actions in learners!
    (qlearningextension:actions [move-toward-chemical] [random-walk] [drop-chemical] [move-and-drop] [walk-and-drop])
    qlearningextension:reward [rewardFunc8]                                            ;; the reward function used
    qlearningextension:end-episode [isEndState] resetEpisode                           ;; the termination condition for an episode and the procedure to call to reset the environment for the next episode
    qlearningextension:action-selection "e-greedy" [0.50 0.9]                          ;; NB 1st param is chance of random action, 2nd parameter is decay factor applied (after each episode the 1st parameter is updated, the new value corresponding to the current value multiplied by the 2nd param)
    qlearningextension:learning-rate learning-rate
    qlearningextension:discount-factor discount-factor
  ]

  setup-global-plot "Average reward per episode" "average reward" 0
end

;;;;;;;;;;;;;;;;;;;
;; GO procedures ;;
;;;;;;;;;;;;;;;;;;;

to go                                              ;; NO RL
  ask turtles
  [ check-cluster
    ;plot-individual
    ifelse chemical > sniff-threshold              ;; ignore pheromone unless there's enough here
      [ move-toward-chemical ]
      [ random-walk ]
    drop-chemical ]                                ;; drop chemical onto patch

  diffuse chemical diffuse-share                   ;; diffuse chemical to neighboring patches
  ask patches
  [ set chemical chemical * evaporation-rate       ;; evaporate chemical
    set pcolor scale-color green chemical 0.1 3 ]  ;; update display of chemical concentration

  let c-avg avg-cluster?
  plot-global "Average cluster size in # of turtles within cluster-radius" "# of turtles" c-avg
  log-ticks "average cluster size in # of turtles: " c-avg

  tick
end

to learn                                       ;; RL
  if episode < episodes                        ;; = learning episodes not finished
  [ ask turtles
    [ if not (breed = Learners)                ;; handle non learning slimes as for 'go' procedure
        [ check-cluster
        ifelse chemical > sniff-threshold
          [ move-toward-chemical ]
          [ random-walk ]
        drop-chemical ]
    ]

    ask Learners                               ;; handle learning slimes
    [ check-cluster
      set p-chemical [chemical] of patch-here
      ifelse chemical > sniff-threshold
      [ set chemical-here true ]               ;; set state variables
        ;move-toward-chemical ]
      [ set chemical-here false ]
        ;random-walk ]
      qlearningextension:learning              ;; NB select an action for the current state, perform the action, get the reward, update the Q-table, verify if the new state is an end state and if so will run the procedure passed to the extension in the end-episode primitive
    ]

    diffuse chemical diffuse-share
    ask patches
    [ set chemical chemical * evaporation-rate
      set pcolor scale-color green chemical 0.1 3 ]

    let c-avg avg-cluster?
    plot-global "Average cluster size in # of turtles within cluster-radius" "# of turtles" c-avg
    log-ticks "average cluster size in # of turtles: " c-avg

    let g-avg-rew 0

    if (ticks > 0) and ((ticks mod ticks-per-episode) = 0) [               ;; NB an episode has just ended
    ;if (ticks > 1) and (is-there-cluster = true) [
      clear-patches                                                        ;; clear chemical
      set is-there-cluster false                                           ;; reset state variables
      set g-avg-rew avg? g-reward-list
      plot-global "Average reward per episode" "average reward" g-avg-rew
      log-episodes "average reward per episode: " g-avg-rew
      set g-reward-list []
      set episode episode + 1

      ask turtles [                                                        ;; reset non learners too
        if not (breed = Learners)
          [
            setxy random-xcor random-ycor
            set ticks-in-cluster 0
            set cluster 0
            set in-cluster false
          ]
      ]
    ]

    if (ticks > 0) and ((ticks mod print-every) = 0)                       ;; log experiment data
      [
        file-open filename
        ;;        Episode,                         Tick,                          Avg custer size X tick,         Avg reward X episode, TBD: Actions distribution (how many turtles choose each available action)
        file-type episode file-type ", " file-type ticks file-type ", " file-type c-avg file-type ", " file-print g-avg-rew
      ]

    tick
  ]
  file-close-all
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEARNING procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to-report rewardFunc1  ;; fixed reward if in cluster, otherwise penalty
  let r penalty
  if in-cluster = true
    [ set r reward ]
  set reward-list lput r reward-list
  report r
end

to-report rewardFunc2  ;; monotonic reward based on ticks-in-cluster
  set reward-list lput ticks-in-cluster reward-list
  report ticks-in-cluster
end

to-report rewardFunc3  ;; reward and penalty based on ticks-in-cluster
  let rew 0
  if (ticks > 0)
    [ set rew
        ((ticks-in-cluster / ticks-per-episode) * reward)
        +
        (((ticks-per-episode - ticks-in-cluster) / ticks-per-episode) * penalty)
      set reward-list lput rew reward-list ]
  report rew
end

to-report rewardFunc4  ;; reward based on both ticks-in-cluster and cluster size, penalty based on ticks-in-cluster
  let rew cluster
  if (ticks > 0)
    [ set rew
        ((ticks-in-cluster / ticks-per-episode) * (cluster / cluster-threshold) * reward)
        +
        (((ticks-per-episode - ticks-in-cluster) / ticks-per-episode) * penalty)
      set reward-list lput rew reward-list ]
  report rew
end

to-report rewardFunc5  ;; additive variation of rewardFunc4
  let rew cluster
  if (ticks > 0)
    [ set rew
        ((ticks-in-cluster / ticks-per-episode) * reward)
        +
        ((cluster / cluster-threshold) * reward)
        +
        (((ticks-per-episode - ticks-in-cluster) / ticks-per-episode) * penalty)
      set reward-list lput rew reward-list ]
  report rew
end

to-report rewardFunc6  ;; variation of rewardFunc5: more 'weight' to cluster size, less 'weight' to penalty
  let rew cluster
  if (ticks > 0)
    [ set rew
        ((ticks-in-cluster / ticks-per-episode) * reward)
        +
        ((cluster / cluster-threshold) * reward ^ 2)
        +
        ((ticks-per-episode - ticks-in-cluster) * (penalty / 10))
      set reward-list lput rew reward-list ]
  report rew
end

to-report rewardFunc7  ;; no ticks-in-cluster
  let rew cluster
  if (ticks > 0)
    [ set rew
        ((cluster ^ 2 / cluster-threshold) * reward)
        +
        (((ticks-per-episode - ticks-in-cluster) / ticks-per-episode) * penalty)
      set reward-list lput rew reward-list ]
  report rew
end

to-report rewardFunc8  ;; variation of rewardFunc6: ratio of ticks not in cluster, instead of absolute difference
  let rew cluster
  if (ticks > 0)
    [ set rew
        ((ticks-in-cluster / ticks-per-episode) * reward)
        +
        ((cluster / cluster-threshold) * (reward ^ 2))
        +
        (((ticks-per-episode - ticks-in-cluster) / ticks-per-episode) * penalty)
      set reward-list lput rew reward-list ]
  report rew
end

to-report isEndState
  ;if is-there-cluster = true [
  if (ticks > 0) and ((ticks mod ticks-per-episode) = 0) [
    ;set is-there-cluster false
    report true
  ]
  report false
end

to resetEpisode
  let avg-rew avg? reward-list
  set g-reward-list lput avg-rew g-reward-list

  ;set-current-plot-pen (word who)
  ;plot avg-rew

  set reward-list []
  set ticks-in-cluster 0
  ;ask patch-here [ set chemical 0 ]
  ;ask [neighbors] of patch-here [ set chemical 0 ]

  setxy random-xcor random-ycor
end

;;;;;;;;;;;;;;;;
;; RL actions ;;
;;;;;;;;;;;;;;;;

to move-toward-chemical  ;; turtle procedure
  ;; examine the patch ahead of you and two nearby patches;
  ;; turn in the direction of greatest chemical
  let ahead [chemical] of patch-ahead look-ahead
  let myright [chemical] of patch-right-and-ahead sniff-angle look-ahead
  let myleft [chemical] of patch-left-and-ahead sniff-angle look-ahead
  ifelse (myright >= ahead) and (myright >= myleft)
  [ rt sniff-angle ]
  [ if myleft >= ahead
    [ lt sniff-angle ] ]
  fd 1                    ;; default don't turn
end

to random-walk  ;; turtle procedure
  ifelse (random-float 1) > 0.5
    [ rt random-float wiggle-angle ]
    [ lt random-float wiggle-angle ]
  fd 1
end

to drop-chemical  ;; turtle procedure
  set chemical chemical + chemical-drop
end

to dont-drop-chemical  ;; turtle procedure

end

to move-and-drop  ;; turtle procedure
  move-toward-chemical
  drop-chemical
end

to walk-and-drop  ;; turtle procedure
  random-walk
  drop-chemical
end

;;;;;;;;;;;;;;;;;;;;;
;; SHOW procedures ;;
;;;;;;;;;;;;;;;;;;;;;

;to setup-individual-plot
;  set-current-plot "Average cluster size in # of turtles within cluster-radius"
;  create-temporary-plot-pen (word who)
;  let p-color scale-color one-of base-colors who 0 count turtles
;  set-plot-pen-color p-color
;end

to setup-global-plot [p-name pen-name pen-color]
  set-current-plot p-name
  create-temporary-plot-pen pen-name
  set-plot-pen-color pen-color
end

;to plot-individual
;  set-current-plot "Average cluster size in # of turtles within cluster-radius"
;  set-current-plot-pen (word who)
;  plot cluster
;end

to plot-global [p-name pen-name what]
  set-current-plot p-name
  set-current-plot-pen pen-name
  plot what
end

to log-ticks [msg what]
  if (ticks > 0) and ((ticks mod print-every) = 0)
    [ type "t" type ticks type ") " type msg print what ]
end

to log-episodes [msg what]
  type "E" type episode type ") " type msg print what
end

;;;;;;;;;;;;;;;;;;;;;
;; HELP procedures ;;
;;;;;;;;;;;;;;;;;;;;;

to check-cluster  ;; turtle procedure
  set cluster count turtles in-radius cluster-radius
  ifelse cluster > cluster-threshold
    [ set in-cluster true
      set is-there-cluster true
      set ticks-in-cluster ticks-in-cluster + 1 ]
    [ set in-cluster false ]
end

;to l-check-cluster
;  set l-cluster count turtles in-radius cluster-radius
;  set l-cluster l-cluster + count Learners in-radius cluster-radius
;  ifelse l-cluster > cluster-threshold
;    [ set l-in-cluster true
;      set is-there-cluster true
;      set l-ticks-in-cluster l-ticks-in-cluster + 1 ]
;    [ set l-in-cluster false ]
;end

to-report avg? [collection]
  let summ 0
  let lengthh 0
  foreach collection [ i ->
    set summ summ + i
    set lengthh lengthh + 1
  ]
  if lengthh > 0
    [ report summ / lengthh ]
  report 0
end

to-report avg-cluster?
  let c-sum 0
  let c-length 0
  foreach sort turtles [ t ->
    if ([cluster] of t) > cluster-threshold
      [
        set c-sum c-sum + ([cluster] of t)
        set c-length c-length + 1
      ]
  ]
  let c-avg 0
  if not (c-length = 0)
    [ set c-avg c-sum / c-length ]
  report c-avg
end

;;;;;;;;;;;;;;;;;;;;;;;;
;; LOGGING procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;

to log-params  ;; NB explicitly modify lines "e-greedy", "ACION SPACE", "OBSERVATION SPACE", and "REWARD" (everything else is logged automatically)
  file-print "--------------------------------------------------------------------------------"
  file-type "TIMESTAMP: " file-print date-and-time
  file-print "PARAMS:"
  file-type "  population " file-print population
  file-type "  wiggle-angle " file-print wiggle-angle
  file-type "  look-ahead " file-print look-ahead
  file-type "  sniff-threshold " file-print sniff-threshold
  file-type "  sniff-angle " file-print sniff-angle
  file-type "  chemical-drop " file-print chemical-drop
  file-type "  diffuse-share " file-print diffuse-share
  file-type "  evaporation-rate " file-print evaporation-rate
  file-type "  cluster-threshold " file-print cluster-threshold
  file-type "  cluster-radius " file-print cluster-radius
  file-type "  learning-turtles " file-print learning-turtles
  file-type "  ticks-per-episode " file-print ticks-per-episode
  file-type "  episodes " file-print episodes
  file-type "  learning-rate " file-print learning-rate
  file-type "  discount-factor " file-print discount-factor
  file-type "  reward " file-print reward
  file-type "  penalty " file-print penalty
  file-type "  e-greedy " file-type 0.5 file-type " " file-type 0.9 file-print ""                                   ;; NB: CHANGE ACCORDING TO ACTUAL CODE!
  file-type "ACTION SPACE: " file-type "move-and-drop " file-type "walk-and-drop " file-type "move-toward-chemical " file-type "random-walk " file-print "drop-chemical"  ;; NB: CHANGE ACCORDING TO ACTUAL CODE!
  file-type "OBSERVATION SPACE: " file-type "chemical-here " file-print "in-cluster"                                ;; NB: CHANGE ACCORDING TO ACTUAL CODE!
  file-type "REWARD: " file-print "rewardFunc8"                                                                     ;; NB: CHANGE ACCORDING TO ACTUAL CODE!
  file-print "--------------------------------------------------------------------------------"
  file-type "Episode, " file-type "Tick, " file-type "Avg custer size X tick, " file-type "Avg reward X episode, " file-print "Actions distribution"  ;; How many turtles choose each available action
end

; Copyright 2022 Stefano Mariani
@#$#@#$#@
GRAPHICS-WINDOW
207
112
725
631
-1
-1
10.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
7
35
433
68
population
population
0
1000
0.0
10
1
NIL
HORIZONTAL

SLIDER
6
214
197
247
sniff-threshold
sniff-threshold
0.1
5.0
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
6
251
197
284
sniff-angle
sniff-angle
0.0
180.0
45.0
5
1
degrees
HORIZONTAL

SLIDER
6
138
197
171
wiggle-angle
wiggle-angle
0.0
90
45.0
5
1
degrees
HORIZONTAL

BUTTON
35
88
98
121
setup
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
104
88
168
121
go
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
7
288
197
321
chemical-drop
chemical-drop
1
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
7
325
197
358
diffuse-share
diffuse-share
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
8
362
197
395
evaporation-rate
evaporation-rate
0
1
0.95
0.05
1
NIL
HORIZONTAL

SLIDER
6
176
197
209
look-ahead
look-ahead
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
1216
65
1388
98
cluster-radius
cluster-radius
1
50
10.0
1
1
NIL
HORIZONTAL

SWITCH
1217
103
1320
136
label?
label?
1
1
-1000

PLOT
735
11
1210
319
Average cluster size in # of turtles within cluster-radius
ticks
# of turtles
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

INPUTBOX
29
462
178
522
print-every
50.0
1
0
Number

SLIDER
1215
27
1479
60
cluster-threshold
cluster-threshold
0
250
25.0
1
1
NIL
HORIZONTAL

INPUTBOX
1217
363
1366
423
ticks-per-episode
250.0
1
0
Number

INPUTBOX
1370
363
1519
423
episodes
1000.0
1
0
Number

PLOT
736
324
1210
632
Average reward per episode
episodes
average reward
0.0
1.0
0.0
1.0
true
true
"" ""
PENS

BUTTON
1216
324
1340
357
NIL
setup-learning
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
1343
324
1406
357
NIL
learn
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
1218
430
1390
463
learning-rate
learning-rate
0
1
0.9
0.05
1
NIL
HORIZONTAL

SLIDER
1218
470
1390
503
discount-factor
discount-factor
0
1
0.9
0.05
1
NIL
HORIZONTAL

SLIDER
1399
451
1571
484
reward
reward
1
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
1400
489
1572
522
penalty
penalty
-100
0
-1.0
1
1
NIL
HORIZONTAL

SLIDER
1216
285
1388
318
learning-turtles
learning-turtles
1
100
50.0
1
1
NIL
HORIZONTAL

TEXTBOX
1220
510
1394
636
learning-rate) 0 = only predefined policy (learns nothing), 1 = only latest rewards (learns too much)\n\ndiscount-factor) 0 = only care about immediate reward, 1 = only care about future reward
11
0.0
1

@#$#@#$#@
## GOALS

Teach learning slimes (= red turtles, aka "learning-turtles") to aggregate in clusters using **basic Q-learning**.

## TL;RD: quick info about RL here

Roughly, the Netlogo screen is split in two:

 * on left side of the simulation arena are the parameters of the basic slime mold aggregation model (actually there are more than the original model, as I extended the number of parameters directly configurable at run-time from the GUI)

 * on the right side there are RL-related parameters

**Important**: as the left side parameters are related to basic slime behaviours, they obviously also affect learning (e.g. decreasing the evaporation rate makes learning more difficult). Hence, **you are strongly advised to keep them fixed** once you find suitable behaviour with `setup` & `go` (no RL involved there).

To **keep track of experiments** remember to:

 1. Explictly modify experiment name in procedure `setup-learning`
 2. Configure RL stuff within `ask Learners [...` in procedure `setup-learning
 3. Explicitly modify lines `"e-greedy", "ACION SPACE", "OBSERVATION SPACE"`, and `"REWARD"` in procedure `log-params` at the very end of file


### NON-RL parameters

As already said, these parameters describe slimes behaviour in the original model, but also indirectly affect learning, making it more difficult or easier (e.g. decreasing the evaporation rate makes learning more difficult).

 * `population` controls the number of non-learning slimes (= blue turtles)
 * `wiggle-angle` controls how much slimes steer around---no effect on learning
 * `look-ahead` controls how far slimes can smell pheromone (higher values enable forming elongated clusters)---no effect on learning
 * `sniff-threshold` controls how sensitive slimes are to pheromone (higher values make slimes less sensitive to pheromone)---unclear effect on learning, could be negligible
 * `sniff-angle` controls how wide is the cone within which slimes can smell pheromone in nearby patches (higher values make slimes able to smell pheromone in a wider cone)---unclear effect on learning, could be negligible
 * `chemical-drop` controls how much pheromone slimes deposit on their patch---unclear effect on learning, could be negligible
 * `diffuse-share` controls how much pheromone diffuses in nearby patches (higher values mean more pheromone is diffused)---unclear effect on learning, but **likely lower values make learning more difficult**
 * `evaporation-rate` controls how much pheromone is retained over time (higher values mean less pheromone evaporates)---unclear effect on learning, but **likely lower values make learning more difficult**

### RL parameters

All the following parameters have a direct effect on Q-learning of learning slimes.

 * `cluster-threshold` controls the minimum number of slimes needed to consider an aggregate within `cluster-radius` a cluster (the higher the more difficult to consider an aggregate a cluster)---**the higher the more difficult to obtain a positive reward** for being within a cluster for learning slimes
 * `cluster-radius` controls the range considered by slimes to count other slimes within a cluster (the higher the easier to form clusters, as turtles far apart are still counted together)---**the higher the easier it is to obtain a positive reward** for being within a cluster for learning slimes
 * `learning-turtles` controls the number of learning slimes (= red turtles)
 * `ticks-per-episode` controls how long a learning episode last (on episode end, slimes position are randomly reset and pheromone is cleared)---slimes should be given enough time to form clusters, hence it is strongly advisable to set this parameter at the very least **2x as low as allowed by non learning slimes forming clusters**
 * `episodes` controls how many learning episodes are automatically run
 * `learning-rate` is the classical Q-learning param, controlling "how fast" slimes learn---higher values cause bigger adjustements to Q-values
 * `discount-factor` is the classical Q-learning param, controlling how much future rewards are given value over immediate ones---higher values cause bigger value given to future rewards
 * `reward` is the raw reward value considered by the reward function (check code to see how it is used)
 * `penalty` is the raw penalty (= negative reward) value considered by the reward function (check code to see how it is used)

### PLOTS

The top plots tracks the average "size" of clusters (in terms of number of turtles therein) based on two parameters:

 * `cluster-threshold` is the minimum number of turtles needed to consider the aggregate a cluster (the higher the more difficult to form legit clusters, hence the more difficult to obtain a positive reward for learning turtles)
 * `cluster-radius` is the range considered to count turtles in a cluster (the higher the easier to form clusters, as turtles far apart are still counted together)

This plot is better suited to monitor non-learning turtles behaviour during a `setup` & `go`: the higher the value the less-and-bigger clusters are produced

The bottom plot is meant to monitor learning, as it plots the average reward per episode (average of the individual rewards of each learning turtle).

### Other params

TBD

-----
## ORIGINAL INFO BELOW
-----

## WHAT IS IT?

This model is inspired by the aggregation behavior of slime-mold cells.
The slime mold spends much of its life as thousands of distinct single-celled units, each moving separately. Under the right conditions, those many cells will coalesce into a single, larger organism. When the environment is less hospitable, the slime mold acts as a single organism; when the weather turns cooler and the mold enjoys a large food supply, "it" becomes a "they." The slime mold oscillates between being a single creature and a swarm.

This model shows how creatures can aggregate into clusters without the control of a "leader" or "pacemaker" cell. This finding was first described by Evelyn Fox Keller and Lee Segel in a paper in 1970.

Before Keller began her investigations, the conventional belief had been that slime mold swarms formed at the command of "pacemaker" cells that ordered the other cells to begin aggregating. In 1962, Shafer showed how the pacemakers could use cyclic AMP as a signal of sorts to rally the troops; the slime mold generals would release the compounds at the appropriate moments, triggering waves of cyclic AMP that washed through the entire community, as each isolated cell relayed the signal to its neighbors. Slime mold aggregation, in effect, was a giant game of Telephone — but only a few elite cells placed the original call.

For the twenty years that followed the publication of Shafer's original essay, mycologists assumed that the missing pacemaker cells were a sign of insufficient data, or poorly designed experiments. But Keller and Segel took another, more radical approach. They shows that Shafer had it wrong --  that the community of slime mold cells were organizing themselves without any need for pacemakers. This was one of the first examples of emergence and self-organization in biology.

Initially, biologists did not accept this explanation. Indeed, the pacemaker hypothesis would continue as the reigning model for another decade. Now, slime mold aggregation is  recognized as a classic case study in bottom-up self-organizing behavior.

In this model, each turtle drops a chemical pheromone (shown in green). The turtles also "sniff" ahead, trying to follow the gradient of other turtles' chemicals. Meanwhile, the patches diffuse and evaporate the pheromone. Following these simple, decentralized rules, the turtles aggregate into clusters.

## HOW TO USE IT

Click the SETUP button to set up a collection of slime-mold cells. Click the GO button to start the simulation.

The POPULATION slider controls the number of slime mold cells in the simulation. Changes in the POPULATION slider do not have any effect until the next SETUP command.

The other sliders affect the way turtles move. Changes to them will immediately affect the model run.

SNIFF-THRESHHOLD -- The minimum amount of chemical that must be present in a turtle's patch before the turtle will look for a chemical gradient to follow. This parameter causes the turtles to aggregate only when there are enough other cells nearby. The default value is 1.0.

SNIFF-ANGLE -- The amount, in degrees, that a turtle turns to the left and right to check for greater chemical concentrations. The default value is 45.

WIGGLE-ANGLE -- The maximum amount, in degrees, that a turtle will turn left or right in its random movements. When WIGGLE-ANGLE is set to zero, the turtle will remain at the same heading until it finds a chemical gradient to follow. The default value is 40.

WIGGLE-BIAS -- The bias of a turtle's average wiggle. When WIGGLE-BIAS = 0, the turtle's average movement is straight ahead. When WIGGLE-BIAS > 0, the turtle will tend to move more right than left. When BIAS < 0, the turtle will tend to move more left than right. The default value is 0.

There are several other critical parameters in the model that are not accessible by sliders. They can be changed by modifying the code in the procedures window.  They are:
- the evaporation rate of the chemical -- set to 0.9
- the diffusion rate of the chemical -- set to 1
- the amount of chemical deposited at each step -- set to 2

## THINGS TO NOTICE

With 100 turtles, not much happens. The turtles wander around dropping chemical, but the chemical evaporates and diffuses too quickly for the turtles to aggregate.

With 400 turtles, the result is quite different. When a few turtles happen (by chance) to wander near one another, they create a small "puddle" of chemical that can attract any number of other turtles in the vicinity. The puddle then becomes larger and more attractive as more turtles enter it and deposit their own chemicals. This process is a good example of positive feedback: the more turtles, the larger the puddle; and the larger the puddle, the more likely it is to attract more turtles.

## THINGS TO TRY

Try different values for the SNIFF-THRESHOLD, SNIFF-ANGLE, WIGGLE-ANGLE, and WIGGLE-BIAS sliders. How do they affect the turtles' movement and the formation of clumps?

Change the SNIFF-ANGLE and WIGGLE-ANGLE sliders after some clumps have formed. What happens to the clumps? Try the same with SNIFF-THRESHOLD and WIGGLE-BIAS.

## EXTENDING THE MODEL

Modify the program so that the turtles aggregate into a single large cluster.

How do the results change if there is more (or less) randomness in the turtles' motion?

Notice that the turtles only sniff for chemical in three places: forward, SNIFF-ANGLE to the left, and SNIFF-ANGLE to the right. Modify the model so that the turtles sniff all around. How does their clustering behavior change? Modify the model so that the turtles sniff in even fewer places. How does their clustering behavior change?

What "critical number" of turtles is needed for the clusters to form? How does the critical number change if you modify the evaporation or diffusion rate?

Can you find an algorithm that will let you plot the number of distinct clusters over time?

## NETLOGO FEATURES

Note the use of the `patch-ahead`, `patch-left-and-ahead`, and `patch-right-and-ahead` primitives to do the "sniffing".

## RELATED MODELS

Ants uses a similar idea of creatures that both drop chemical and follow the gradient of the chemical.

## CREDITS AND REFERENCES

Keller, E & Segel, L. (1970). Initiation of slime mold aggregation viewed as an instability. Journal of Theoretical Biology,
Volume 26, Issue 3, March 1970, Pages 399–415.

Wilensky, U., & Resnick, M. (1999). Thinking in levels: A dynamic systems approach to making sense of the world. Journal of Science Education and Technology, 8(1), 3-19.

Johnson, S. (2001). Emergence: The Connected Lives of Ants, Brains, Cities, and Software. New York: Scribner.

Resnick, M. (1996). Beyond the centralized mindset. Journal of the Learning Sciences, 5(1), 1-22.

See also http://www.creepinggarden.com for video of slime mold.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1997).  NetLogo Slime model.  http://ccl.northwestern.edu/netlogo/models/Slime.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Stefano Mariani (stefano.mariani@unimore.it)

Original copyright info below.

Copyright 1997 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was developed at the MIT Media Lab using CM StarLogo.  See Resnick, M. (1994) "Turtles, Termites and Traffic Jams: Explorations in Massively Parallel Microworlds."  Cambridge, MA: MIT Press.  Adapted to StarLogoT, 1997, as part of the Connected Mathematics Project.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2000.

<!-- 1997 2000 MIT -->
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
