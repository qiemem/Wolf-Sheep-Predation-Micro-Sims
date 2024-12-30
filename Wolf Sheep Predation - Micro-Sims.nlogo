extensions [ ls table ]

; The available basic behaviors of wolves and sheep are put in a separate nls file so that the primary and cognitive model use the same behaviors.
__includes [ "actions.nls" ]


globals [
  sheep-actions ; list of actions that each sheep can choose from each step
  wolf-actions ; list of actions that wolves can choose from each step

  patches-with-sheep ; number of patches containing at least one sheep; used for computing wolf efficiency
  num-sheep-actions
  num-wolf-actions
  grass ; number of patches with grass

  sheep-efficiency ; measures sheep's ability to find food
  wolf-efficiency ; measures wolves' ability to find food
  sheep-escape-efficiency ; measures sheep's ability to avoid wolves

  ; The following are used to calculate the entire run efficiency numbers
  sheep-efficiency-weighted-sum ; sum of efficiencies of sheep actions for entire run
  wolf-efficiency-weighted-sum ; sum of efficiencies of wolf actions for entire run
  sheep-escape-efficiency-weighted-sum ; sum of efficiencies of escapes from wolf eat attempts for entire run
  total-sheep-actions ; total number of sheep actions for entire run
  total-wolf-actions ; total number of wolf actions for entire

  smoothed-values ; table used for smoothing efficiency statistics for graphing

  weighted-moving-averages

  param-reporters
  last-param-values
]

; Sheep and wolves are both breeds of turtle.
breed [ sheep a-sheep ]  ; sheep is its own plural, so we use "a-sheep" as the singular.
breed [ wolves wolf ]
turtles-own [
  energy ; wolves and sheep gain energy when they eat and lose energy when they move
  chosen-move ; the move the agent has chosen to perform on the current tick
]
patches-own [
  countdown ; ticks until the patches regrows grass
]

to setup
  ls:reset
  clear-all

  set smoothed-values table:make
  set weighted-moving-averages table:make

  ; Define the available actions as strings of code so that they can be passed between models
  ; The component behavior (MOVE, EAT-GRASS, TURN, EAT-SHEEP) are defined in actions.nls.
  ; Each behavior reports the amount of energy it uses or gains; thus, adding them together gives the total change in energy for the agent.
  set sheep-actions [
    "(move 1) + (eat-grass)"
    "(turn 30) + (move 1) + (eat-grass)"
    "(turn -30) + (move 1) + (eat-grass)"
  ]
  set wolf-actions [
    "(move 1) + (eat-sheep)"
    "(turn 30) + (move 1) + (eat-sheep)"
    "(turn -30) + (move 1) + (eat-sheep)"
  ]
  ask patches [
    set pcolor brown
    set countdown random grass-regrowth-time
  ]
  ask n-of (initial-grass-density * count patches) patches [
    set pcolor green
    set countdown grass-regrowth-time
  ]

  set-default-shape wolves "wolf"
  create-wolves initial-number-wolves [
    setxy random-xcor random-ycor
    set color black
    set size 2
    let b wolf-threshold / 2
    set energy b + random b
  ]

  set-default-shape sheep "sheep"
  create-sheep initial-number-sheep [
    setxy random-xcor random-ycor
    set shape "sheep"
    set color white
    set size 1.5
    let b sheep-threshold / 2
    set energy b + random b
  ]

  set grass count patches with [ pcolor = green ]

  set sheep-efficiency 1
  set wolf-efficiency 1
  set sheep-escape-efficiency 1

  set num-wolf-actions count wolves
  set num-sheep-actions count sheep

  set param-reporters (list
    [-> grass-regrowth-time ]
    [-> sheep-gain-from-food ]
    [-> sheep-threshold ]
    [-> newborn-energy ]
    [-> wolf-gain-from-food ]
    [-> sheep-see-sheep? ]
    [-> sheep-see-wolves? ]
    [-> sheep-see-grass? ]
    [-> wolves-see-sheep? ]
    [-> wolves-see-wolves? ]
    [-> wolves-see-grass? ]
    [-> sheep-vision ]
    [-> sheep-sim-n ]
    [-> sheep-sim-l ]
    [-> sheep-death-penalty ]
    [-> wolf-vision ]
    [-> wolf-sim-n ]
    [-> wolf-sim-l ]
    [-> wolf-death-penalty ]
  )

  reset-ticks
end

to go
  if not any? turtles [ stop ]

  set sheep-efficiency 0
  set wolf-efficiency 0
  set sheep-escape-efficiency 0

  ; Wolves and sheep choose actions before any agent acts so that they are all working from the same amount of information.
  ; If the individual agents choose actions immediately before acting, we have to track which agents have acted and which
  ; haven't in order for the cognitive model to not be a step behind for some portion of the other agents, dramatically
  ; increasing complexity. Alternative methods of scheduling include, for instance, having sheep choose actions, then
  ; having sheep act, then having wolves choose actions, then having wolves act. See Chapter 7 in Head (2024) for a detailed
  ; description of scheduling strategies.
  ask sheep [ sheep-choose-action ]
  ask wolves [ wolf-choose-action ]
  wolves-and-sheep-act
  ask patches [ grow-grass ]

  tick
end

; Have sheep choose a move based on simulations of outcomes defaulting to a random move if no simulations are performed
to sheep-choose-action
  let results simulate-sheep
  set chosen-move ifelse-value empty? results [ one-of sheep-actions ] [ pick-best results ]
end

; Have sheep simulate possible different scenerios with different actions according to current parameters and report resulting reward values
to-report simulate-sheep
  report simulate sheep-vision sheep-sim-n sheep-sim-l sheep-death-penalty sheep-see-sheep? sheep-see-wolves? sheep-see-grass?
end

; Have wolf choose a move based on simulations of outcomes defaulting to a random move if no simulations are performed
to wolf-choose-action
  let results simulate-wolf
  set chosen-move ifelse-value empty? results [ one-of wolf-actions ] [ pick-best results ]
end

; Have wolf simulate possible different scenerios with different actions according to current parameters and report resulting reward values
to-report simulate-wolf
  report simulate wolf-vision wolf-sim-n wolf-sim-l wolf-death-penalty wolves-see-sheep? wolves-see-wolves? wolves-see-grass?
end

; Make all wolves and sheep carry out selected actions, tracking efficiency statistics
to wolves-and-sheep-act
  ; Initialize factors used in computing species efficiency. Each factor must be kept up to date throughout the tick to
  ; reflect the state of the model when each agent acts.
  set num-sheep-actions 0 ; number of sheep who lived long enough to act
  set num-wolf-actions count wolves ; Wolves can't die before acting, so we can compute number of wolves acting here.
  set patches-with-sheep count patches with [ any? sheep-here ]

  ask turtles [
    ifelse breed = sheep [
      set num-sheep-actions num-sheep-actions + 1
      sheep-act
    ] [
      wolf-act
    ]
  ]

  set sheep-efficiency-weighted-sum sheep-efficiency-weighted-sum + sheep-efficiency
  set wolf-efficiency-weighted-sum wolf-efficiency-weighted-sum + wolf-efficiency
  set sheep-escape-efficiency-weighted-sum sheep-escape-efficiency-weighted-sum + sheep-escape-efficiency
  set total-sheep-actions total-sheep-actions +  num-sheep-actions
  set total-wolf-actions total-wolf-actions + num-wolf-actions

  ; Normalize aggregated efficiency values by the number of agents who acted.
  set wolf-efficiency safe-div wolf-efficiency num-wolf-actions
  set sheep-escape-efficiency safe-div sheep-escape-efficiency num-wolf-actions
  set sheep-efficiency safe-div sheep-efficiency num-sheep-actions
end

; Have wolf carry out selected action and compute efficiency
to wolf-act
  ; Efficiency is effectively the ratio between what proportion of agents ate and their probability of randomly eating
  ; based on on population densities. However, those population densities change throughout a tick, so we have to compute
  ; them at the time the agent acts.
  let prob-of-eating patches-with-sheep / count patches
  let num-sheep count sheep
  act chosen-move
  ; Detect if the wolf successfully at a sheep. The only way the number of sheep can change during the wolf's action is
  ; if the wolf eats a sheep. Using COUNT SHEEP rather than COUNT SHEEP-HERE means that this calculation will still be
  ; correct if we allow wolves to eat that are not only on the pach (e.g. in a radius around the wolf). It should not
  ; impact performance.
  ifelse count sheep < num-sheep [
    set wolf-efficiency wolf-efficiency + 1 / prob-of-eating
  ] [
    set sheep-escape-efficiency sheep-escape-efficiency + 1 / (1 - prob-of-eating)
  ]
  death ; wolves die if out of energy
  if energy > wolf-threshold [ reproduce wolf-threshold ]
end

; Have sheep carry out selected action and compute efficiency
to sheep-act
  ; The number of patches with sheep must be kept up to date and COUNT PATCHES WITH [ ANY? SHEEP-HERE ] is expensive. Hence,
  ; update it based on the sheep's movement.
  let was-alone? not any? other sheep-here
  act chosen-move
  let is-alone? not any? other sheep-here
  (ifelse
    was-alone? and not is-alone? [ set patches-with-sheep patches-with-sheep - 1 ]
    not was-alone? and is-alone? [ set patches-with-sheep patches-with-sheep + 1 ]
  )
  death
  if energy > sheep-threshold [ reproduce sheep-threshold ]
end

to grass-get-eaten ; patch procedure
  set sheep-efficiency sheep-efficiency + count patches / grass
  set grass grass - 1
  set pcolor brown
end

to act [ action ] ; turtle pocedure
  ; Actions report the change in energy they cause.
  set energy energy + runresult action
end

to act-random [ actions ] ; turtle procedure
  set energy energy + runresult (one-of actions)
end

to-report pick-best [ results ] ; turtle procedure
  let moves remove-duplicates map first results
  let scores map [ m -> safe-mean map last filter [ p -> first p = m ] results ] moves
  let best max scores
  (foreach moves scores [ [ m s ] ->
    if s = best [
      report m
    ]
  ])
end

; Turtle simulates possible different scenerios with different actions and report resulting rewards
to-report simulate [ vision num dur death-penalty see-sheep? see-wolves? see-grass? ]
  ifelse num > 1 and dur > 0 and (see-sheep? or see-wolves? or see-grass?) [
    setup-mind vision see-sheep? see-wolves? see-grass? death-penalty
    ls:let num num
    ls:let dur dur
    report ls:report 0 [ run-micro-sims 0 num dur ]
  ] [
    report []
  ]
end

; Turtle initiliazes the cognitive model based on their surroundings
; All turtles use the same model for their cognitive model as it is completely re-initialized for each batch of simulations and thus does not need to track any state
to setup-mind [ vision see-sheep? see-wolves? see-grass? death-penalty ]
  if empty? ls:models [ ; Create the cognitive model if we haven't yet
    ls:create-models 1 "wsp-cog-model.nlogo"
    ls:assign 0 wolf-actions wolf-actions
    ls:assign 0 sheep-actions sheep-actions
  ]

  ; This could be more concise using LS:ASSIGN to set these variables, but its slower than LS:LET.
  let visible-patches patches in-radius vision
  ; Get coordinates of wolves and sheep around the turtle relative to the turtles position.
  ls:let new-wolf-coords [ relative-cors ] of ifelse-value see-wolves? [ other wolves-on visible-patches ] [ no-turtles ]
  ls:let new-sheep-coords [ relative-cors ] of ifelse-value see-sheep? [ other sheep-on visible-patches ] [ no-turtles ]
  ifelse see-grass? [
    ; Get coordinates of patches with and without grass relative to current turtle.
    ; Note that encoding this with something like:
    ; map [ p -> [ pcolor ] of p ] sort visible-patches
    ; would be much more compact. However, its faster and easier to initiliaze with lists of coordinates since you can use PATCHES AT-POINTS.
    ls:let new-live-grass-coords [ relative-pcors ] of visible-patches with [ pcolor = green ]
    ls:let new-dead-grass-coords [ relative-pcors ] of visible-patches with [ pcolor = brown ]
  ] [
    ls:let new-live-grass-coords []
    ls:let new-dead-grass-coords []
  ]

  ls:let my-energy energy
  ; Store where in the turtle's current patch it is
  ls:let my-xcor (xcor - pxcor)
  ls:let my-ycor (ycor - pycor)
  ls:let my-heading heading

  ls:let new-sheep-gain-from-food sheep-gain-from-food
  ls:let new-wolf-gain-from-food wolf-gain-from-food

  ls:let my-breed (word breed)

  ls:let new-vision vision
  ls:let new-death-penalty death-penalty

  ls:ask 0 [
    ; Set cognitive model parameters based on observed local state
    set reward-discount 0.8
    set wolf-coords new-wolf-coords
    set sheep-coords new-sheep-coords
    set live-grass-coords new-live-grass-coords
    set dead-grass-coords new-dead-grass-coords
    set ego-breed my-breed
    set init-energy my-energy
    set init-xcor my-xcor
    set init-ycor my-ycor
    set init-heading my-heading
    set sheep-gain-from-food new-sheep-gain-from-food
    set wolf-gain-from-food new-wolf-gain-from-food
    set vision new-vision
    set death-penalty new-death-penalty
    set grass-density ifelse-value empty? live-grass-coords [
      0.5
    ] [
      length live-grass-coords / (length live-grass-coords + length dead-grass-coords)
    ]
  ]
end

to-report can-see-grass? ; turtle reporter
  report ifelse-value is-a-sheep? self [ sheep-see-grass? ] [ wolves-see-grass? ]
end

to-report can-see-wolves? ; turtle reporter
  report ifelse-value is-a-sheep? self [ sheep-see-wolves? ] [ wolves-see-wolves? ]
end

to-report can-see-sheep? ; turtle reporter
  report ifelse-value is-a-sheep? self [ sheep-see-sheep? ] [ wolves-see-sheep? ]
end

to-report relative-cors ; turtle reporter
  report (list (relative-xcor xcor) (relative-ycor ycor) heading)
end

to-report relative-pcors ; turtle reporter
  report list (relative-xcor pxcor) (relative-ycor pycor)
end

to-report relative-xcor [ x ] ; turtle reporter
  let d x - [ pxcor ] of myself
  report (ifelse-value
    ; account for world wrapping
    d > world-width / 2 [ d - world-width ]
    (0 - d) > world-width / 2 [ d + world-width ]
    [ d ]
  )
end

to-report relative-ycor [ y ] ; turtle reporter
  let d y - [ pycor ] of myself
  report (ifelse-value
    ; account for world wrapping
    d > world-height / 2 [ d - world-height ]
    (0 - d) > world-height / 2 [ d + world-height ]
    [ d ]
  )
end

to reproduce [ threshold ] ; turtle procedure
  let baby-energy round (threshold * newborn-energy)
  set energy energy - baby-energy
  hatch 1 [
    set energy baby-energy
    rt random-float 360
    fd 1
    if breed = sheep and not any? other sheep-here [
      set patches-with-sheep patches-with-sheep + 1
    ]
  ]
end

to sheep-die ; sheep procedure - also called from actions.nls; primary and child model handle differently
  if not any? other sheep-here [
    set patches-with-sheep patches-with-sheep - 1
  ]
  die
end

to death  ; turtle procedure (i.e. both wolf and sheep procedure)
  ; when energy dips below zero, die
  if energy < 0 [
    ifelse is-a-sheep? self [ sheep-die ] [ die ]
  ]
end

to grow-grass  ; patch procedure
  ; countdown on brown patches:  if reach 0, grow some grass
  if pcolor = brown [
    ifelse countdown <= 0 [
      set pcolor green
      set grass grass + 1
      set countdown grass-regrowth-time
    ] [
      set countdown countdown - 1
    ]
  ]
end

to-report safe-mean [ lst ] ; report 0 on empty list instead of erroring
  ifelse empty? lst [
    report 0
  ] [
    report mean lst
  ]
end

to-report safe-div [ num den ] ; report 0 on instead of divide by zero
  if den = 0 [ report 0 ]
  report num / den
end

; Applies multiple passes of exponential smoothing to isolate long term trends. This works by applying a simple
; first order low pass filter multiple times. A single pass is a standard exponential moving average. The coefficient, C,
; defines a cutoff frequency; patterns that happen more quickly
; Each subsequent
; pole
; NAME is a string to
to-report smoothed-val [ name cur-value order c ]
  foreach range order [ i ->
    let var (word name "-" (1 + i))
    let last-value table:get-or-default smoothed-values var cur-value
    set cur-value last-value + c * (cur-value - last-value)
    table:put smoothed-values var cur-value
  ]
  report cur-value
end

to-report weighted-moving-average [ name value weight window ]
  let weighted-values-key (word name "-weighted-values")
  let weights-key (word name "-weights")
  let weighted-total-key (word name "-weighted-total")
  let total-weight-key (word name "-total-weight")

  let weighted-value value * weight
  let weighted-values lput weighted-value table:get-or-default weighted-moving-averages weighted-values-key []
  let weights lput weight table:get-or-default weighted-moving-averages weights-key []
  let weighted-total weighted-value + table:get-or-default weighted-moving-averages weighted-total-key 0
  let total-weight  weight + table:get-or-default weighted-moving-averages total-weight-key 0

  while [ length weighted-values > window ] [
    set weighted-total weighted-total - first weighted-values
    set total-weight total-weight - first weights
    set weighted-values but-first weighted-values
    set weights but-first weights
  ]

  table:put weighted-moving-averages weighted-values-key weighted-values
  table:put weighted-moving-averages weights-key weights
  table:put weighted-moving-averages weighted-total-key weighted-total
  table:put weighted-moving-averages total-weight-key total-weight

  report safe-div weighted-total total-weight
end

to-report smoothed [ variable c ]
  report smoothed-val variable (runresult variable) 1 c
end


to-report is-grass?
  report pcolor = green
end

; Copyright 2024 Bryan Head.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
720
10
1238
529
-1
-1
10.0
1
14
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
0
10
175
43
initial-number-sheep
initial-number-sheep
0
250
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
150
175
183
sheep-gain-from-food
sheep-gain-from-food
0.0
50.0
5.0
1.0
1
NIL
HORIZONTAL

SLIDER
175
10
350
43
initial-number-wolves
initial-number-wolves
0
250
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
115
175
148
grass-regrowth-time
grass-regrowth-time
0
100
30.0
1
1
NIL
HORIZONTAL

BUTTON
0
80
85
113
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
85
80
175
113
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

PLOT
370
10
720
190
populations
time
pop.
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"sheep" 1.0 0 -13345367 true "" "plot count sheep"
"wolves" 1.0 0 -2674135 true "" "plot count wolves"
"grass / 4" 1.0 0 -10899396 true "" "plot grass / 4"

MONITOR
645
80
715
125
sheep
count sheep
3
1
11

MONITOR
645
125
715
170
wolves
count wolves
3
1
11

SLIDER
0
325
175
358
sheep-vision
sheep-vision
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
175
325
350
358
wolf-vision
wolf-vision
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
0
185
175
218
sheep-threshold
sheep-threshold
0
200
70.0
10
1
NIL
HORIZONTAL

SLIDER
175
185
350
218
wolf-threshold
wolf-threshold
0
200
70.0
10
1
NIL
HORIZONTAL

SLIDER
0
360
175
393
sheep-sim-n
sheep-sim-n
1
50
12.0
1
1
NIL
HORIZONTAL

SLIDER
0
395
175
428
sheep-sim-l
sheep-sim-l
1
sheep-vision
3.0
1
1
NIL
HORIZONTAL

SLIDER
175
360
350
393
wolf-sim-n
wolf-sim-n
1
50
12.0
1
1
NIL
HORIZONTAL

SLIDER
175
395
350
428
wolf-sim-l
wolf-sim-l
1
wolf-vision
3.0
1
1
NIL
HORIZONTAL

SLIDER
175
150
350
183
wolf-gain-from-food
wolf-gain-from-food
0
1
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
175
115
350
148
newborn-energy
newborn-energy
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
0
45
175
78
initial-grass-density
initial-grass-density
0
1
0.35
0.05
1
NIL
HORIZONTAL

SWITCH
0
290
175
323
sheep-see-grass?
sheep-see-grass?
0
1
-1000

SWITCH
0
255
175
288
sheep-see-wolves?
sheep-see-wolves?
0
1
-1000

SWITCH
0
220
175
253
sheep-see-sheep?
sheep-see-sheep?
0
1
-1000

SWITCH
175
290
350
323
wolves-see-grass?
wolves-see-grass?
1
1
-1000

SWITCH
175
255
350
288
wolves-see-wolves?
wolves-see-wolves?
0
1
-1000

SWITCH
175
220
350
253
wolves-see-sheep?
wolves-see-sheep?
0
1
-1000

SLIDER
0
430
175
463
sheep-death-penalty
sheep-death-penalty
-50
0
-10.0
1
1
NIL
HORIZONTAL

SLIDER
175
430
350
463
wolf-death-penalty
wolf-death-penalty
-50
0
-10.0
1
1
NIL
HORIZONTAL

TEXTBOX
370
410
700
436
Efficiencies for entire run:
12
0.0
1

MONITOR
370
425
455
470
sheep
sheep-efficiency-weighted-sum / total-sheep-actions
3
1
11

MONITOR
540
425
625
470
wolves
wolf-efficiency-weighted-sum / total-wolf-actions
3
1
11

MONITOR
455
425
540
470
escape
sheep-escape-efficiency-weighted-sum / total-wolf-actions
4
1
11

PLOT
370
190
720
405
smoothed efficiency
NIL
NIL
0.0
1.0
0.5
1.0
true
true
"" ""
PENS
"sheep" 1.0 0 -13345367 true "" "plotxy ticks smoothed-val \"seff\" sheep-efficiency 6 0.03"
"wolves" 1.0 0 -2674135 true "" "plotxy ticks smoothed-val \"weff\" wolf-efficiency 6 0.03"
"escape" 1.0 0 -11221820 true "" "plotxy ticks smoothed-val \"escape\" sheep-escape-efficiency 6 0.03"

MONITOR
660
260
717
305
sheep
table:get smoothed-values \"seff-6\"
3
1
11

MONITOR
660
305
717
350
escape
table:get smoothed-values \"escape-6\"
3
1
11

MONITOR
660
350
717
395
wolves
table:get smoothed-values \"weff-6\"
3
1
11

@#$#@#$#@
## WHAT IS IT?

This model extends the Wolf Sheep Predation model by giving the wolves and sheep the ability to make decisions based on predictions the possible outcomes of their actions. To do so, they run simplified miniature versions of the Wolf Sheep Predation model that only consider their immediate surroudings using the LevelSpace extension.

## HOW IT WORKS

This model consists of two component models connected using LevelSpace. The primary model is based on the standard Wolf Sheep Predation model with several modifications to improve system robustness across a variety of agent behaviors.. Just as in the original model, the model consists of wolves, sheep, and ground patches, which may or may not have have grass. Each step, the wolves and sheep must choose a direction to move in: left, right, or forward. If a wolf finds a sheep, it eats the sheep, gaining energy based on the energy of the sheep. If a sheep finds grass, it eats the grass, gaining some energy. If the energy of a wolf or sheep exceeds a threshold, it reproduces, giving some of its energy to its child. Each step costs the wolves and sheep one energy; if they run out of energy, they die.

In order to decide which direction to move in, the wolves and sheep make use of the second model, called the cognitive model. The cognitive model uses a simplified form of the Wolf Sheep Predation rules: wolves still eat sheep and sheep still eat grass, but agents do not reproduce, nor does grass regrow. Instead, an agent in the primary model initializes the child model based on the locations of the grass and the other agents around them. It creates a represention of itself in the cognitive model based on its location and energy level. This representation is called the ego. The cognitive model will then perform multiple simulations of only a couple ticks each from that state. In those simulations, the agents all make random decisions. The cognitive model keeps track of the initial action the ego makes in each simulation and computes an expected reward for that action based on how much its energy ended up changing and whether or not it died. The agent in the primary model will then select the action that performed the best in the cognitive model.

In order to measure how the cognitive model impacts agent performance, the model measure "agent efficiency". To measure wolf and sheep food seeking efficiency, this measures the ratio of how much food the species ate versus how much we would expect them to eat if positioned randomly given current population densities.To measure how well sheep escape being eaten by wolves, it computes how many sheep survive versus how many we would expect to survive. This allows us to measure the performance of agents that mostly controls for the complex population dynamics of a predator prey system. See chapter 4 of Head (2024) for a detailed discussion of the design of these metrics.

## HOW TO USE IT

1. Adjust the slider parameters (see below), or use the default settings.
2. Press the SETUP button.
3. Press the GO button to begin the simulation.
4. Look at the monitors to see the current population sizes
5. Look at the POPULATIONS plot to watch the populations fluctuate over time
6. Look at the SMOOTHED EFFICIENCY plot to watch how the efficiency measurements settle.
7. Change either the wolf or sheep cognitive parameters; watch how the efficiency measurements change in response. 

Parameters:
INITIAL-NUMBER-SHEEP: The initial size of sheep population
INITIAL-NUMBER-WOLVES: The initial size of wolf population
INITIAL-GRASS-DENSITY: The ratio of patches that will be initially be covered in grass
GRASS-REGROWTH-TIME: How long it takes for grass to regrow once it is eaten.
NEWBORN-ENERGY: The ratio of a parent's energy given to a newborn child upon reproduction.
SHEEP-GAIN-FROM-FOOD: The amount of energy sheep get for every grass patch eaten
WOLF-GAIN-FROM-FOOD: The ratio of a sheep's energy the wolf gains when it eats that sheep.
SHEEP-THRESHOLD: The energy level at which sheep reproduce.
WOLF-THRESHOLD: The energy level at which the wolves reproduce.

Cognitive parameters: These parameters control the behavior of the cognitive model for the specified species; they behave equivalently for each species so are listed here only once:
<species>-SEE-SHEEP?: Whether the species will use sheep in its cognitive model.
<species>-SEE-WOLVES?: Whether the species will use wolves in its cognitive model.
<species>-SEE-GRASS?: Whether the species will use grass in its cognitive model.
<species>-VISION: The radius around an agent in which it will include other agents in its cognitive model.
<species>-SIM-N: The number of simulations the species will run in its cognitive model.
<species>-SIM-L: The number of ticks the species will run the simulations of its cognitive model for.
<species>-DEATH-PENALTY: How much to penalize death in the cognitive model (in units of energy lost).

As in the original Wolf Sheep Predation, the POPULATION graph and associated monitors show the population dynamics of the model.

The SMOOTHED EFFICIENCY graph and associated monitors show efficiency values to measure how well wolves find sheep, how well sheep find grass, and how well sheep escape wolves. Raw efficiency values vary greatly from tick to tick. For example, it is not uncommon for the wolves to not eat any sheep on a particular tick. Hence, the graph highlights trends in data that take place over around 100 ticks or more.

The EFFICIENCIES FOR ENTIRE RUN monitors show the overall efficiency values for all ticks since SETUP was last pressed.

## THINGS TO NOTICE

Try running the model with species only using a single simulation of a single tick. This means the agents are picking random actions since they have nothing to compare that single simulation with. Try slowly increasing the number of simulations a species uses. Notice how efficiency dramatically increases at first and then begins to taper off. How does the number of ticks a species uses affect this?

As sheep efficiency increases, notice that that sheep population decreases. Why would an increased ability to get food cause the population to lower? What happens to the wolf population?


## THINGS TO TRY

Try running the model with and without SHEEP-SEE-WOLVES?. What happens to wolf efficiency?

Run the model with SHEEP-SEE-SHEEP? and SHEEP-SEE-GRASS? enabled. Note the overall sheep efficiency. Now re-run the model with SHEEP-SEE-SHEEP? disabled. Note that overall sheep efficiency has decreased somewhat. Why would sheep being able to see other sheep improve their ability to get food?

Try the above with with SHEEP-SEE-GRASS? disabled. Why would SHEEP-SEE-SHEEP? improve the sheep's ability to get food even when they can't see grass? Note that, in the cognitive model, if an agent travels that the ego has not seen, it will randomly fill it in with grass or dirt.

Can you find any settings that result in one of the species going extinct?

## EXTENDING THE MODEL

Try expanding the different actions agents can take by modifying the WOLF-ACTIONS and SHEEP-ACTIONS variables. Here are some ideas:

- Add a larger variety of turn angles.
- Making a eating separate action so that agents have to stand still to eat.
- Add a new action for reproduction. This will require modifying the cognitive model so that agents receive some incentive to reproduce.

The cognitive model has all agents taking random actions. How might this be modified to let agents better predict the actions of other agents? One possible strategy would be to have the agents in the cognitive model use a cognitive model themselves for making decisions. However, this would likely make the model run quite slowly. Can you think of any other ideas? See chapter 7 of Head (2015) for one such idea.

Having agents make decisions by running many small simulations is a very general one. Can you try adapting this technique to another model?

## NETLOGO FEATURES

This model highlights a powerful use case of the LevelSpace extension: having agents use agent-based models to make predictions about their actions.

## RELATED MODELS

See the original Wolf Sheep Predation model for the basis of this model.

## CREDITS AND REFERENCES

Wilensky, U. & Reisman, K. (1998). Connected Science: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. International Journal of Complex Systems, M. 234, pp. 1 - 12. (The Wolf-Sheep-Predation model is a slightly extended version of the model described in the paper.)

Wilensky, U. & Reisman, K. (2006). Thinking like a Wolf, a Sheep or a Firefly: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. Cognition & Instruction, 24(2), pp. 171-209. http://ccl.northwestern.edu/papers/wolfsheep.pdf .

Wilensky, U., & Rand, W. (2015). An introduction to agent-based modeling: Modeling natural, social and engineered complex systems with NetLogo. Cambridge, MA: MIT Press.

Head, B. (2024) Agents Modeling Agents: The Design and Analysis of Multi-Level Agent-Based Models. PhD Thesis, Northwestern University. https://search.proquest.com/openview/0da8c22a8fa052247a5af1cdba4edaf9/1?pq-origsite=gscholar&cbl=18750&diss=y.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Head, B. & Wilensky, U. (2024).  NetLogo Wolf Sheep Predation - Micro-Sims model.  http://ccl.northwestern.edu/netlogo/models/WolfSheepPredationMicroSims.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2024 Bryan Head.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2000.

<!-- 1997 2000 -->
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
set model-version "sheep-wolves-grass"
set show-energy? false
setup
repeat 75 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="basic-sheep-vary-n" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-vary-n-l" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="9"/>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-vary-n-l-high-n" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="9"/>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-vision" first="0" step="1" last="10"/>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolf-vary-n-l" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="9"/>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolf-vary-n-l-big" repetitions="25" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>count grass</metric>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="20"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="19"/>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="versus-sheep" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? smart-sheep
or not any? random-sheep</exitCondition>
    <metric>count smart-sheep</metric>
    <metric>count smart-wolves</metric>
    <metric>count grass</metric>
    <metric>count random-sheep</metric>
    <metric>count random-wolves</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="versus-wolves" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? smart-wolves
or not any? random-wolves</exitCondition>
    <metric>count smart-sheep</metric>
    <metric>count smart-wolves</metric>
    <metric>count grass</metric>
    <metric>count random-sheep</metric>
    <metric>count random-wolves</metric>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="versus-sheep-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? smart-sheep
or not any? random-sheep</exitCondition>
    <metric>count smart-sheep</metric>
    <metric>count smart-wolves</metric>
    <metric>count grass</metric>
    <metric>count random-sheep</metric>
    <metric>count random-wolves</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="versus-wolves-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? smart-wolves
or not any? random-wolves</exitCondition>
    <metric>count smart-sheep</metric>
    <metric>count smart-wolves</metric>
    <metric>count grass</metric>
    <metric>count random-sheep</metric>
    <metric>count random-wolves</metric>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-efficiency" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count wolves</metric>
    <metric>count sheep</metric>
    <metric>count grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="20"/>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolf-efficiency" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count wolves</metric>
    <metric>count sheep</metric>
    <metric>count grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="20"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolf-efficiency-n" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count wolves</metric>
    <metric>count sheep</metric>
    <metric>count grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="50"/>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-efficiency-n" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count wolves</metric>
    <metric>count sheep</metric>
    <metric>count grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="50"/>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-sheep">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-smart-wolves">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cog-rates?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reward-discount">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolves-cross-fixed" repetitions="10" runMetricsEveryStep="true">
    <setup>set wolf-vision wolf-sim-l + 2
setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>not any? wolves</exitCondition>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-efficiency</metric>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ws-10x5x10x5" repetitions="1" runMetricsEveryStep="true">
    <setup>set sheep-vision sheep-sim-l + 2
set wolf-vision wolf-sim-l + 2
setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>not any? wolves</exitCondition>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="5"/>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="10"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="wolf-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ws-5x5x5x5" repetitions="1" runMetricsEveryStep="true">
    <setup>set sheep-vision sheep-sim-l + 2
set wolf-vision wolf-sim-l + 2
setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>not any? wolves</exitCondition>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="5"/>
    <steppedValueSet variable="sheep-sim-l" first="0" step="1" last="5"/>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="5"/>
    <steppedValueSet variable="wolf-sim-l" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="wolf-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-01-15-ws-10x5x10x5-1000" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="0" step="1" last="10"/>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="0" step="1" last="10"/>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-01-16-s-30x5-2000" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="sheep-sim-l" first="1" step="1" last="5"/>
  </experiment>
  <experiment name="2023-01-16-w-30x5-2000" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="wolf-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-01-20-s-n_3_6_9-l_3-w-n_3_9_30-l_5-perception-sweep" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="3"/>
      <value value="9"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-01-20-s-wu_0_6-n_12-l_3-w-wu_0_6-n_12-l_5-perception-sweep" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-01-s-30x5-2000-all-at-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;all-at-once&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="sheep-sim-l" first="1" step="1" last="5"/>
  </experiment>
  <experiment name="2023-04-01-w-30x5-2000-all-at-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;all-at-once&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="wolf-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-01-s-30x5-2000-sw-smart" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;sheep-wolves-smart&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="sheep-sim-l" first="1" step="1" last="5"/>
  </experiment>
  <experiment name="2023-04-01-w-30x5-2000-sw-smart" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;sheep-wolves-smart&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="wolf-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-02-s-n_3_6_9-l_3-w-n_3_9_30-l_5-perception-sweep-smart" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;sheep-wolves-smart&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="3"/>
      <value value="9"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-02-s-wu_0_6-n_12-l_3-w-wu_0_6-n_12-l_5-perception-sweep-smart" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;sheep-wolves-smart&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-02-s-wu_0_6-n_12-l_3-w-wu_0_6-n_12-l_3-perception-sweep-smart" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;sheep-wolves-smart&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-06-s-wu_0_6-n_12-l_3-w-wu_0_6-n_12-l_3-perception-sweep-all-at-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;all-at-once&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-ego-on-wu?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-06-s-30x5-2000-sw-all-at-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;all-at-once&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sheep-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="sheep-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-l">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-ego-on-wu?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-04-06-w-30x5-2000-sw-all-at-once" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scheduling">
      <value value="&quot;all-at-once&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-warmup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-n">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-sim-l">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-sim-warmup">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wolf-sim-n" first="1" step="1" last="30"/>
    <steppedValueSet variable="wolf-sim-l" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="track-ego-on-wu?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-penalty">
      <value value="-10"/>
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
