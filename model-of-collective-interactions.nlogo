globals
[
  speed-file-name
  time-step

  car-appear
  ticks-to-appear
  next-car?
  car-length

  ;OUTPUTS OF THE MODEL
  traffic-flow-model
  traffic-density-model

  number-of-accidents
  xcor-accidents

  eco-number-of-accidents
  conv-number-of-accidents

  eco-nearest-follow-dist-car-ahead
  mean-eco-nearest-follow-dist-car-ahead
  median-eco-nearest-follow-dist-car-ahead
  sd-eco-nearest-follow-dist-car-ahead

  conv-nearest-follow-dist-car-ahead
  mean-conv-nearest-follow-dist-car-ahead
  median-conv-nearest-follow-dist-car-ahead
  sd-conv-nearest-follow-dist-car-ahead

  eco-travel-time
  mean-eco-travel-time
  median-eco-travel-time
  sd-eco-travel-time
  sum-eco-travel-time

  conv-travel-time
  mean-conv-travel-time
  median-conv-travel-time
  sd-conv-travel-time
  sum-conv-travel-time

  eco-emission-co2
  mean-eco-emission-co2
  median-eco-emission-co2
  sd-eco-emission-co2
  sum-eco-emission-co2

  conv-emission-co2
  mean-conv-emission-co2
  median-conv-emission-co2
  sd-conv-emission-co2
  sum-conv-emission-co2

  ;SPECIFIC CONSUMPTION FACTORS
  b0
  b1
  b2
  b3
  b4
]

breed [eco-drivers eco-driver]
breed [conv-drivers conv-driver]

turtles-own
[
  ;SPEED
  speed                                                                                                         ; The current speed of the car.
  speed-before                                                                                                  ; The before speed of car.
  speed-desired                                                                                                 ; The maximum speed the car can reach.
  speed-min                                                                                                     ; The minimum speed the car can reach.
  acceleration                                                                                                  ; The acceleration rate of car.
  deceleration                                                                                                  ; The deceleration rate of car.

  ;REACTION TIME
  reaction-time                                                                                                 ; Value in seconds to driver react after take a decision.
  ticks-to-react                                                                                                ; Counter of ticks in the simulation to driver reacts.
  decision                                                                                                      ; After the driver asks: "Is there a vehicle within the following distance ahead?", if YES take a decision, if NO take another. This variable records the current decision.
  decision-before                                                                                               ; Record the previous decision and compare it with the current one. If the decision changes, the new decision will take some time to react.
  status                                                                                                        ; The current status of the driver. Whether "accelerating", "decelerating", "stopped" or "cruise".
  reacting?                                                                                                     ; Has the driver started to react? if TRUE takes action, if FALSE no.

  ;FOLLOW DISTANCE
  following-dist                                                                                                ; The desired following distance in seconds to the car ahead.
  dist                                                                                                          ; The desired following distance in meters to the car ahead. Calculated using current speed.
  car-ahead                                                                                                     ; The closest car ahead of driver.
  dist-car-ahead                                                                                                ; The current distance of car-ahead.
  all-dist-car-ahead                                                                                            ; List with all distances of car-ahead during simulation.
  min-dist-car-ahead

  ;DISTANCE TRAVELLED
  distance-travelled
  distance-travelled-km

  ;TRAVELLING TIME
  ticks-traveling-time                                                                                          ; The traveling time of car in ticks.
  hmss-traveling-time                                                                                           ; The traveling time of car in hours, minutes, seconds and miliseconds.

  ;CONSUMPTION AND EMISSIONS
  consumption                                                                                                   ; The total fuel consumption by car.
  instantaneous-consumption                                                                                     ; The instantaneous fuel consumption by car.
  current-emission-co2                                                                                          ; The current emission of CO2 by car.
  emission-co2                                                                                                  ; The total emission of CO2 by car.
]

to setup
  clear-all
  if seed-on [random-seed seed]
  draw-road

  set-default-shape turtles "car"

  set xcor-accidents []
  set eco-nearest-follow-dist-car-ahead []
  set conv-nearest-follow-dist-car-ahead []
  set eco-travel-time []
  set conv-travel-time []
  set eco-emission-co2 []
  set conv-emission-co2 []

  set time-step 0.1
  set car-length 0.15
  set ticks-to-appear 1
  set b0 2.5E-4                                                                                                 ; 0.00025 l/s
  set b1 2.4525E-5                                                                                              ; 0.000024525 l/m
  set b2 0                                                                                                      ;
  set b3 3.25E-8                                                                                                ; 0.0000000325 ls^2/m^3
  set b4 1.25E-4


  if record-speed-desired?
  [
  set speed-file-name user-new-file
  if file-exists? speed-file-name [file-delete speed-file-name]
  ]

  reset-ticks
end




















to go
  next-car-appear

  ask turtles
  [
    set decision-before decision                                                                                ; Before starts all analysis and procedures, record the decision before.
    set speed-before speed                                                                                      ; Before starts all analysis and procedures, record the speed before.

    following-distance-and-car-ahead-process                                                                    ; Estimates following distance in meters and the car ahead.
    decision-making-process                                                                                     ; This procedure is based in High-Level Algorithm developed. The driver decides if accelerate, decelerate, or keep speed.

    fd speed * time-step                                                                                        ; The driver moves forward, and their moves is based in their current speed. based in the formula 's = s0 + v.Δt'.

    count-distance                                                                                              ; Procedure to calculate the distance travelled by cars.
    crashing-cars-process                                                                                       ; Procedure to crash between cars.
    calc-consumption-and-emission                                                                               ; Procedure to count the consumption by cars.
    count-time                                                                                                  ; Procedure to count the travelling time by cars.
    end-of-road-and-outputs                                                                                     ; Procedure in the end of highway.
  ]

  output-update
  if ticks >= 216000 [stop]
  tick
end

;===== TURTLES PROCEDURES ==================================================================================================================================================================================================
; THE PROCESS OF CALCULATING THE FOLLOWING DISTANCE AND DISTANCE OF THE CAR AHEAD ==========================================================================================================================================
to following-distance-and-car-ahead-process
  set dist precision (speed * following-dist) 4                                                                 ; Calculating the following distance in meters, using the formula 's = s0 + v.Δt'.
  if dist < car-length + 0.05 [set dist car-length + 0.05]                                                      ; Setting the minimum distance of the following distance, that is a car length and one meter away.
  set car-ahead min-one-of other turtles in-cone dist 15 [distance myself]                                      ; Defining the car ahead the closest car in following distance of the driver.
end

; THE DECISION MAKING PROCESS ===============================================================================================================================================================================================
to decision-making-process
                                                                                                                ; COMFORTABLE FOLLOWING
  ifelse car-ahead != nobody                                                                                    ; Verifying if there is a car inside of my following distance to set 'decision' and take action.
  [
    set decision "Y"
    if speed > [speed] of car-ahead                                                                             ; The driver checks that the car ahead inside of the following distance is slower or equal than yourself.
    [
      reaction                                                                                                  ; If the car ahead is slower or equal, then the driver spends time to react to take some action.

      if reacting? = FALSE                                                                                      ; If the driver didn't spend all the time needed to react, their status is just 'reacting' and your velocity is constant.
      [set status "reacting"]

      if reacting? = TRUE                                                                                       ; If the driver spent all the time needed to react, then the driver starts to decelerate and change their status to 'decelerating'.
      [
        set status "decelerating"
        slow-down
      ]
    ]
  ]

                                                                                                                ; FREE FLOW
  [
    set decision "N"                                                                                            ; If there is no car in the following distance, set 'decision',
    ifelse speed >= speed-desired                                                                               ; and check your own speed to see if it is equal to or greater than the maximum allowed.
    [
      set speed speed-desired                                                                                   ; If your speed is equal to or greater, then the driver keeps your speed and set your status to 'cruise'.
      set status "cruise"
    ]

    [
      reaction                                                                                                  ; If your speed is not equal or greater, then the driver starts to react to accelerate your car.

      if reacting? = FALSE                                                                                      ; If the driver didn't spend all the time needed to react, their status is just 'reacting' and your velocity is constant.
      [set status "reacting"]



      if reacting? = TRUE                                                                                       ; If the driver spent all the time needed to react, then the driver starts to accelerate to reach the maximum speed and change their status to 'accelerating'.
      [
        set status "accelerating"
        speed-up
      ]
    ]
  ]

  if speed <= speed-min                                                                                         ; If after the driver decides to decelerate, and your own velocity is lower than zero, then the driver stops and changes their 'status'.
  [
    set speed speed-min
    set status "stopped"
  ]

                                                                                                                ; CLOSE FOLLOWING
;  ifelse dist-car-ahead < (speed * 1.0) and car-ahead != nobody and decision-before != "N"                      ; If there is a car ahead and the following distance is lower than 1.0 second, braking at maximum.
;  [set deceleration 0.2778]
;  [                                                                                                             ; Otherwise, brake smoothly and comfortably.
;    (
;      ifelse
;      breed = eco-drivers
;      [set deceleration eco-deceleration]
;      breed = conv-drivers
;      [set deceleration conv-deceleration]
;    )
;  ]
end

; THE PROCESS OF DISTANCE TRAVELLED =========================================================================================================================================================================================
to count-distance
  set distance-travelled distance-travelled + (( speed * time-step ) * 100 / 3.6 )
  set distance-travelled-km distance-travelled * 0.001
end

; THE PROCESS OF CRASHING CARS ==============================================================================================================================================================================================
to crashing-cars-process
  if car-ahead != nobody                                                                                        ; Check if there is a car ahead.
  [
    set dist-car-ahead precision ((distance car-ahead) - car-length) 4                                          ; Set the distance value in patches of the car ahead of the driver.
    set all-dist-car-ahead lput dist-car-ahead all-dist-car-ahead                                               ; Creates a list with all distances throughout the simulation from the car ahead.

    if dist-car-ahead <= 0                                                                                      ; Procedure for crashing cars: The driver checks whether the distance to the car in front is less than or equal to half the length of his car plus half the length of the car in front.
    [
      ;show (word "The Car "who " ("breed")" " hit the Car " [who] of car-ahead " (" [breed] of car-ahead ")" " at " precision xcor 4)   ; Print "Who hit who?"
      set xcor-accidents lput xcor xcor-accidents
      ask car-ahead [die]                                                                                       ; Remove the car in front that was hit.

      set number-of-accidents number-of-accidents + 1                                                           ; Counter of number of accidents.
      ifelse breed = eco-drivers
      [set eco-number-of-accidents eco-number-of-accidents + 1]
      [set conv-number-of-accidents conv-number-of-accidents + 1]
      die                                                                                                       ; Remove the crashed car.
    ]
  ]
end

; THE FUEL CONSUMPTION PROCESS ======================================================================================================================================================================================================
to calc-consumption-and-emission
  let vn speed * 100 / 3.6                                                                                      ; Speed of the car in m/s.
  let an (((speed - speed-before) * 100 / 3.6) / time-step)                                                     ; Accelerate of the car in m/s².

  set instantaneous-consumption (b0 + (b1 * vn) + (b2 * (vn ^ 2)) + (b3 * (vn ^ 3)) + (b4 * vn * an)) * time-step ; Liters/Δt.

  if instantaneous-consumption < 0
  [set instantaneous-consumption 0]

  set consumption consumption + instantaneous-consumption                                                       ; Liters.
  set current-emission-co2 (instantaneous-consumption * 2.39)                                                   ; 2.39kg of CO₂ per liter consumed.
  set emission-co2 emission-co2 + current-emission-co2                                                          ; kg of CO₂ emitted.
end

; THE PROCESS OF COUNT TRAVELLING TIME ======================================================================================================================================================================================
to count-time
  set ticks-traveling-time ticks-traveling-time + 1
  let total-seconds ticks-traveling-time * time-step
  let hours floor (total-seconds / 3600)
  let remaining-seconds total-seconds - (hours * 3600)
  let minutes floor (remaining-seconds / 60)
  let seconds floor remaining-seconds - (minutes * 60)
  let milisec floor (remaining-seconds * 1000 mod 1000)
  set hmss-traveling-time (word hours ":" minutes ":" seconds "." milisec)
end

; THE ACCELERATION PROCESS =================================================================================================================================================================================================
to speed-up
  set speed precision (speed + acceleration * time-step) 4                                                      ; Based on formula 'v = v0 + a.Δt'.
end

; THE DECELERATION PROCESS =================================================================================================================================================================================================
to slow-down
  set speed precision (speed - deceleration * time-step) 4                                                      ; Based on formula 'v = v0 + (-a).Δt'.
end

; THE REACTION TIME PROCESS ================================================================================================================================================================================================
to reaction
  ifelse ticks-to-react >= (reaction-time / time-step)                                                          ; Checks whether the time taken to react was all the time needed, comparing simulation ticks with seconds.
  [set reacting? TRUE]                                                                                          ; If it was, set reacting true.
  [                                                                                                             ; If wasn't, set reacting false and count ticks to react.
    set reacting? FALSE
    set ticks-to-react ticks-to-react + 1
  ]

  if decision != decision-before                                                                                ; If the driver changes your decision between accelerate and decelerate, he needs react again.
  [
    set ticks-to-react 0
    set reacting? FALSE
  ]
end

to end-of-road-and-outputs
  if xcor >= (max-pxcor - 1)                                                                                    ; The actions to take in the end of highway.
  [
    ifelse all-dist-car-ahead != []
    [
      let min-dist-car-ahead-seconds precision(((min all-dist-car-ahead) * 100 / 3.6) / (speed * 100 / 3.6))4   ; [following distance = distance-of-car-ahead / speed] -> following distance of car-ahead in seconds.
      set min-dist-car-ahead  min-dist-car-ahead-seconds
    ]
    [set min-dist-car-ahead "null"]

    (
      ifelse
      breed = eco-drivers
      [
        set eco-nearest-follow-dist-car-ahead lput min-dist-car-ahead eco-nearest-follow-dist-car-ahead
        set eco-travel-time lput ticks-traveling-time eco-travel-time
        set eco-emission-co2 lput emission-co2 eco-emission-co2
      ]
      breed = conv-drivers
      [
        set conv-nearest-follow-dist-car-ahead lput min-dist-car-ahead conv-nearest-follow-dist-car-ahead
        set conv-travel-time lput ticks-traveling-time conv-travel-time
        set conv-emission-co2 lput emission-co2 conv-emission-co2
      ]
    )
    die
  ]
end
;===== END OF TURTLES PROCEDURES ===========================================================================================================================================================================================

to output-update
  let eco-nearest-follow-dist-car-ahead-remove-null filter [x -> x != "null"] eco-nearest-follow-dist-car-ahead
  let conv-nearest-follow-dist-car-ahead-remove-null filter [x -> x != "null"] conv-nearest-follow-dist-car-ahead

  if eco-nearest-follow-dist-car-ahead-remove-null != [] and length eco-nearest-follow-dist-car-ahead-remove-null >= 2
  [
    set mean-eco-nearest-follow-dist-car-ahead (mean eco-nearest-follow-dist-car-ahead)
    set median-eco-nearest-follow-dist-car-ahead (median eco-nearest-follow-dist-car-ahead)
    set sd-eco-nearest-follow-dist-car-ahead (standard-deviation eco-nearest-follow-dist-car-ahead)
  ]

  if conv-nearest-follow-dist-car-ahead-remove-null != [] and length conv-nearest-follow-dist-car-ahead-remove-null >= 2
  [
    set mean-conv-nearest-follow-dist-car-ahead (mean conv-nearest-follow-dist-car-ahead)
    set median-conv-nearest-follow-dist-car-ahead (median conv-nearest-follow-dist-car-ahead)
    set sd-conv-nearest-follow-dist-car-ahead (standard-deviation conv-nearest-follow-dist-car-ahead)
  ]

  if eco-travel-time != [] and length eco-travel-time >= 2
  [
    set mean-eco-travel-time (mean eco-travel-time)
    set median-eco-travel-time (median eco-travel-time)
    set sd-eco-travel-time (standard-deviation eco-travel-time)
    set sum-eco-travel-time (sum eco-travel-time)
  ]

  if conv-travel-time != [] and length conv-travel-time >= 2
  [
    set mean-conv-travel-time (mean conv-travel-time)
    set median-conv-travel-time (median conv-travel-time)
    set sd-conv-travel-time (standard-deviation conv-travel-time)
    set sum-conv-travel-time (sum conv-travel-time)
  ]

  if eco-emission-co2 != [] and length eco-emission-co2 >= 2
  [
    set mean-eco-emission-co2 (mean eco-emission-co2)
    set median-eco-emission-co2 (median eco-emission-co2)
    set sd-eco-emission-co2 (standard-deviation eco-emission-co2)
    set sum-eco-emission-co2 (sum eco-emission-co2)
  ]

  if conv-emission-co2 != [] and length conv-emission-co2 >= 2
  [
    set mean-conv-emission-co2 (mean conv-emission-co2)
    set median-conv-emission-co2 (median conv-emission-co2)
    set sd-conv-emission-co2 (standard-deviation conv-emission-co2)
    set sum-conv-emission-co2 (sum conv-emission-co2)
  ]
  set traffic-flow-model flow
  set traffic-density-model traffic-density
end

; CARS EMERGING IN THE ROAD
to next-car-appear
  ifelse ticks-to-appear >= (car-appear / time-step)
  [
    create-cars
    set ticks-to-appear 1
    set next-car? TRUE
  ]
  [
    set ticks-to-appear ticks-to-appear + 1
    set next-car? FALSE
  ]

  if next-car? = TRUE
  [
    set car-appear seconds-per-car
  ]
end

to create-cars
  ask patch 0 2
  [
    if not any? other turtles-here
    [
      sprout 1
      [
        setxy 0 2
        set heading 90

        let speed-desired-max-range (speed-desired-mean + 3 * speed-desired-sd)
        let speed-desired-min-range (speed-desired-mean - 3 * speed-desired-sd)

        set speed-desired precision ((random-normal-in-bounds speed-desired-mean speed-desired-sd speed-desired-min-range speed-desired-max-range) * 3.6 / 100) 2
        set speed speed-desired





        if record-speed-desired?
        [
          file-open speed-file-name
          file-print (word who ","speed-desired ";")
          file-close
        ]


        if use-recorded-speed-desired?
        [
          file-open input-speed-file-name
          while [not file-at-end?]
          [
            let line file-read-line

            let start-id position "" line
            let end-id position "," line
            let turtle-str (substring line start-id end-id)
            let extract-turtle-id read-from-string turtle-str

            let start-dspeed position "," line
            let line-end position ";" line

            let speed-str substring line (start-dspeed + 1) line-end
            let extract-speed-desired read-from-string speed-str

            if extract-turtle-id = who
            [
              set speed-desired extract-speed-desired
              set speed speed-desired
            ]
          ]
          file-close
        ]

        set reaction-time seconds-to-react
        set reacting? FALSE
        set all-dist-car-ahead []

        let proportion (count eco-drivers / count turtles)
        ifelse proportion < (eco-drivers-percent / 100)
        [
          set breed eco-drivers
          set color green
          set acceleration (eco-acceleration * 3.6 / 100)
          set deceleration (eco-deceleration * 3.6 / 100)
          set following-dist eco-following-dist
        ]
        [
          set breed conv-drivers
          set color red
          set acceleration (conv-acceleration * 3.6 / 100)
          set deceleration (conv-deceleration * 3.6 / 100)
          set following-dist conv-following-dist
        ]
      ]
    ]
  ]
end

; REPORTS ==================================================================================================================================================================================================================
to-report simu-time
  let total-seconds ticks * time-step
  let hours floor (total-seconds / 3600)
  let remaining-seconds total-seconds - (hours * 3600)
  let minutes floor (remaining-seconds / 60)
  let seconds floor remaining-seconds - (minutes * 60)
  let milisec floor (remaining-seconds * 1000 mod 1000)
  report (word hours "h " minutes "m " seconds "s " milisec "ms")
end

to-report seconds-per-car
  let sec ((1 / flow) * 3600)
  report sec
end

to-report traffic-density
  let rho (precision(((count eco-drivers) + (count conv-drivers)) / 21.7)2)
  report rho
end

to-report hmss-mean-conv-travel-time
  let total-seconds mean-conv-travel-time * time-step
  let hours floor (total-seconds / 3600)
  let remaining-seconds total-seconds - (hours * 3600)
  let minutes floor (remaining-seconds / 60)
  let seconds floor remaining-seconds - (minutes * 60)
  let milisec floor (remaining-seconds * 1000 mod 1000)
  report (word hours "h " minutes "m " seconds "s " milisec "ms")
end

to-report hmss-mean-eco-travel-time
  let total-seconds mean-eco-travel-time * time-step
  let hours floor (total-seconds / 3600)
  let remaining-seconds total-seconds - (hours * 3600)
  let minutes floor (remaining-seconds / 60)
  let seconds floor remaining-seconds - (minutes * 60)
  let milisec floor (remaining-seconds * 1000 mod 1000)
  report (word hours "h " minutes "m " seconds "s " milisec "ms")
end

to-report random-normal-in-bounds [mid dev mmin mmax]
  let result random-normal mid dev
  if result < mmin or result > mmax
    [ report random-normal-in-bounds mid dev mmin mmax ]
  report result
end

to standard-setup
  set eco-drivers-percent 0
  set flow 1000
  set seconds-to-react 0.68
  set eco-acceleration 1.00
  set conv-acceleration 1.50
  set eco-deceleration 3.00
  set conv-deceleration 5.00
  set eco-following-dist 2.84
  set conv-following-dist 1.57
  set speed-desired-mean 27.7778
  set speed-desired-sd 0.93
end

to draw-road
  ask patches [
    set pcolor green - random-float 0.5
  ]

  ask patches with [ pycor >= 1 and pycor <= 3] [
    set pcolor grey - 2.5 + random-float 0.2
  ]

  draw-road-lines
end

to draw-road-lines
  draw-line 0.5 grey 0
  draw-line 2.5 grey 0
end

to draw-line [ y line-color gap ]
  create-turtles 1 [
    setxy (min-pxcor - 0.5) y
    set size 0.1
    set pen-size 0.1
    hide-turtle
    set color line-color
    set heading 90
    repeat world-width [
      pen-up
      forward gap
      pen-down
      forward (1 - gap)
    ]
    die
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
5
75
913
89
-1
-1
1.151
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
781
0
4
0
0
1
ticks
30.0

BUTTON
70
98
133
131
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
6
98
69
131
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
5
175
195
208
eco-drivers-percent
eco-drivers-percent
0
100
50.0
5
1
%
HORIZONTAL

SLIDER
389
175
579
208
eco-acceleration
eco-acceleration
0
2.2769
1.0
0.01
1
m/s²
HORIZONTAL

SLIDER
389
262
579
295
eco-deceleration
eco-deceleration
0
7.7173
2.92
0.01
1
m/s²
HORIZONTAL

PLOT
1547
827
1937
1023
Speeds (km/h)
Time
Speed
0.0
10.0
20.0
31.0
true
true
"" ""
PENS
"Max" 1.0 0 -3844592 true "" "if ticks > 0 and count turtles > 0 \n[plot (max [speed] of turtles) * 100]"
"Min" 1.0 0 -13345367 true "" "if ticks > 0 and count turtles > 0 \n[plot (min [speed] of turtles) * 100]"
"Mean" 1.0 0 -7500403 true "" "if ticks > 0 and count turtles > 0 \n[plot (mean [speed] of turtles) * 100]"

MONITOR
1547
194
1672
239
NIL
simu-time
0
1
11

MONITOR
1787
259
1882
304
eco-drivers
count eco-drivers
0
1
11

MONITOR
1547
259
1642
304
conv-drivers
count conv-drivers
0
1
11

TEXTBOX
7
10
466
63
IMPACT OF INDIVIDUAL ECO-DRIVER BEHAVIOUR ON COLLECTIVE TRAFFIC DYNAMICS
20
0.0
1

TEXTBOX
745
17
915
71
Developed by Thiago M. Cypreste\nAeronautics Institute of Technology\nMaster's Thesis - 2024\nAll rights reserved
10
0.0
1

SWITCH
5
310
209
343
seed-on
seed-on
1
1
-1000

TEXTBOX
5
135
70
153
INPUTS
18
0.0
1

TEXTBOX
581
159
683
178
Following Distance
10
0.0
1

TEXTBOX
198
160
288
179
Reaction Time
10
0.0
1

TEXTBOX
5
160
137
178
Traffic Flow
10
0.0
1

TEXTBOX
389
159
487
179
Acceleration
10
0.0
1

TEXTBOX
389
247
479
265
Deceleration
10
0.0
1

SLIDER
389
209
579
242
conv-acceleration
conv-acceleration
0
2.2769
1.5
0.01
1
m/s²
HORIZONTAL

SLIDER
389
297
579
330
conv-deceleration
conv-deceleration
0
7.7173
4.98
0.01
1
m/s²
HORIZONTAL

INPUTBOX
773
175
888
235
speed-desired-mean
27.7778
1
0
Number

INPUTBOX
773
234
888
294
speed-desired-sd
0.93
1
0
Number

TEXTBOX
774
160
884
186
Desired speed (m/s)
10
0.0
1

TEXTBOX
1552
169
1702
190
OUTPUTS
18
0.0
1

SLIDER
198
175
388
208
seconds-to-react
seconds-to-react
0
1
0.68
0.01
1
s
HORIZONTAL

SLIDER
5
210
195
243
flow
flow
0
2000
1000.0
50
1
car/hour
HORIZONTAL

SLIDER
581
175
771
208
eco-following-dist
eco-following-dist
0
5
2.84
0.01
1
s
HORIZONTAL

SLIDER
581
209
771
242
conv-following-dist
conv-following-dist
0
5
1.56
0.01
1
s
HORIZONTAL

MONITOR
1547
639
1787
684
NIL
mean-conv-emission-co2
17
1
11

MONITOR
1547
449
1787
494
NIL
mean-conv-travel-time
17
1
11

MONITOR
1787
449
2027
494
NIL
mean-eco-travel-time
17
1
11

SWITCH
5
345
209
378
record-speed-desired?
record-speed-desired?
1
1
-1000

SWITCH
5
380
208
413
use-recorded-speed-desired?
use-recorded-speed-desired?
1
1
-1000

INPUTBOX
5
415
208
475
input-speed-file-name
speeds-of-turtles-1.5sd.txt
1
0
String

TEXTBOX
5
290
53
314
Tools
18
0.0
1

MONITOR
1675
194
1822
239
traffic density (cars/km)
traffic-density
17
1
11

MONITOR
1970
192
2192
237
mean speed
(mean ([speed] of turtles) * 100 / 3.6)
4
1
11

MONITOR
1825
194
1952
239
removed cars
number-of-accidents * 2
17
1
11

MONITOR
1547
304
1786
349
NIL
mean-conv-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1547
349
1786
394
NIL
median-conv-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1547
394
1786
439
NIL
sd-conv-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1547
494
1787
539
NIL
median-conv-travel-time
17
1
11

MONITOR
1547
539
1787
584
NIL
sd-conv-travel-time
17
1
11

MONITOR
1547
684
1787
729
NIL
median-conv-emission-co2
17
1
11

MONITOR
1547
729
1787
774
NIL
sd-conv-emission-co2
17
1
11

MONITOR
1547
774
1787
819
NIL
sum-conv-emission-co2
17
1
11

MONITOR
1787
304
2026
349
NIL
mean-eco-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1787
349
2026
394
NIL
median-eco-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1787
394
2026
439
NIL
sd-eco-nearest-follow-dist-car-ahead
17
1
11

MONITOR
1787
494
2027
539
NIL
median-eco-travel-time
17
1
11

MONITOR
1787
539
2027
584
NIL
sd-eco-travel-time
17
1
11

MONITOR
1787
639
2025
684
NIL
mean-eco-emission-co2
17
1
11

MONITOR
1787
684
2025
729
NIL
median-eco-emission-co2
17
1
11

MONITOR
1787
729
2025
774
NIL
sd-eco-emission-co2
17
1
11

MONITOR
1787
774
2025
819
NIL
sum-eco-emission-co2
17
1
11

MONITOR
1787
584
2027
629
NIL
sum-eco-travel-time
17
1
11

MONITOR
1547
584
1787
629
NIL
sum-conv-travel-time
17
1
11

BUTTON
270
98
357
132
NIL
standard-setup
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
5
244
195
289
NIL
seconds-per-car
17
1
11

INPUTBOX
213
310
273
370
seed
12345.0
1
0
Number

BUTTON
135
98
265
131
go until next car appear
repeat (seconds-per-car / time-step) [go]
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
# TRAFFIC BASIC

## Inputs:
- Fluxo
- Aceleração
- Desaceleração
- Tempo de Reação
- Distância de Seguimento

## Mudanças:
**Volume:** Diferente do TRAFFIC BASIC, onde os veículos são criados no início através do processo TO SETUP ao longo da rodovia de forma aleatória, os veículos serão criados um a um durante a simulação no início da rodovia e eliminados no fim dela, sendo sua criação feita em certos ticks.
Busca-se manter as mesmas propriedades sistemicas de fluxo e de densidade na rodovia, alterando apenas a forma no qual os veículos serão originados.


**Aceleração:** A taxa de aceleração atribuída no TRAFFIC BASIC é uma taxa de 'patches/ticks' e não há uma relação descrita no modelo com 'm/s²'. A velocidade do veículo também é relacionada a 'patches/ticks', com velocidade máxima sendo a de 1 patch/tick.  
A relação de ticks para segundos é dada da seguinte forma: cada tick equivale a 0.1 segundos, portanto 1 segundo na simulação equivale a 10 ticks.
A relação de patches para metros é dada da seguinte forma: cada patch equivale a 5 metros, tamanho do comprimento de um veículo na simulação.
Portanto, para uma taxa de 0.0045 patches/tick, essa taxa é 0.225 m/s². pois:
0.0045 * 5 / 0.1 = 0.225.
Pesquisando a aceleração de 0 a 100km/h do veículo Renault Logan Expression 1.6 8v, encontrei que ele atinge essa velocidade em 12.2 segundos. Como 100km/h é igual a 27.78m/s, a aceleração é 2.278m/s². Entretanto deve-se observar que a aceleração condiz com o desempenho máximo do veículo e não necessariamente o comportamento do condutor em condições normais. Também é necessário identificar os padrões distintos entre eco-condutores e condutores convencionais para aceleração.

**Desaceleração:** No código do TRAFFIC BASIC, a desaceleração é computada do seguinte modo: O veículo que está se aproximando do veículo da frente, iguala a sua velocidade a velocidade do veículo e reduz uma pequena taxa de desaceleração desta velocidade. 
Como neste procedimento nunca ocorre uma colisão e o veículo, optamos por alterar o código e apenas reduzir a velocidade através da desaceleração.
A frenagem é definida por:

_D = V² / 250μ_

onde:
D = Distância (m)
V = Velocidade (km/h)
u = Coeficiente de atrito

Um veículo a 100km/h por exemplo, leva 50 metros, ou 10 patches, para parar completamente.
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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="conv-acceleration">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="car-mean">
      <value value="3.5398"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eco-deceleration">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="car-sd">
      <value value="0.831"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="speed-desired-sd">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eco-fd-mean">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conv-deceleration">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conv-fd-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export-accidents-outputs">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eco-acceleration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export-outputs">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eco-fd-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eco-drivers-percent">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seconds-to-react">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="speed-desired-mean">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conv-fd-mean">
      <value value="1.5"/>
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
