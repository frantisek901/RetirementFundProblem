;; This is code for the basic model for solving
;; Spring Complexity Challenge 2018
;;
;; František 2018/04/25 Created
;; František 2018/05/19 Last edited
;;
;; REMARKS: This is kick-off model - Two FDs, one stright conservative, the second agressive with risk ballancing,
;; REMARKS: the PLs have to be IPLs, they have no choice to be PIs, and IPLs only could switch their FD.
;;
;; NEXT DEVELOPEMENT: CLEAN THE CODE!!!
;;
;; Forbidden variables because of Santa Fe Tournament code:
;; entry?, wealth, my-pool, myp myc, my-strategy, lowp, highp, lown, highn, current-lowpay, current-highpay, current-lown, current-highn
;;

breed [players player]
breed [funds fund]

players-own [
  id      ;; Differs from WHO in case there are some Funds, used mainly for counting exact position on pool.
  myPool  ;; Codes the pool where the Player is.
  myFund  ;; Codes the Fund the Player is incorporated in, in case the Player is independent, variable is set to -1.
  indie?  ;; Codes whether Player is PI (TRUE) or IPL (FALSE)
  moved?  ;; Codes whether Player has moved recent step (TRUE) or not (FALSE).
  wallet  ;; Stores sum of pay-offs in the game so far.
  stayed  ;; Counter, indicates how many steps the respective PL (PI/IPL) is staying at the pool.
  treaty  ;; Counter, indicates how many steps the respective IPL is a part of the FD
  inspFD  ;; Parameter - how many last dividends IPL inspects to decide which FD to choose
  rule    ;; Codes which rule the PL uses for choosing FD
  payoff0 ;; List with dividends received from FD 0
  payoff1 ;; List with dividends received from FD 1
  pChoiceFD0  ;; The probability the IPL chooses the FD 0 through the learning algorithm
]

funds-own [
  treasury  ;; Stores sum of pay-offs of incorporated Players - typically, the Treasury is evenly distributed to all incorporated Players at the end of the step.
  assigned  ;; Stores the agenset of IPLs.
  myStable  ;; Stores a number of IPLs sent to Stable pool
  myLow     ;; Stores a number of IPLs sent to Low pool
  myHigh    ;; Stores a number of IPLs sent to High pool
  iStable   ;; Stores an agentset of IPLs which should be send to Stable pool
  iLow      ;; Stores an agentset of IPLs which should be send to Low pool
  iHigh     ;; Stores an agentset of IPLs which should be send to High pool
  dividend  ;; Stores the size of pay-off paid at the end of step to every assigned IPL.
  diviList  ;; Records the dividends since the start of the game
  entering  ;; Stores the value of entering fee.
  conserve  ;; Codes how conservative the FD is
  aversive  ;; Codes how risk-aversive the FD is
]

globals [
  stablePayoff  ;; Codes updated pay-off being paid in recent step to the Players on the Stable pool.
  lowPayoff     ;; Codes updated pay-off being paid in recent step to the Players on the Low pool.
  highPayoff    ;; Codes updated pay-off being paid in recent step to the Players on the High pool.
]

to setup
  ;; For the tournament ballance we need omitt simulations with TRule1 > TRule2
  if tRule1 > tRule2 [stop]

  ;; clear the world
  clear-all
  random-seed ifelse-value (randomSeed?) [RS][200000000 - random 100000000]

  ;; set up Pools, Funds, Players
  set-globals
  set-pools
  set-funds
  set-players

  ;; start ticks
  reset-ticks

  ;; do the first step inside setup, because the first round has the exception (not paying Tau)
  first-step
end

to first-step
  ;; choose of being PI/IPL and then (in case of IPL) fund
  if any? funds [ask players [choose-fund]]

  ;; FDs assign their IPLs
  if any? funds [ask funds [assign-players]]

  ;; PIs choose pool / FDs sent their IPLs
  ask players with [indie?] [choose-pool]
  if any? funds [ask funds [send-players]]

  ;; move to pool without paying TAU
  ask players [move-to-pool]

  ;; get paid-off
  update-pay-offs
  ask players [get-paid-off]
  ask funds [pay-dividend-off]

  ;; Finish the step 1
  tick

  ;; Because of technical reasons STAYED can't be initialized to 1 but 999,
  ;; so after the whole first step the value is corrected
  ask players [set stayed 1]
end


to go
  ;; Checking whether to STOP model
  ;; For the tournament ballance we need omitt simulations with TRule1 > TRule2
  if ticks = simulationStop or tRule1 > tRule2 [stop]

  ;; IPLs and PIs update counter how long they stayed at their pools - counter indicates which step has been just started
  ask players [set stayed (stayed + 1)]

  ;; PLs choose being PI/IPL and then (in case of IPL) fund
  if any? funds [ask players [choose-fund]]

  ;; FDs assign their IPLs
  if any? funds [ask funds [assign-players]]

  ;; PIs choose pool / FDs sent their IPLs
  ask players with [indie?] [choose-pool]
  if any? funds [ask funds [send-players]]

  ;; PLs move to pool and pay TAU, in case of chosing different pool
  ask players [move-to-pool]

  ;; get paid-off
  update-pay-offs
  ask players [get-paid-off]
  ask funds [pay-dividend-off]

  ;; finish step
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                          ;;;;;
;;;;;  GO PROCEDURES (+ the first step procedures)                                             ;;;;;
;;;;;                                                                                          ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to choose-fund
  ;; For the reasons of paying enteringFee we have to record assignment to fund before deciding/choosing
  let before myFund

  ;; Choose being PI/IPL - only PIs and IPLs with Treaty > T could choose (because IPLs with Treaty <= T could not leave the FDs)
  if indie? or treaty > T [set indie? beingPI?]

  ;; PLs choose their fund
  set myFund ifelse-value (indie?) [-1] [ifelse-value (treaty > T) [chosenFund][myFund]]

  ;; updating Treaty
  set treaty ifelse-value (myFund = -1) [999] [ifelse-value (before = myfund) [(treaty + 1)][1]]

  ;; paying enteringFee - fee is paid if an IPL just entered the FD (during the Step 1 is not paid)
  if treaty = 1 and ticks > 0 [paying-entering-fee]
end

;;;; CHOOSE-FUND REPORTERS
to-report beingPI?
  ;; HERE ARE THE RULES FOR DECIDING BEING PI OR IPL (TRUE=PI, FALSE=IPL)
  ;; In this version, PLs have to be IPLs, they will not be independent
  report false
end

to-report chosenFund
  ;; HERE ARE THE RULES FOR CHOOSING FD
  if rule = 0 [report rule-00]
  if rule = 1 [report rule-01]
  if rule = 2 [report rule-02]
  if rule = 3 [report rule-03]
  if rule = 4 [report rule-04]
  if rule = 5 [report rule-05]
  if rule = 6 [report rule-06]
  if rule = 7 [report rule-07]
  if rule = 8 [report rule-08]
  if rule = 9 [report rule-09]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                          ;;;;;
;;;;; RULES' AND THEIR REPORTERS                                               ;;;;;
;;;;;                                                                          ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report rule-00
  ;; Completely random choosing
  report random F
end

to-report rule-01
  ;; INFORMATION-DETERMINISTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 perfectInformation (0) ;; the parameter is WHO number of fund!!!
  let m1 perfectInformation (1) ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report deterministicDecision (m0) (m1)
end

to-report rule-02
  ;; EXPERIENCE-DETERMINISTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 experience (0)  ;; the parameter is WHO number of fund!!!
  let m1 experience (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report deterministicDecision (m0) (m1)
end

to-report rule-03
  ;; PARTIAL-DETERMINISTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 imperfectInformation (0)  ;; the parameter is WHO number of fund!!!
  let m1 imperfectInformation (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report deterministicDecision (m0) (m1)
end

to-report rule-04
  ;; INFORMATION-STOCHASTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 perfectInformation (0) ;; the parameter is WHO number of fund!!!
  let m1 perfectInformation (1) ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report stochasticDecision (m0) (m1)
end

to-report rule-05
  ;; EXPERIENCE-STOCHASTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 experience (0)  ;; the parameter is WHO number of fund!!!
  let m1 experience (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report stochasticDecision (m0) (m1)
end

to-report rule-06
  ;; PARTIAL-STOCHASTIC
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 imperfectInformation (0)  ;; the parameter is WHO number of fund!!!
  let m1 imperfectInformation (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report stochasticDecision (m0) (m1)
end

to-report rule-07
  ;; INFORMATION-LEARNING
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 perfectInformation (0)  ;; the parameter is WHO number of fund!!!
  let m1 perfectInformation (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report learningDecision (m0) (m1)
end

to-report rule-08
  ;; EXPERIENCE-LEARNING
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 experience (0)  ;; the parameter is WHO number of fund!!!
  let m1 experience (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report learningDecision (m0) (m1)
end

to-report rule-09
  ;; PARTIAL-LEARNING
  ;; Firstly, getting dividends' means through the respective reporter
  let m0 imperfectInformation (0)  ;; the parameter is WHO number of fund!!!
  let m1 imperfectInformation (1)  ;; the parameter is WHO number of fund!!!

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report learningDecision (m0) (m1)
end

to-report perfectInformation [idFD] ;; idFD is abbreviation for "ID of fund"
  ;; firstly, checking the length of FDs' DiviList - the list might be shorter than InspFD wants
  let x length [diviList] of fund idFD
  if x > inspFD [set x inspFD]

  ;; secondly, we construct sublists of dividends in the last X steps of both FDs
  let fx sublist ([diviList] of fund idFD) 0 x

  ;;  thirdly, we count mean dividend in the last X steps
  let mx 0
  if not empty? fx [set mx mean fx]

  ;; lastly, reporting itself
  report mx
end

to-report experience [idFD] ;; idFD is abbreviation for "ID of fund"
  ;; firstly, dermining which pay-off experince we use - whether PAYOFF1 or PAYOFF0
  let payOff ifelse-value (idFD = 0) [payOff0] [payOff1]

  ;; secondly, checking the length of FDs' DiviList - the list might be shorter than InspFD wants
  let x length payoff
  if x > inspFD [set x inspFD]

  ;; secondly, we construct sublists of dividends in the last X steps of both FDs
  let fx sublist payOff 0 x

  ;;  thirdly, we count mean dividend in the last X steps
  let mx 0
  if not empty? fx [set mx mean fx]

  ;; lastly, reporting itself
  report mx
end

to-report imperfectInformation [idFD] ;; idFD is abbreviation for "ID of fund"
  ;; firstly, checking the length of FDs' DiviList - the list might be shorter than InspFD wants
  let x length [diviList] of fund idFD
  if x > inspFD [set x inspFD]

  ;; secondly, we randomly select X dividends from the FD's list of dividends
  let fx n-of x ([diviList] of fund idFD)

  ;;  thirdly, we count mean dividend from the dividends' selection
  let mx 0
  if not empty? fx [set mx mean fx]

  ;; lastly, reporting itself
  report mx
end

to-report deterministicDecision [m0 m1]
  ;; Firstly, let's start with initializing value of ChosenFD by copying the MyFund value
  let chosenFD myFund

  ;; Secondly, we determine ChosenFD
  ;; In a few steps until "ticks <= probing", the ChosenFD will be set randomly,
  ;; after this period the IPL chooses FD 0 if its mean dividend is bigger in inspected period,
  ;; and lastly, there is also probability of Epsilon, that the IPL changes her mind and chooses the other FD.
  ifelse ticks < probing [
    set chosenFD random F   ;; in the first few steps we initialize ChosenFD randomly
  ][
    set chosenFD ifelse-value (m0 >  m1) [0][1] ;; IPL chooses FD 0 if the M0 is bigger than M1
    if epsilon > random 100 [set chosenFD (1 - chosenFD)] ;; probability of Epsilon, that the IPL changes her mind and chooses the other FD
  ]

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report chosenFD
end

to-report stochasticDecision [m0 m1]
  ;; Firstly, let's start with initializing value of ChosenFD by copying the MyFund value
  let chosenFD myFund

  ;; Secondly, we determine ChosenFD
  ;; In a few steps until "ticks <= probing", the ChosenFD will be set randomly,
  ;; after this period the IPL chooses FD proportionally to ratio of mean dividends in inspected period,
  ;; and lastly, there is also probability of Epsilon, that the IPL changes her mind and chooses the other FD.
  ifelse ticks < probing [
    set chosenFD random F   ;; in the first few steps we initialize ChosenFD randomly
  ][
    set chosenFD ifelse-value (m0 > random-float (m0 + m1)) [0][1] ;; IPL chooses FD 0 proportionally to ratio of M0 / (M0 + M1)
    if epsilon > random 100 [set chosenFD (1 - chosenFD)] ;; probability of Epsilon, that the IPL changes her mind and chooses the other FD
  ]

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report chosenFD
end

to-report learningDecision [m0 m1]
  ;; Firstly, let's start with initializing value of ChosenFD by copying the MyFund value
  let chosenFD myFund

  ;; This part contains random decision for the first PROBING steps (as other decision routines) and
  ;; the decision algorithm based on reinforcement learning
  ifelse ticks < probing [
    set chosenFD random F   ;; in the first few steps we choose ChosenFD randomly
  ][
    ;; Secondly, we update probability of choosing FD 0
    if m0 > m1 [set pChoiceFD0 (pChoiceFD0 + learning)]
    if m0 < m1 [set pChoiceFD0 (pChoiceFD0 - learning)]
    if pChoiceFD0 > 100 [set pChoiceFD0 100]
    if pChoiceFD0 < 0 [set pChoiceFD0 0]

    ;; Thirdly, we use the learning term "pChoiceFD0" for stochastic choosing of FD
    set chosenFD ifelse-value (pChoiceFD0 > random 100) [0][1] ;; IPL chooses FD proportionally to learning term
    if epsilon > random 100 [set chosenFD (1 - chosenFD)] ;; probability of Epsilon, that the IPL changes her mind and chooses the other FD
  ]

  ;; lastly, reporting chosen FD through respective reporter according M0 and M1 values
  report chosenFD
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END

to paying-entering-fee
  let payment ([entering] of fund myFund)
  set wallet  (wallet - payment)
  ask fund myFund [set treasury (treasury + payment)]
end

to assign-players
  set assigned (players with [myFund = [who] of myself])
end

to choose-pool
  ;; variable Before is created for telling whether the PL changed pool
  let before myPool

  ;; Only PIs who stayed longer than S may change pool, but during the first round they could choose freely
  if stayed > S or ticks = 0 [set myPool chosenPool]

  ;; variable Moved? tells whether the PL changed pool
  set moved? (myPool != before)  ;; PI might randomly choose same pool - in this case she does not move
end

;;;; CHOOSE-POOL REPORTERS
to-report chosenPool
  ;; HERE ARE THE RULES FOR CHOOSING A POOL
  ;; choosing fund to be assigned to for at least T steps
  report random 2.1
end


to send-players
  ;; Firstly, we erase myS/L/H and iS/L/H variables
  ;; - these variables are used in the rest of routine and its subroutines,
  ;; in case the variables are not erased runtime errors occur.
  erasing-variables-for-send-players-routine

  ;; Secondly, check whether there are any IPLs - in case no, STOP
  if count assigned = 0 [stop]

  ;; Thirdly, do subroutines
  decide-about-allocation-numbers
  decide-whom-reallocate
  send-players-to-their-pools
  refund-tau
end

to erasing-variables-for-send-players-routine
  set myStable 0
  set myLow 0
  set myHigh 0
  set iStable no-turtles
  set iLow no-turtles
  set iHigh no-turtles
end

to decide-about-allocation-numbers
  ;; HERE ARE THE RULES FOR FDS STRATEGY
  ;; Now there are 2 strategies, no learning - true agressive and true conservative
  ;; We assign FD 0 as conservative and FD 1 as agressive
  if who = 0 [
    if (strategyFD0 = 0) [conservative-strategy]
    if (strategyFD0 = 1) [aggressive-strategy]
    if (strategyFD0 = 2) [mixed-strategy]
  ]
  if who = 1 [
    if (strategyFD1 = 0) [conservative-strategy]
    if (strategyFD1 = 1) [aggressive-strategy]
    if (strategyFD1 = 2) [mixed-strategy]
  ]
end

to conservative-strategy
  ;; This strategy sends 1 on Low, 1 on High and the rest on the Stable, but it starts with assigning the first member on Stable pool
  ;; I think we have to start with the 1 on Stable - this makes difference between Coservative and Aggressive in cases of low N,
  ;;it might happen the Conservative has the only one member or just two, then sending them firstly on the risky pools does not make
  ;; a difference between Conservative and Aggressive, ther would be need of 3 members which means 30% in case of N=30.

  ;; Firstly, assign 1 on Stable and store the rest of the count of Assigned IPLs
  set myStable 1
  let remainingIPLs (count assigned) - 1

  ;; Secondly, we assign at least 1 IPL on Low and 1 IPL on High, if there are at least 2 IPLs assigned
  if remainingIPLs > 0 [
    set myLow 1
    set remainingIPLs (remainingIPLs - 1)
  ]
  if remainingIPLs > 0 [
    set myHigh 1
    set remainingIPLs (remainingIPLs - 1)
  ]

  ;; Thirdly, we assign the rest to Stable
  set myStable (myStable + remainingIPLs)
end

to aggressive-strategy  ;; This strategy sends a half on Low, a half on High and 0 on Stable
  ;; Firstly, store important past values and the count of Assigned IPLs
  let remainingIPLs (count assigned)

  ;; Secondly, we assign a half on Low, a half on High and 0 on Stable
  set myLow   (ceiling (remainingIPLs / 2))
  set myHigh    remainingIPLs - myLow
  set myStable 0
 end

to mixed-strategy  ;; This strategy sends 37.5% on Low, a 25% on High and rest (around 37.5%) on Stable
  ;; Firstly, store important past values and the count of Assigned IPLs
  let remainingIPLs (count assigned)

  ;; Secondly, we assign a half on Low, a half on High and 0 on Stable
  set myStable (round (remainingIPLs * conservativeness))
  set myHigh   (ceiling ((remainingIPLs - myStable) * (1 - aversiveness)))
  set myLow    (remainingIPLs - myHigh - myStable)
end


to decide-whom-reallocate
  ;; Firstly, let's monitor contemporary numbers/agentsets
  let myOnStable (assigned with [myPool = stablePool])
  let myOnLow (assigned with [myPool = lowPool])
  let myOnHigh (assigned with [myPool = highPool])

  ;; Secondly, compare contemporary numbers and allocations
  let cStable ((count myOnStable) - myStable)
  let cLow ((count myOnLow) - myLow)
  let cHigh ((count myOnHigh) - myHigh)

  ;; Thirdly, send PLs from over-crowded pools to CONTAINER
  let container no-turtles
  if cStable > 0 [set container (n-of cStable myOnStable)]
  if cLow > 0 [set container (turtle-set container (n-of cLow myOnLow))]
  if cHigh > 0 [set container (turtle-set container (n-of cHigh myOnHigh))]

  ;; Fourthly, underpopulated pools receive PLs from CONTAINER
  if cStable < 0 [
    let taken (n-of abs(cStable) container)
    set iStable taken
    ask taken [set container other container]
  ]
  if cLow < 0 [
    let taken (n-of abs(cLow) container)
    set iLow taken
    ask taken [set container other container]
  ]
  if cHigh < 0 [
    let taken (n-of abs(cHigh) container)
    set iHigh taken
    ask taken [set container other container]
  ]
end

to send-players-to-their-pools
  ask iStable [
    let before myPool
    set myPool stablePool
    set moved? (before != myPool)
  ]
  ask iLow [
    let before myPool
    set myPool lowPool
    set moved? (before != myPool)
  ]
  ask iHigh [
    let before myPool
    set myPool highPool
    set moved? (before != myPool)
  ]
end

to refund-tau
  ;; count how much it has cost to re-allocate IPLs
  let moved (assigned with [moved?])
  let cost  (tau * count moved)

  ;; send Taus from Treasury to moved IPLs Wallets
  set treasury (treasury - cost)
  ask moved [set wallet (wallet + tau)]
end

to move-to-pool
  if moved? [
    ;; Set exact position on the pool
    setxy xPosition yPosition

    ;; pay the Tau - fee for switching pools
    pay-tau

    ;; set marker MOVED? to FALSE, because it is not needed anymoore in this step
    set moved? false
  ]
end

to pay-tau
  ;; paying Tau iself
  set wallet (wallet - tau)

  ;; Reset of variable Stayed - the PL just switched pools, so she starts Stayed again from 1
  set stayed 1
end

to update-pay-offs
  ;; STABLE
  set stablePayoff sStablePayoff

  ;; LOW
  let nLow count players with [myPool = lowPool]
  set lowPayoff 0
  if random 99.1 < pLow and nLow > 0 [set lowPayoff (sLowPayoff / nLow)]

  ;; HIGH
  let nHigh count players with [myPool = highPool]
  set highPayoff 0
  if random 99.1 < pHigh and nHigh > 0 [set highPayoff (sHighPayoff / nHigh)]
end

to get-paid-off
  ;; defining recent pay-off of the PL
  let myPayoff 0
  if myPool = stablePool [set myPayoff stablePayoff]
  if myPool = lowPool [set myPayoff lowPayoff]
  if myPool = highPool [set myPayoff highPayoff]

  ;; PIs are paid directly to their Wallet
  ;; IPLs send their pay-offs to Treasury of their FDs
  ifelse indie? [
    set wallet (wallet + myPayoff)
  ][
    ask fund myFund [set treasury (treasury + myPayoff)]
  ]
end

to pay-dividend-off
  ;; counting assigned IPLs
  let a (count assigned)

  ;; just for fun monitor of assigned IPLs' number
  set label a

  ;; calculating a size of the Dividend
  set dividend ifelse-value (a = 0) [0][treasury / a]

  ;; sending dividend to assigned IPLs
  ask assigned [
    set wallet (wallet + [dividend] of myself)
  ]

  ;; Asking assigned to record the dividend to respective list
  if who = 0 [ask assigned [set payoff0 fput ([dividend] of myself) payoff0]]
  if who = 1 [ask assigned [set payoff1 fput ([dividend] of myself) payoff1]]

  ;; erasing treasury and storing dividend to the DiviList variable list
  set treasury 0
  set diviList fput (precision dividend 1) diviList
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                          ;;;;;
;;;;;  SETUP PROCEDURES                                                                        ;;;;;
;;;;;                                                                                          ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-globals
  ifelse fixed40And80Payoffs? [
    set sLowPayoff 40
    set sHighPayoff 80
  ][
    set sLowPayoff (0.8 * N)
    set sHighPayoff (1.6 * N)
  ]
  set stablePayoff 0  ;; Codes updated pay-off being paid in recent step to the Players on the Stable pool.
  set lowPayoff    0  ;; Codes updated pay-off being paid in recent step to the Players on the Low pool.
  set highPayoff   0  ;; Codes updated pay-off being paid in recent step to the Players on the High pool.
  set maxInspFD minInspFD ;; InspFD is set to one exact value not to a range of them - both parameters are tied, for untie make the whole line a comment
end

to set-pools
  ;; defining pools' code numbers via variables: "stablePool", "lowPool" and "highPool"
  ;;;; "stablePool", "lowPool" and "highPool" are variables constantly defined through reporters.
  ;;;; See reporting procedures (to-report) at the end of code
  ask patch stablePool 0 [
    set pcolor blue
    set plabel "STABLE pool"
  ]
  ask patch lowPool 0 [
    set pcolor green
    set plabel "LOW pool"
  ]
  ask patch highPool 0 [
    set pcolor red
    set plabel "HIGH pool"
  ]
  ask patch 3 0 [
    set pcolor black
    set plabel "FUND'S pool"
  ]
end

to set-funds
  create-funds F [
    ;; displaying variables
    set color (5 + who * 10)
    set shape "house"
    set size 0.3
    set label who
    set label-color white

    ;; initialization of Fund's variables
    set entering enteringFee
    set treasury 0
    set dividend 0
    set myStable 0
    set myLow 0
    set myHigh 0
    set assigned no-turtles
    set iStable no-turtles
    set iLow no-turtles
    set iHigh no-turtles

    ;; Initialization of FD's strategy variables
    set diviList []
    set conserve 50
    set aversive 50

    ;; now we evenly distribute FDs over their pool
    let biasX (who mod 3)
    let biasY ((who - biasX) / 3)
    set xcor 3 + (-0.3 + (biasX * 0.3) + xcor)
    set ycor (0.3 + (biasY * -0.3) + ycor)
    ;; Note: maximum of funds is 9! in case of larger number it is needed to accomodate coefitients of SIZE, and PLACE
  ]
end

to set-players
  create-players N [
    ;; displaying variables
    set color yellow
    set size 0.1
    set id (who - F)
    setxy xPosition yPosition

    ;; initialization of Players's behavior
    set rule ifelse-value (id < (nOfTRule1) * N) [tRule1][tRule2]
    set myPool 0
    set myFund -1    ;; -1 indicates the PL is not part of any FD
    set wallet 0
    set stayed 999    ;; Just for the technical reasons, after the first step the STAYED will be set to 1
    set treaty 999    ;; 999 indicates the PL is not part of any FD
    set moved? false
    set indie? false
    set inspFD minInspFD + random (maxInspFD - minInspFD + 0.1)  ;; Personal parameter - how many last dividends IPL inspects to decide which FD to choose
    set payoff0 []
    set payoff1 []
    set pChoiceFD0 50
  ]
end

;; "xPosition" and "yPosition" are variables describing detailed position of PLAYERS on the pools.
;;;; We evenly distribute players over their pools, but we set the coordinates exactly according their WHO number,
;;;; so the player has unique and always same position on every pool.
;;;; Note: maximum of players is 100! In case of larger number it is needed to accomodate coefitients of SIZE, and PLACE.
to-report xPosition
  let biasX (id mod 10)
  let biasY ((id - biasX) / 10)
  report (-0.45 + (biasX * 0.1) + myPool)
end
to-report yPosition
  let biasX (id mod 10)
  let biasY ((id - biasX) / 10)
  report (0.45 + (biasY * -0.1))
end

;; Fund's reporters
to-report enteringFee
  report tau
end

;; Monitoring reporters
to-report minWallet
  report precision (min [wallet] of players) 1
end
to-report maxWallet
  report precision (max [wallet] of players) 1
end
to-report medianWallet
  report precision (median [wallet] of players) 1
end
to-report meanWallet
  report precision (mean [wallet] of players) 1
end

;; "stablePool", "lowPool" and "highPool" are variables defined via following reporters.
to-report stablePool
  report 0
end
to-report lowPool
  report 1
end
to-report highPool
  report 2
end

to save-players-rules-and-results
  ;; For the tournament ballance we need omitt simulations with TRule1 > TRule2
  if tRule1 > tRule2 [stop]

  ;; Opening the file with results
  file-open "results5.csv"

  ;; Each PL writes in its own variables (ID, RULE, pChoiceFD0, and WALLET) and contextual variables (all others)
  ask players [
    file-print (word
      RS ";"
      N ";"
      tRule1 ";"
      tRule2 ";"
      nOfTRule1 ";"
      minInspFD ";"
      probing ";"
      epsilon ";"
      learning ";"
      id ";"
      rule ";"
      pChoiceFD0 ";"
      round (wallet)
      )
  ]

  ;; Closing file
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
4
88
812
297
-1
-1
200.0
1
10
1
1
1
0
1
1
1
0
3
0
0
0
0
1
ticks
30.0

BUTTON
4
10
59
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

BUTTON
59
10
114
44
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
1019
184
1112
217
tau
tau
0
1
1.0
0.05
1
NIL
HORIZONTAL

SLIDER
1111
184
1204
217
N
N
30
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
169
10
262
43
F
F
0
9
2.0
1
1
NIL
HORIZONTAL

SLIDER
1017
216
1112
249
sStablePayoff
sStablePayoff
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
1111
216
1204
249
sLowPayoff
sLowPayoff
0
99
40.0
1
1
NIL
HORIZONTAL

SLIDER
1203
216
1298
249
sHighPayoff
sHighPayoff
0
200
80.0
1
1
NIL
HORIZONTAL

PLOT
1031
360
1339
481
Histogram of Wallet
NIL
NIL
80.0
200.0
0.0
10.0
true
false
"" ""
PENS
"default" 5.0 1 -16777216 true "" "histogram [wallet] of players"

MONITOR
80
43
138
88
NIL
minWallet
17
1
11

MONITOR
138
43
199
88
NIL
maxWallet
17
1
11

MONITOR
4
43
81
88
NIL
medianWallet
17
1
11

BUTTON
114
10
169
44
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
261
10
354
43
S
S
0
50
0.0
1
1
NIL
HORIZONTAL

SLIDER
811
10
904
43
maxInspFD
maxInspFD
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
353
10
446
43
T
T
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
1203
184
1296
217
pLow
pLow
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
1293
184
1386
217
pHigh
pHigh
0
100
25.0
1
1
NIL
HORIZONTAL

MONITOR
811
88
895
133
FD 0 - dividend
precision ([dividend] of fund 0) 2
17
1
11

MONITOR
811
132
895
177
FD1 - dividend
precision ([dividend] of fund 1) 2
17
1
11

INPUTBOX
1386
184
1468
245
simulationStop
100.0
1
0
Number

SLIDER
720
10
812
43
minInspFD
minInspFD
0
100
4.0
1
1
NIL
HORIZONTAL

PLOT
4
297
812
534
plot 1
NIL
NIL
0.0
100.0
0.0
50.0
true
true
"" ""
PENS
"Fund 1" 1.0 0 -16777216 true "" "if ticks > 0 [plot count players with [myFund = 1]]"
"Dividend FD0 * 10" 1.0 0 -955883 true "" "if ticks > 0 [plot 10 * (precision ([dividend] of fund 0) 1)]"
"Dividend FD1 * 3" 1.0 0 -2674135 true "" "if ticks > 0 [plot 3 * (precision ([dividend] of fund 1) 1)]"

SLIDER
1034
146
1126
179
nRandoms
nRandoms
0
50
0.0
1
1
NIL
HORIZONTAL

CHOOSER
704
43
796
88
strategyFD0
strategyFD0
0 1 2 3
0

CHOOSER
795
43
887
88
strategyFD1
strategyFD1
0 1 2 3
1

SLIDER
303
43
430
76
conservativeness
conservativeness
0
1
0.6
0.01
1
NIL
HORIZONTAL

SLIDER
430
43
522
76
probing
probing
1
20
4.0
1
1
NIL
HORIZONTAL

SLIDER
521
43
613
76
epsilon
epsilon
0
50
10.0
1
1
NIL
HORIZONTAL

CHOOSER
1162
135
1254
180
testedRule
testedRule
0 1 2 3
2

SLIDER
445
10
537
43
tRule1
tRule1
0
9
0.0
1
1
NIL
HORIZONTAL

SLIDER
536
10
628
43
tRule2
tRule2
0
9
3.0
1
1
NIL
HORIZONTAL

SLIDER
628
10
720
43
nOfTRule1
nOfTRule1
0
1
0.5
0.1
1
NIL
HORIZONTAL

MONITOR
811
177
929
222
mean Wallet of tRule1
precision mean [wallet] of players with [rule = tRule1] 1
17
1
11

MONITOR
811
221
930
266
mean Wallet of tRule2
precision mean [wallet] of players with [rule = tRule2] 1
17
1
11

SLIDER
199
43
304
76
aversiveness
aversiveness
0
1
0.6
0.01
1
NIL
HORIZONTAL

SLIDER
612
43
704
76
learning
learning
0
100
7.0
0.1
1
NIL
HORIZONTAL

MONITOR
811
310
878
355
learning 2
precision (mean [pChoiceFD0] of players with [rule = tRule2]) 1
17
1
11

MONITOR
811
266
878
311
learning 1
precision (mean [pChoiceFD0] of players with [rule = tRule1]) 1
17
1
11

INPUTBOX
939
14
989
74
RS
100.0
1
0
Number

SWITCH
1051
23
1182
56
randomSeed?
randomSeed?
1
1
-1000

SWITCH
1051
71
1232
104
fixed40And80Payoffs?
fixed40And80Payoffs?
0
1
-1000

@#$#@#$#@
## ABREVIATIONS
TRP   == The Retirement Problem
FD/s  == Fund/s
PL/s  == Player/s
PI/s  == Private Investor/s, Indie/s
IPL/s == Incorporated Player/s - governed by a FD


## PURPOSE STATEMENT

The aim of this model is to explore chaotic system of The Retirement Problem (TRP). In TRP game there are three pools and Players (PLs) have to choose among them. The pools differ in the character of pay-off they give to PLs in the end of every step: 

  1. Stable pool gives $1 to each PL on the pool
  2. Low pool gives $0 with probability 50 % or (with the same probability of 50 %) it evenly distributes $40 to PLs being present on the pool (e.g. in case 5 PLs are present on the Low pool and they are lucky, so the pay-off is paid, every of these 5 PLs receives $8)
  3. High pool gives $0 with probability 75 % or (with the probability of 25 %) it evenly distributes $80 to PLs being present on the pool (e.g. in case 5 PLs are present on the High pool and they are lucky, so the pay-off is paid, every of these 5 PLs receives $16)

TRP combines two famous problems/games in one:

  1. El Farol Bar Problem - decision to choose safe Stable pool or a risky one
  2. Minority Game - in case of risky decision in (1.), the decision between Low or High pool
Note: In the long run the average value paid on both pools is $20, so the case is to predict which of the risky pools will be less populated, because the smaller population means higher personal pay-off received. On the other hand, the High and Low pools are not exactly same - as options in the original Minority Game - they differ in the level of risk, so the risk-aversive or risk-seeking strategies might be biased towards respective option.

This model studies two specific questions regarding TRP:

### 1) How to cool down the chaotic nature of the system? 
How to reduce oscilations around equilibria and make a system more stable and predictable?
 
Note: In present version there is the only one parameter tested for this chaos reduction - W (it is minimal number of steps a Player (PL) has to wait at a chosen pool before she is allowed to switch to another pool). It does not mean that the PL has to every W<sup>th</sup> step change the pool, W just mean number of mandatory waiting steps - the PL could stay the whole game at one pool.

### 2) How differ behaviors of Funds (FDs) and Private Investors (PIs) in cases of 1, 2, and 3 FDs? 
There are five types of decisions in one game:

  1. Every PL has to choose between being PI or being IPL (incorporated PL by a FD).
  2. PIs have to choose a pool to invest in.
  3. IPLs have to choose a FD to be a part of.
  4. FDs have to decide best strategies to compete with PIs.
  5. FDs have to decide best strategies to compete with othes FDs.

There is also sixth possible decision:

  6. FDs have to decide about size of entering fee. 
Note: Architecture is prepared for this decision, but in present version is the value set to Tau.  

## HOW IT WORKS
Every PL has to choose whether invest privately/ on her own and then become a PI, or whether to become incorporated in a FD. In case of more FDs the PL has to choose between/among FDs. The PI has to choose a pool to invest in privately. In case a PI switches between two pools, the PI has to pay the Tau (fee for switching pools).

FDs make decisions instead of their incorporated PLs and allocate their PLs on pools. In case of moving the PLs from a pool to another pool the FDs not the PLs pay the Tau. In case of entering a FD or switching between FDs, any PL pays the Tau (taken here as an administrative fee - in case the PL is not on intended pool the burden of paying Tau is on entering/switching PL, OK, in case the PL is on right pool the Tau is profit of the FD). The leaving of any FD and becoming PI is not taxed, i.e. costs 0. There is same exception as for the PI - the Tau as entering fee is not paid in the first step. 

The FD is only organising unit without its own capital. The FD allocates PLs (i.e. commands incorporated PLs to move to selected pools). The FD collects PLs' pay-offs. The FD pays Taus insted of moved PLs from collected pay-offs. And the FD evenly distributes rest of collected pay-offs to incorporated PLs (i.e. capital of any FD is 0 at the start and at the end of every step). 
FDs decide what is the ideal number of IPLs on Stable, Low and High pool.
Taus paid as entering fee are evenly redistributed at the end of step. 
All FDs use same algorithm for minimising Tau cost in allocation phase - they find the minimal number of switches needed for transition from past allocation to realising a new allocation. 
There is a special right of FDs - to not respect S parameter. PIs have to stay at least S steps at the chosen pool, but FDs might to move them before this waiting/staying period ends.

## VARIBLES
### SLIDERS
N == the number of PLs in the simulation

F == the number of FDs in the simulation

S == the minimal number of steps the PLs must _stay_ at the pool

T == the minimal lenght of _treaty_ with the FD, i.e. the minimal number of steps the IPL has to be a part of the FD

tau == the payment/fee for the switching pools

sStablePayoff == pay-off paid to each PL on Stable pool

sLowPayoff == overall pay-off paid to all PLs on Low pool together

sHighPayoff == overall pay-off paid to all PLs on High pool together

pLow == probability of pay-off at Low pool

pHigh == probability of pay-off at High pool

maxInspFD == the maximum allowed number of last dividends inspected according  to deciding which FD to choose

minInspFD == the minimum allowed number of last dividends inspected according  to deciding which FD to choose
 
nRandoms == the number of IPLs following the random rule (rule-00)

conservativeness == the fraction of IPLs sent by FD with Mixed strategy to Stable pool

aversiveness == the fraction of IPLs sent by FD with Mixed strategy to Low pool, note the whole is only risky pools, i.e. the sum of Low and High pools

probing == (for rule-01) the minimal number of steps from start, which IPLs have to stay in the initially randomly chosen FD  

testedRule == chooser/slider indicating number of IPLs rule under testing

tRule1 == slider idicating number of the first IPLs rule in the rules combat

tRule2 == slider idicating number of the second IPLs rule in the rules combat

nOfTRule1 == slider indicating the fraction of IPLs following the tRule1 in the rules combat

learning == the size of change probability choosing FD0 in learning algorithm
 

#### Sliders from past versions:
PoBI == probability of being independent: needed for the earlier stages of model, where PLs decided randomly whether to become PI or IPL

PoCF == probability of changing fund: needed in earlier stages of model, where IPLs decide randomly whether to change fund or stay in same fund


## SOME HEGES

10 20 20 -> 20 5 25 -> +10 -15 +5 -> +10 -10/-5 +5
10 20 20 -> 20 10 20 -> +10 -10 +0 
10 20 20 -> 25 10 15 -> +15 -10 -5 -> +10/+5 -10 -5 

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="SOU" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>save-players-rules-and-results</final>
    <metric>precision mean [wallet] of players with [rule = tRule1] 1</metric>
    <metric>precision mean [wallet] of players with [rule = tRule2] 1</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="tRule1">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tRule2">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pHigh">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sLowPayoff">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conservativeness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nOfTRule1">
      <value value="10"/>
      <value value="25"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pLow">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probing">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minInspFD">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="F">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sStablePayoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationStop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testedRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aversiveness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxInspFD">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nRandoms">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sHighPayoff">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SOU2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>precision mean [wallet] of players with [rule = tRule1] 1</metric>
    <metric>precision mean [wallet] of players with [rule = tRule2] 1</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="11" step="1" last="40"/>
    <enumeratedValueSet variable="tRule1">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tRule2">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pHigh">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sLowPayoff">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conservativeness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nOfTRule1">
      <value value="10"/>
      <value value="25"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pLow">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probing">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minInspFD">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="F">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sStablePayoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationStop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testedRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aversiveness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxInspFD">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nRandoms">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sHighPayoff">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SOU3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>save-players-rules-and-results</final>
    <metric>precision mean [wallet] of players with [rule = tRule1] 1</metric>
    <metric>precision mean [wallet] of players with [rule = tRule2] 1</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="41" step="1" last="100"/>
    <enumeratedValueSet variable="tRule1">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tRule2">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pHigh">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sLowPayoff">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conservativeness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nOfTRule1">
      <value value="10"/>
      <value value="25"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pLow">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probing">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minInspFD">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="F">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sStablePayoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationStop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testedRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aversiveness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxInspFD">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nRandoms">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sHighPayoff">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SOU4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>save-players-rules-and-results</final>
    <metric>count [assigned] of fund 0</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed40And80Payoffs?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="N">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
      <value value="300"/>
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tRule1" first="0" step="1" last="9"/>
    <steppedValueSet variable="tRule2" first="0" step="1" last="9"/>
    <enumeratedValueSet variable="nOfTRule1">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probing">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minInspFD">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pHigh">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sLowPayoff">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conservativeness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pLow">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="F">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sStablePayoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationStop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testedRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aversiveness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxInspFD">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nRandoms">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sHighPayoff">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SOU5" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>save-players-rules-and-results</final>
    <metric>count [assigned] of fund 1</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed40And80Payoffs?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="1" step="1" last="80"/>
    <enumeratedValueSet variable="N">
      <value value="30"/>
      <value value="50"/>
      <value value="62"/>
      <value value="75"/>
      <value value="92"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tRule1" first="0" step="1" last="9"/>
    <steppedValueSet variable="tRule2" first="0" step="1" last="9"/>
    <enumeratedValueSet variable="nOfTRule1">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
      <value value="0.15"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minInspFD">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilon">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probing">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pHigh">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sLowPayoff">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conservativeness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pLow">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="F">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sStablePayoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationStop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testedRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aversiveness">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxInspFD">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nRandoms">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategyFD1">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sHighPayoff">
      <value value="80"/>
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
