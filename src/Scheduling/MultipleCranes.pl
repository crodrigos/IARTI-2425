:- module(cranes, [
    testForDifNumVessels/0,
    obtainShortestSequence/2
]).

:- use_module("../Utils/Timer.object").
:- use_module("../Utils/Map.object").
:-['../Utils/ListUtils.pl'].
:-['../Utils/BulkWrite.pl'].

:-['Schedule.object.pl'].
:-['vessels.pl']. % Example Vessels
:-['scheduling_vessels_1.pl']. % Given Code





%:- module(cranes, [testForDifNumVessels/0]).

% ---- PREDICADOS DINAMICOS ----

% Current Shortest Schedule and Interval
% shortest_delay(Sequence, Delay)
:- dynamic shortest_delay/2.
shortest_delay(_, 999999).
setShortestDelay(NewSchedule, NewDelay):-
    %bw("Delay: ", NewDelay),
    retract(shortest_delay(_,_)),
    asserta(shortest_delay(NewSchedule, NewDelay)).

% Current Longest Schedule and Interval
% longest_delay(Sequence, Delay)
:- dynamic longest_delay/2.
longest_delay(_, 0).
setLongestDelay(NewSchedule, NewDelay):-
    %bw("Delay: ", NewDelay),
    retract(longest_delay(_,_)),
    asserta(longest_delay(NewSchedule, NewDelay)).

% Current Shortest Time for Schedule
:- dynamic shortest_time/1.
shortest_time(-1).
setShortestTime(NewTime):-
    retract(shortest_time(_)),
    asserta(shortest_time(NewTime)).

% Interval Size to be analyzed
% Default Value 167, 24*7 (a week)
:- dynamic max_time/1.
max_time(167).
setMaxTime(NewValue):-
    retract(max_time(_)),
    asserta(max_time(NewValue)).

% Calculation time statistics
:- dynamic calculation_time/2.
calculation_time(0, 0).
setCalculationTime(TimeTaken, NCalculations):-
    retract(calculation_time(_,_)),
    asserta(calculation_time(TimeTaken, NCalculations)).

% Maximum number of cranes
% WARNING: DONT CHANGE DURING RUNTIME
:- dynamic max_cranes/1.
max_cranes(7).
setMaxCranes(NewValue):-
    retract(max_cranes(_)),
    asserta(max_cranes(NewValue)).

testForDifNumVessels:-
    allVessels(VL_temp), length(VL_temp, NumberOfVessels),
    range(NumberOfVessels, NVL),
    
    ProgramTimerID is 0001,
    reset_timer(ProgramTimerID), start_timer(ProgramTimerID),
    max_cranes(NCranes), !,

    ((
        member(NVessels, NVL),
        allVessels(NVessels, ListOfVessels),
        bw("\n\n", "----------------------"),
        bw("Number of Vessels: ", NVessels),

        CraneTimerID = "craneId", reset_timer(CraneTimerID), start_timer(CraneTimerID),

        obtainShortestSequence(ListOfVessels, NCranes),

        get_elapsed_time(CraneTimerID, Time),
        bw("\nCalculation for N Vessels (ms): ", Time),

        fail
    ); true), bw("\n\n").


obtainShortestSequence(ListOfVessels, NCranesAvailable):-

    mapremove(medium),
    mapset(medium, (0, 0, 0)),

    ((
        setShortestDelay(_, 9999999),
        setLongestDelay(_, 0),
        range(NCranesAvailable, CL), !,

        member(NCranes, CL),
        bw(["\n\nTesting for ", NCranes, " crane(s)\n"]),

        reset_timer, start_timer,
        (obtainShortestSequence1(ListOfVessels, NCranes); true),
        
        shortest_delay(SSeq, SDelay), 
        longest_delay(LSeq, LDelay),
        mapget(medium, (_, _, MDelay)),

        bw("Shortest Sequence: ", SSeq),
        bw(" Longest Sequence: ", LSeq),

        bw("   Shortest Delay: ", SDelay),
        bw("    Longest Delay: ", LDelay),
        bw("     Medium Delay: ", MDelay),

        get_elapsed_time(CalculationTime),
        bw("Calculation Time (ms): ", CalculationTime),

        SDelay =< 0, !
    ); true).

obtainShortestSequence1(ListOfVessels, NCranes):-
    permutation(ListOfVessels, NewOrderedVesselList),
    sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence),
    sum_delays(TripletSequence, Delay),
    compare_shortest_delay(TripletSequence, Delay),
    compare_longest_delay(TripletSequence, Delay),
    compare_median_delay(Delay),
    fail.


compare_longest_delay(TripletSequence, Delay):-
    longest_delay(_, LongestDelay),
    ((
        Delay > LongestDelay, !, setLongestDelay(TripletSequence, Delay)
    ); true).

compare_median_delay(Delay):-
    mapget(medium, (Sum, N, Medium)), % FIXME: Maybe change from mapset
    N1 is N+1,
    Sum1 is Sum+Delay,
    Medium1 is Sum1/N1,
    mapset(medium, (Sum1, N1, Medium1)).


sequenceTemporization(OrderedVesselList, NCranes, TripletSequence):-
    sequenceTemporization1(0, NCranes, OrderedVesselList, TripletSequence).

sequenceTemporization1(_, _, [], []):- !.
sequenceTemporization1(
    EndPrevSeq, 
    NCranes,
    [Vessel|LV], 
    [(Vessel, TStartUnloading, TEndLoading)|Seq]
):-
    vessel(Vessel, TArrival, _, DUnloading, DLoading),
    ((TArrival > EndPrevSeq, !, TStartUnloading is TArrival); TStartUnloading is EndPrevSeq + 1),
    OperationDuration is (DUnloading + DLoading)//NCranes + 1,
    TEndLoading is TStartUnloading + OperationDuration,
    sequenceTemporization1(TEndLoading, NCranes, LV, Seq).
