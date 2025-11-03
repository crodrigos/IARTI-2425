:- use_module("../Utils/Timer.object").
:-['../Utils/ListUtils.pl'].
:-['../Utils/BulkWrite.pl'].

:-['Schedule.object.pl'].
:-['vessels.pl']. % Example Vessels
:-['scheduling_vessels_1.pl']. % Given Code



% ---- PREDICADOS DINAMICOS

% Current Shortest Schedule and Interval
% shortest_delay(Sequence, Delay)
:- dynamic shortest_delay/2.
shortest_delay(_, 999999).
setShortestDelay(NewSchedule, NewDelay):-
    %bw("Delay: ", NewDelay),
    retract(shortest_delay(_,_)),
    asserta(shortest_delay(NewSchedule, NewDelay)).

% Current Shortest Time for Schedule
:- dynamic shortest_time/1.
shortest_time(0).
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
    allVessels(VL_temp),length(VL_temp, NumberOfVessels),

    range(NumberOfVessels, NVL),
    
    ProgramTimerID is 0001,
    reset_timer(ProgramTimerID),start_timer(ProgramTimerID),
    max_cranes(NCranes),!,

    ((
        member(NVessels, NVL),

        allVessels(NVessels, ListOfVessels),
        bw("\n\n", "----------------------"),
        bw("Number of Vessels: ", NVessels),

        CraneTimerID = "craneId", reset_timer(CraneTimerID), start_timer(CraneTimerID),

        obtainShortestSequence(ListOfVessels,NCranes,S,D),

        get_elapsed_time(CraneTimerID, Time),
        bw("\nCalculation for N Vessels (ms): ", Time),

        fail
    );true),bw("\n\n").
    % get_elapsed_time(TimerID, Time),
    % bw("\n\n Total Runtime Duration (ms): ", Time).

obtainShortestSequence(ListOfVessels,NCranesAvailable, ShortestSchedule, ShortestDelay):-
    ((
        setShortestDelay(_,9999999),
        range(NCranesAvailable, CL),!,

        member(NCranes, CL),
        bw(["\n\nTesting for ", NCranes,  " crane(s)\n"]),

        reset_timer, start_timer,
        (obtainShortestSequence1(ListOfVessels,NCranes);true),
        
        shortest_delay(Seq,Delay), get_elapsed_time(CalculationTime),
        bw("Shortest Delay: ", Delay),
        bw("Shortest Sequence: ", Seq),
        bw("Calculation Time (ms): ", CalculationTime),
        
        Delay=<0,!
    );true),
    shortest_delay(ShortestSchedule, ShortestDelay).

obtainShortestSequence1(ListOfVessels,NCranes):-
    permutation(ListOfVessels, NewOrderedVesselList),
    sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence), % 
    sum_delays(TripletSequence, Delay),
    compare_shortest_delay(TripletSequence, Delay),
    fail.

sequenceTemporization(OrderedVesselList, NCranes, TripletSequence):-
    sequenceTemporization1(0,NCranes, OrderedVesselList, TripletSequence).

sequenceTemporization1(_,_,[],[]):-!.
sequenceTemporization1(
    EndPrevSeq, 
    NCranes,
    [Vessel| LV], 
    [(Vessel, TStartUnloading, TEndLoading)|Seq]
):-
    vessel(Vessel, TArrival, _, DUnloading, DLoading),

    ((TArrival>EndPrevSeq,!,TStartUnloading is TArrival);TStartUnloading is EndPrevSeq+1),
    
    OperationDuration is (DUnloading + DLoading)//NCranes + 1,
 
    TEndLoading is TStartUnloading + OperationDuration,

    sequenceTemporization1(TEndLoading,NCranes,LV,Seq).
