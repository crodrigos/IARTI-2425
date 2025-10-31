
:-['Schedule.object.pl'].
:-['ListUtils.pl'].
:-['vessels.pl']. % Example Vessels
:-['scheduling_vessels_1.pl']. % Given Code
:-['BulkWrite.pl'].
:-['Counter.object.pl'].

:- autoload(library(lists), [append/3, member/2, last/2]).

max_cranes(10).

% Current Shortest Schedule and Interval
% shortest_delay(Sequence, Delay)
:-dynamic shortest_delay/2.
shortest_delay(_,999999).
setShortestDelay(NewSchedule, NewDelay):-
    %bw([ "New Delay: ", NewDelay, "\n"]),
    retract(shortest_delay(_,_)),
    asserta(shortest_delay(NewSchedule, NewDelay)).

% Current Shortest Time for Schedule
:-dynamic shortest_time/1.
shortest_time(0).
setShortestTime(NewTime):-
    retract(shortest_time(_)),
    asserta(shortest_time(NewTime)).

% Interval Size to be analized
% Default Value 167, 24*7 (a week)
:-dynamic max_time/1.
max_time(167).
setMaxTime(NewValue):-
    retract(max_time(_)),
    asserta(max_time(NewValue)).

:-dynamic calculation_time/2.
calculation_time(0, 0).
setCalculationTime(TimeTaken, NCalculations):-
    retract(calculation_time(_,_)),
    asserta(calculation_time(TimeTaken, NCalculations)).


%
% [
%     [vessel1, vessel2], % Grua1
%     [vessel3], % Grua2
%     [vessel4], % Grua3
% ]
createOrderedVesselMatrix(Vessels, NCranes, VesselMatrix):-
    createdOrderedVesselMatrix1(Vessels, NCranes, 1, VesselMatrix).

% FIXME: VERIFICAR SE EXISTEM DUPLICADOS NO findall
createdOrderedVesselMatrix1(Vessels, Ncranes, Ncranes, [Vessels]).
createdOrderedVesselMatrix1(Vessels, NCranes, Depth, [Head|Rest]) :-
    length(Vessels, NVessels),
    NCranes > NVessels,
    Depth < NCranes,

    append(Head, Tail, Vessels),
    Depth1 is Depth + 1,
    createdOrderedVesselMatrix1(Tail, NCranes, Depth1, Rest).

createdOrderedVesselMatrix1(Vessels, NCranes, Depth, [Head|Rest]) :-
    Depth < NCranes,
    append(Head, Tail, Vessels),
    Head \= [],
    Tail \= [],
    Depth1 is Depth + 1,
    createdOrderedVesselMatrix1(Tail, NCranes, Depth1, Rest).

createMultipleCraneSchedule([],[]).
createMultipleCraneSchedule([OrderedVesselCrane| VesselMatrix], [ScheduleOfCrane| Schedule]):-
    createMultipleCraneSchedule(VesselMatrix, Schedule),
    sequence_temporization(OrderedVesselCrane, ScheduleOfCrane).


checkMultipleCraneScheduleDelay([], 0).
checkMultipleCraneScheduleDelay([CraneSchedule|RestCraneSchedule], NewDelay):-
    checkMultipleCraneScheduleDelay(RestCraneSchedule, CurrentDelay),
    sum_delays(CraneSchedule, Delay),
    NewDelay is Delay + CurrentDelay.

% STARTING POINT OF PROGRAM
run:-
    obtainShortestSequence(ShortestSchedule, ShortestDelay, ShortestTime),
    writeSchedule(ShortestSchedule, ShortestDelay, ShortestTime).

obtainShortestSequence(ShortestSchedule, ShortestDelay, ShortestTime):-
    obtainShortestSequence1,
    shortest_delay(ShortestSchedule, ShortestDelay),
    shortest_time(ShortestTime).

obtainShortestSequence1:-
    max_cranes(MaxCranes), % Get Max Cranes
    range(MaxCranes, RL),!, % Gen List of [1,2,...,MaxCranes]

    member(NCranes, RL), % Get Value from RL
    bw(["\nCalculating for ", NCranes, " Crane(s)\n\n"]),

    reset_timer, % Statistics
    start_timer, % Statistics

    reset_count,
    start_count,
    (obtainShortestSequenceForNCranes(NCranes);true),


    get_elapsed_time(T), % Statistics
    bw("Time Taken (ms): ", T), % Statistics
    get_count(C),
    bw("Iteratons: ", C),

    shortest_delay(Schedule,_),
    \+ checkScheduleExtendsMaxTime(Schedule).

% obtainShortestSequence(+NCranes)
% Testar todas os possives Horarios para um Num Especifico de GRUAS.
% O Valor é colocado em shortest_delay(-Schedule, Delay)
obtainShortestSequenceForNCranes(NCranes):-
    allVessels(AllVessels),
    createOrderedVesselMatrix(AllVessels,NCranes,VesselMat), % Backtracking here
    createMultipleCraneSchedule(VesselMat,AllCraneSchedule),
    checkMultipleCraneScheduleDelay(AllCraneSchedule, Delay),
    compareShortestScheduleDelay(AllCraneSchedule, Delay),
    increment_count,
    fail.

compareShortestScheduleDelay(AllCraneSchedule, Delay):-
    shortest_delay(_,LowestDelay),
    ((Delay=<LowestDelay,!,setShortestDelay(AllCraneSchedule, Delay));true).


checkScheduleExtendsMaxTime(Schedule):-
    max_time(MaxTimeAllowed),
    checkScheduleExtendsMaxTime1(Schedule,MaxScheduleTime),
    setShortestTime(MaxScheduleTime),
    ((MaxScheduleTime>MaxTimeAllowed,fail)).

checkScheduleExtendsMaxTime1([],0).
checkScheduleExtendsMaxTime1([CraneSchedule|Schedule], NewMaxScheduleTime):-
    checkScheduleExtendsMaxTime1(Schedule, MaxScheduleTime),
    last(CraneSchedule, (Vessel,TSUnloading, TimeEndLoading)),
    (
        (TimeEndLoading>MaxScheduleTime,!,NewMaxScheduleTime is TimeEndLoading)
        ; NewMaxScheduleTime is MaxScheduleTime
    ).

