:-['Schedule.object.pl'].
:-['ListUtils.pl'].
:-['vessels.pl']. % Example Vessels
:-['scheduling_vessels_1.pl']. % Given Code
:-['BulkWrite.pl'].
:-['Counter.object.pl'].

:- autoload(library(lists), [append/3, member/2, last/2]).

max_docks(1). % FIXME: SPRINT B só considera 1 doca.

% ---- PREDICADOS DINAMICOS

% Current Shortest Schedule and Interval
% shortest_delay(Sequence, Delay)
:- dynamic shortest_delay/2.
shortest_delay(_, 999999).
setShortestDelay(NewSchedule, NewDelay):-
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

% ============================================================
% PROGRAMA
% ============================================================

run:-
    obtainShortestSequence(ShortestSchedule, ShortestDelay, ShortestTime),
    writeSchedule(ShortestSchedule, ShortestDelay, ShortestTime).

obtainShortestSequence(ShortestSchedule, ShortestDelay, ShortestTime):-
    obtainShortestSequence1,
    shortest_delay(ShortestSchedule, ShortestDelay),
    shortest_time(ShortestTime).

obtainShortestSequence1:-
    max_docks(MaxDocks), % Get Max Docks
    range(MaxDocks, RL),!, % Gen List of [1,2,...,MaxDocks]

    member(NDocks, RL), % Get Value from RL
    bw(["\nCalculating for ", NDocks, " Dock(s)\n\n"]),

    reset_timer, % Statistics
    start_timer, % Statistics

    reset_count,
    start_count,
    (obtainShortestSequenceForNDocks(NDocks); true),

    get_elapsed_time(T), % Statistics
    bw("Time Taken (ms): ", T), % Statistics
    get_count(C),
    bw("Iteratons: ", C),

    shortest_delay(Schedule,_),
    \+ checkScheduleExtendsMaxTime(Schedule).

% obtainShortestSequence(+NDocks)
% Testar todas os possiveis Horarios para um Num Especifico de DOCAS.
% O Valor é colocado em shortest_delay(-Schedule, Delay)
obtainShortestSequenceForNDocks(NDocks):-
    allVessels(AllVessels),
    createOrderedVesselMatrix(AllVessels, NDocks, VesselMat), % Backtracking here
    createMultipleDockSchedule(VesselMat, AllDockSchedule),
    checkMultipleDockScheduleDelay(AllDockSchedule, Delay),
    compareShortestScheduleDelay(AllDockSchedule, Delay),
    fail.

compareShortestScheduleDelay(AllDockSchedule, Delay):-
    shortest_delay(_, LowestDelay),
    ((Delay =< LowestDelay, !, setShortestDelay(AllDockSchedule, Delay)); true).

checkScheduleExtendsMaxTime(Schedule):-
    max_time(MaxTimeAllowed),
    checkScheduleExtendsMaxTime1(Schedule, MaxScheduleTime),
    setShortestTime(MaxScheduleTime),
    ((MaxScheduleTime > MaxTimeAllowed, fail)).

checkScheduleExtendsMaxTime1([], 0).
checkScheduleExtendsMaxTime1([DockSchedule|Schedule], NewMaxScheduleTime):-
    checkScheduleExtendsMaxTime1(Schedule, MaxScheduleTime),
    last(DockSchedule, (Vessel, _, TimeEndLoading)),
    (
        (TimeEndLoading > MaxScheduleTime, !, NewMaxScheduleTime is TimeEndLoading)
        ; NewMaxScheduleTime is MaxScheduleTime
    ).

% ============================================================
% VESSEL MATRIX AND SCHEDULING
% ============================================================

%
% [
%     [vessel1, vessel2], % Dock1
%     [vessel3], % Dock2
%     [vessel4], % Dock3
% ]
createOrderedVesselMatrix(Vessels, NDocks, VesselMatrix):-
    createdOrderedVesselMatrix1(Vessels, NDocks, 1, VesselMatrix).

% FIXME: VERIFICAR SE EXISTEM DUPLICADOS NO findall
createdOrderedVesselMatrix1(Vessels, NDocks, NDocks, [Vessels]).
createdOrderedVesselMatrix1(Vessels, NDocks, Depth, [Head|Rest]) :-
    length(Vessels, NVessels),
    NDocks > NVessels,
    Depth < NDocks,
    append(Head, Tail, Vessels),
    Depth1 is Depth + 1,
    createdOrderedVesselMatrix1(Tail, NDocks, Depth1, Rest).
createdOrderedVesselMatrix1(Vessels, NDocks, Depth, [Head|Rest]) :-
    Depth < NDocks,
    append(Head, Tail, Vessels),
    Head \= [],
    Tail \= [],
    Depth1 is Depth + 1,
    createdOrderedVesselMatrix1(Tail, NDocks, Depth1, Rest).

createMultipleDockSchedule([], []).
createMultipleDockSchedule([OrderedVesselDock|VesselMatrix], [ScheduleOfDock|Schedule]):-
    createMultipleDockSchedule(VesselMatrix, Schedule),
    sequence_temporization(OrderedVesselDock, ScheduleOfDock).

checkMultipleDockScheduleDelay([], 0).
checkMultipleDockScheduleDelay([DockSchedule|RestDockSchedule], NewDelay):-
    checkMultipleDockScheduleDelay(RestDockSchedule, CurrentDelay),
    sum_delays(DockSchedule, Delay),
    NewDelay is Delay + CurrentDelay.
