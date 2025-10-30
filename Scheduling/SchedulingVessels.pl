
:-['Schedule.object.pl'].
:-['ListUtils.pl'].
:-['vessels.pl']. % Example Vessels
:-['scheduling_vessels_1.pl']. % Given Code 
:-['BulkWrite.pl'].

max_cranes(5).

% Current Shortest Sequence and Interval
% shortest_delay(Sequence, Delay)
:-dynamic shortest_delay/2.
shortest_delay(_,999999).
setShortestDelay(NewSchedule, NewDelay):-
    bw(["New Delay: ", NewDelay, '\n']),
    retract(shortest_delay(_,_)),
    asserta(shortest_delay(NewSchedule, NewDelay)).
    

% Interval Size to be analized
% Default Value 167, 24*7 (a week) 
:-dynamic max_time/1.
max_time(167). 
setMaxTime(NewValue):-
    retract(max_time(_)),
    asserta(max_time(NewValue)).


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


checkMultipleCraneScheduleDelay([], 9999999).
checkMultipleCraneScheduleDelay([CraneSchedule|RestCraneSchedule], NewDelay):-
    checkMultipleCraneScheduleDelay(RestCraneSchedule, CurrentDelay),
    sum_delays(CraneSchedule, Delay),
    NewDelay is Delay + CurrentDelay.


obtainShortestSequence(ShortestSchedule, ShortestDelay):-
    max_cranes(MaxCranes),
    max_time(MaxTime),

    range(MaxCranes, RL),
    member(NCranes, RL),
    bw("\nCranes ", NCranes),

    (obtainShortestSequence1(NCranes);true),

    % TODO: Verificar se horario obtido acaba antes de max_time

    shortest_delay(ShortestSchedule, ShortestDelay).
    %writeSchedule(ShortestSchedule, ShortestDelay)


% obtainShortestSequence(+NCranes, -AllCraneSchedule, -Delay)
obtainShortestSequence1(NCranes):-
    allVessels(AllVessels),
    createOrderedVesselMatrix(AllVessels,NCranes,VesselMat), % Backtracking here
    createMultipleCraneSchedule(VesselMat,AllCraneSchedule),
    checkMultipleCraneScheduleDelay(AllCraneSchedule, Delay),
    compareShortestScheduleDelay(AllCraneSchedule, Delay),
    fail.

compareShortestScheduleDelay(AllCraneSchedule, Delay):-
    shortest_delay(_,LowestDelay),
    ((Delay<LowestDelay,!,setShortestDelay(AllCraneSchedule, Delay));true).