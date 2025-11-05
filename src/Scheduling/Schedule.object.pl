
:-use_module('../Utils/BetterWrite').

% Schedule - 
% [
% [Crane1, [ [boat1, T_Loading, T_Unloading, T_E] | ...]],
% [Crane2, [ [boat5, T_Loading, T_Unloading, T_E] | ...]],
% ...
% [CraneX, [ [boatX, T_Loading, T_Unloading, T_E] | ...]],
% ]

% addVesselToSchedule(+Crane, +VesselSchedule, +CurrentSchedule, -Ve)
addVesselToSchedule(Crane, VesselSchedule, [], [[Crane, [VesselSchedule]]]).
addVesselToSchedule(Crane, VesselSchedule,
    [[Crane, CraneSchedule] | Rest],
    [[Crane, NewCraneSchedule] | Rest]) :-
    append([VesselSchedule], CraneSchedule, NewCraneSchedule), !.
addVesselToSchedule(Crane, VesselSchedule,
    [[OtherCrane, OtherSchedule] | Rest],
    [[OtherCrane, OtherSchedule] | NewRest]) :-
    addVesselToSchedule(Crane, VesselSchedule, Rest, NewRest).

createEmptySchedule(0, []):-!.
createEmptySchedule(NCranes,[[NCranes,[]]|Rest]):-
    N1Cranes is NCranes-1,
    createEmptySchedule(N1Cranes, Rest).



writeSchedule(AllCraneSchedule, Delay, Time) :- !,
    bw(['Delay: ', Delay, '\n']),
    bw(['Time: ', Time, '\n']),
    writeSchedule1(1, AllCraneSchedule),
    bw('\n\n'), !.

writeSchedule1(_, []) :- !.
writeSchedule1(NCrane, [[]| RestSchedules]) :-
    bw(['Crane ', NCrane, ': EMPTY \n']),
    N1Crane is NCrane + 1,
    writeSchedule1(N1Crane, RestSchedules).
writeSchedule1(NCrane, [CraneSchedule | RestSchedules]) :-
    bw(['Crane ', NCrane, ':\n']),
    writeCraneSchedule(CraneSchedule),
    N1Crane is NCrane + 1,
    writeSchedule1(N1Crane, RestSchedules).

writeCraneSchedule([]) :- !.
writeCraneSchedule([(Vessel, StUnloading, EnLoading) | RestSchedule]) :-
    bw(['\tVessel: ', Vessel, '  ', StUnloading, ' - ', EnLoading, '\n']),
    writeCraneSchedule(RestSchedule).