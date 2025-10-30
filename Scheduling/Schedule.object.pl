
:-['BulkWrite.pl'].

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



writeSchedule(AllCraneSchedule, Delay):-
    bw(['Delay: ', Delay, '\n']),    
    writeSchedule1(AllCraneSchedule),
    bw('\n\n'),!.

writeSchedule1([]):-!.
writeSchedule1([CraneSchedule|Schedule]):-   
    write(CraneSchedule), 
    writeCraneSchedule(CraneSchedule),
    writeSchedule1(Schedule).

writeCraneSchedule([]):-!.
writeCraneSchedule([[Vessel, StUnloading, EnLoading]|RestSchedule]):-
    bw(['Vessel: ']),
    bw([Vessel, '  ', StUnloading, ' - ', EnLoading]),
    writeCraneSchedule(RestSchedule).