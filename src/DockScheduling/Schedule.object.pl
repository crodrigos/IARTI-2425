
:- module(schedule, []).



:-use_module('../Utils/BetterWrite').

:-use_module(library(assoc)).



% Schedule - 
% [
% [Crane1, [ [boat1, T_Loading, T_Unloading, T_E] | ...]],
% [Crane2, [ [boat5, T_Loading, T_Unloading, T_E] | ...]],
% ...
% [CraneX, [ [boatX, T_Loading, T_Unloading, T_E] | ...]],
% ]

%! addVesselToSchedule(+Crane, +VesselSchedule, +CurrentSchedule, -FullSchedele).
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






writeDockDailySchedules(Vessels) :-
    findall(EventPair,
        (
            member((Vessel, Arr, Dep), Vessels),
            day_event(Vessel, Arr, arrival, DayA, EventA),
            day_event(Vessel, Dep, departure, DayD, EventD),
            ((Day = DayA, Event=EventA);(Day = DayD, Event=EventD)),
            pairs_keys_values(EventPair, [Day], [Event])
        ),
        EventsTemp
    ),
    findall(EventKV, member([EventKV],EventsTemp), Events),
    group_pairs_by_key(Events, EventsGrouped),
    print_days(EventsGrouped).
    
% Helper to convert hour to day and text
day_event(Vessel, Hour, Type, Day, [EventText]) :- 
    Day is (Hour // 24) + 1,
    HourInDay is Hour mod 24,
    format(atom(EventText), "~w ~w at hour ~w", [Vessel, Type, HourInDay]).

% Group (Day,Event) pairs into [(Day,[Event,...]), ...]
group_events_by_day(Events, Grouped) :-
    sort(Events, Sorted),
    group_pairs_by_key(Sorted, Grouped).

% Print everything nicely
print_days([]).
print_days([Day-Events|Rest]) :-
    format("Day ~w:\n", [Day]),
    forall(member([E], Events), format("  - ~w\n", E)),
    nl,
    print_days(Rest).
