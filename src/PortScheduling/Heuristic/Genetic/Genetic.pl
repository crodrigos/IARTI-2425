:-module(geneticPort, []).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../../Heuristic/AStar/AStar.pl',
    '../../MultipleDocks.pl',
    '../../../Heuristic/Genetic/Genetic.pl'
]).

generateInitialPopulation(_,_, 0, []).
generateInitialPopulation(VesselList, DockList, PopulationSize, [Schedule|RestSchedule]):-
    S1 is PopulationSize-1,
    generateInitialPopulation(VesselList, DockList, S1, RestSchedule),
    splitVesselListInDocksRand(VesselList,DockList, Schedule).