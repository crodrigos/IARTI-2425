:- module(geneticPort, []).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../../Heuristic/AStar/AStar.pl',
    '../../MultipleDocks.pl',
    '../../../Heuristic/Genetic/Genetic.pl'
]).

genetic(
    VesselList, DockList,
    MaxGenerations, PopulationSize, 
    CrossProb, MutProb,
    StagnationMinumum, StagnationAnalysisLength,
    Best, Delay
):-
    generatePopulation(VesselList, DockList, PopulationSize, Population),
    genetic:genetic(
        Population,
        MaxGenerations, PopulationSize,
        geneticPort:crossover, geneticPort:mutate,
        geneticPort:evaluate,
        CrossProb, MutProb,
        StagnationMinumum, StagnationAnalysisLength,
        FinalPopulation   
    ),
    [(Delay,Best)|_] = FinalPopulation.


genetic(
    VesselList, DockList,
    MaxGenerations, PopulationSize, 
    CrossProb, MutProb,
    Best, Delay
):-
    generatePopulation(VesselList, DockList, PopulationSize, Population),
    genetic:genetic(
        Population,
        MaxGenerations, PopulationSize,
        geneticPort:crossover, geneticPort:mutate,
        geneticPort:evaluate,
        CrossProb, MutProb,
        FinalPopulation   
    ),
    [(Delay,Best)|_] = FinalPopulation.




generatePopulation(VesselList, DockList, PopulationSize, ScheduleList):-
    var(PopulationSize),!,
    generatePopulation(VesselList, DockList, 20, ScheduleList).
generatePopulation(_, _, 0, []):-!.
generatePopulation(VesselList, DockList, PopulationSize, [Schedule|RestSchedule]):-
    splitVesselListInDocksRand(VesselList, DockList, Schedule),
    PopSize1 is PopulationSize-1,
    generatePopulation(VesselList, DockList, PopSize1, RestSchedule).



evaluate(Schedule, Delay):-
    scheduleTemporizationAndDelay(Schedule, _, _, Delay).



mutate(Schedule, Mutated):-
    reorderSchedule(Schedule, VL, DL, VNL),
    length(VL,L),
    L1 is L-1,
    random(0, L1, P),
    mutateListVessels(P,0,VL,VLMutated),
    reorderSchedule(Mutated, VLMutated, DL, VNL).

    
mutateListVessels(_,_,[V], [V]):-!.
mutateListVessels(P1,C,[H1,H2|T], [M1,M2|MT]):-
    ((C>=P1)->
        H1=M2,H2=M1,T=MT,!
    ;
        H1=M1,
        C1 is C+1,
        mutateListVessels(P1,C1,[H2|T], [M2|MT])
    ).





crossover(Sch1, Sch2, Offspring1, Offspring2):-
    reorderSchedule(Sch1, Parent1, Docks1, VesselNL1),
    reorderSchedule(Sch2, Parent2, Docks2, VesselNL2),

    orderCrossover(Parent1, Parent2, O1, O2),

    splitVesselListInDocks(O1, Docks2, Offspring1),
    splitVesselListInDocks(O2, Docks1, Offspring2).

nullValue("NULL").

orderCrossover(Seq1, Seq2, D1, D2):-
    length(Seq1, L),

    getRandomCrossoverPoints(L, CrossStart, CrossEnd),

    listutils:subset_range(Seq1, CrossStart, CrossEnd, Seq1Keep),
    listutils:subset_range(Seq2, CrossStart, CrossEnd, Seq2Keep),

    subsituteIfNotInList(Seq1, Seq1Keep, Seq1Fill),
    subsituteIfNotInList(Seq2, Seq2Keep, Seq2Fill),

    subtract(Seq1, Seq2Keep, Seq1Switch),
    subtract(Seq2, Seq1Keep, Seq2Switch),
    
    fillNULLWith(Seq1Fill, Seq2Switch, D1),
    fillNULLWith(Seq2Fill, Seq1Switch, D2).

getRandomCrossoverPoints(N,Start,End):-
    CrossMax is N-1,
    random(0, CrossMax, Cross1),
    random(0, CrossMax, Cross2),
    ((Cross2>Cross1) -> 
        Start=Cross1, End=Cross2,! ;
        getRandomCrossoverPoints(N,Start,End)
    ),!.  

fillNULLWith([], _, []).
fillNULLWith([H|T], [SubsH|SubsT],[OUT|Tail]):-
    nullValue(NULL),
    ((H==NULL)->
        OUT=SubsH,!,
        fillNULLWith(T,SubsT, Tail)
    ;
        OUT=H,!,
        fillNULLWith(T,[SubsH|SubsT],Tail)
    ).

subsituteIfNotInList([],_,[]).
subsituteIfNotInList([H|T], V, [Output|Rest]):-
    nullValue(NULL),
    ((member(H,V),Output=H,!);Output=NULL),
    subsituteIfNotInList(T,V,Rest).




reorderSchedule([], [], [], []).
reorderSchedule([(D,L)|SchRest], OL, [D|DocksRest], [Len|VesselNRest]):-
    reorderSchedule(SchRest, OrderedRest, DocksRest, VesselNRest),
    length(L, Len),
    append([L, OrderedRest],OL).