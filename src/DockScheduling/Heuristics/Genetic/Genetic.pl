:- module(geneticDock, []).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../vessels.pl',
    '../../CraneScheduling.pl',
    '../../vars.pl',
    '../../../Heuristic/Genetic/Genetic.pl'
]).
:- use_module(library(lists)).
:- use_module(library(quintus)).
:- use_module(library(random)).

population_size(N):-map:map("gen_popsize", N).
:-population_size(10).

mutation_probability(P):-map:map("gen_mutprob", P).
:-mutation_probability(0.03).

crossover_probability(P):-map:map("gen_crossprob", P).
:-crossover_probability(0.7).

max_generations(G):-map:map("gen_maxgens", G).
:-max_generations(60).

nullValue("NULL").

nCranes(NCranes):-map:map("gen_ncranes", NCranes).
:-nCranes(3).

% GEN INICIAL POP
% REPETIR
/**
 * GEN NOVA POPULAÇÃO:
 * 1. CRUZAMENTO
 * 2. MUTAÇÃO
 * 4. REMOVER REPETIÇÕES
 * 3. AVALIAR MELHORES
 */
% REPETIR,

% TODO: Add Generic Goal for Mutation, Crossover and Evaluation
genetic(
        VesselList, NCranes, 
        MaxGenerations, PopulationSize, 
        CrossProb, MutProb, 
        Best, Delay
    ):-
        nCranes(NCranes),
        generateInitialPopulation(VesselList, PopulationSize, InitialPop),
        genetic:genetic(
            InitialPop,
            MaxGenerations, PopulationSize,
            geneticDock:orderCrossover, geneticDock:mutate, geneticDock:evaluate,
            CrossProb, MutProb,
            FinalPopulation
        ),
        [(Delay,Best)|_] = FinalPopulation.

mutate([V], [V]):-!.
mutate(VesselSequence, VesselSequenceMutated):-
    length(VesselSequence,L),
    L1 is L-1,
    random(0, L1, P),
    mutate1(P,0,VesselSequence,VesselSequenceMutated).
    
    
mutate1(_,_,[V], [V]):-!.
mutate1(P1,C,[H1,H2|T], [M1,M2|MT]):-
    ((C>=P1)->
        H1=M2,H2=M1,T=MT,!
    ;
        H1=M1,
        C1 is C+1,
        mutate1(P1,C1,[H2|T], [M2|MT])
    ).

evaluate(VesselL, Delay):-
    nCranes(NCranes),
    vesselSequenceDelay(VesselL, NCranes, _, Delay).

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


% generateInitialPopulation1(_,Size,Pop):-
%     length(Pop,L2),
%     Size=L2,!.

generateInitialPopulationRand(_,0,[]):-!.
generateInitialPopulationRand(ListOfVessels,Size,[Permutation|Rest]):-
    Size1 is Size-1,
    generateInitialPopulation(ListOfVessels, Size1, Rest),
    ((
        random_permutation(ListOfVessels, Permutation),
        \+member(Permutation,Rest),!
    );true).


generateInitialPopulation(V,1,[V]):-!.
generateInitialPopulation(V,Size,[V|Rest]):-
    Size1 is Size-1,
    generateInitialPopulation(V,Size1,Rest),!.
generateInitialPopulation(V,_,[V]):-!.