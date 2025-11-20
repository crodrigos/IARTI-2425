:- module(genetic, [
   genetic/5,
   genetic/7
]).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../vessels.pl',
    '../../CraneScheduling.pl',
    '../vars.pl'
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
        MaxGeneration, CrossProb, MutProb, 
        Best, Delay
    ):-
    mutation_probability(MutProb),
    crossover_probability(CrossProb),
    genetic(VesselList, NCranes, MaxGenerations, Best, Delay).

genetic(VesselList, NCranes, MaxGenerations, Best, Delay):-
    nCranes(NCranes),
    max_generations(MaxGenerations),
    
    generateInitialPopulation(VesselList,InitialPop),
    
    genetic1(MaxGenerations, 0, InitialPop, Pop),
    Pop = [Best|_],
    vesselSequenceDelay(Best, NCranes, Seq, Delay).

genetic1(G,G,P,P):-!.
genetic1(_,_,Population,Final):-
    [Best|_] = Population,
    nCranes(NCranes),
    vesselSequenceDelay(Best, NCranes, _, Delay),
    Delay==0,!,Population=Final.

genetic1(MaxGeneration, CurrGeneration, Population, Final):-
    
    genNewPopulation(Population, NewPopulation),
    mutatePopulation(NewPopulation,MutatePop),

    length(MutatedPop, Lmut), 

    evaluateAndTrimPopulation(MutatePop, NewGen),    

    C1 is CurrGeneration+1,
    genetic1(MaxGeneration, C1, NewGen, Final),!.


genNewPopulation(Population, NewPopulation):-
    findall(Out, genNewPopulation1(Population, Out), Bag),
    append(Bag, NewPopulation_temp),
    append(Population, NewPopulation_temp, NewPopulation).
    

genNewPopulation1(Population, OUT):-
    select(El1, Population,PopulationREST),
    select(El2, PopulationREST,_),

    random(PROB),
    crossover_probability(CROSS_PROB),

    ((PROB >= CROSS_PROB) ->
        orderCrossover(El1,El2,Filho1,Filho2),!,
        OUT = [Filho1, Filho2]
    ;
        OUT = []
    ).

mutatePopulation(Population, MutatedPop):-
    findall(M, mutatePopulation1(Population,M), MutatedPop).

mutatePopulation1(Population, MutatedEl):-
    select(El, Population, _),
    random(PROB),
    mutation_probability(MUTATE_PROB),
    (
        (PROB >= MUTATE_PROB,!, mutate(El, MutatedEl));
        MutatedEl = El
    ).

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
        H1=M1,ç
        C1 is C+1,
        mutate1(P1,C1,[H2|T], [M2|MT])
    ).

evaluateAndTrimPopulation(Population, NewPop):-
    nCranes(NCranes),
    population_size(PopSize),

    findall(D-VesselSeq, (
        member(VesselSeq, Population),
        sequenceTemporization(VesselSeq, NCranes, TripletSequence),
        sumDelays(TripletSequence, D)
    ), PopulationEval),

    keysort(PopulationEval,PopulationSorted),

    listutils:getFirstXOfList(PopSize, PopulationSorted, PopulationBest),

    findall(
        V, 
        (
            member((D-V), PopulationBest)
            % bw("\tDelay: ", D)
        ), 
        NewPop
    ).



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


generateInitialPopulation(ListOfVessels,Population):-
    population_size(PopSize),
    generateInitialPopulation1(ListOfVessels,PopSize,Population).

% generateInitialPopulation1(_,Size,Pop):-
%     length(Pop,L2),
%     Size=L2,!.

generateInitialPopulation1(_,0,[]):-!.
generateInitialPopulation1(ListOfVessels,Size,[Permutation|Rest]):-
    Size1 is Size-1,
    generateInitialPopulation1(ListOfVessels, Size1, Rest),
    ((
        random_permutation(ListOfVessels, Permutation),
        \+member(Permutation,Rest),!
    );true).

testCrossover(D1,D2):-
    allVessels(10, VL),
    random_permutation(VL,L1),
    random_permutation(VL,L2),
    
    writeln(L1),
    writeln(L2),
    orderCrossover(L1,L2,D1,D2).

testGenetic(V,C,Gens,B,D):-
    allVesselsRandom(V,VL),
    genetic(VL,Gens,C,B,D).
