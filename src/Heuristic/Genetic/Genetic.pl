:- module(genetic, [genetic/9]).


:- use_module([
    '../../Utils/Timer.object.pl',
    '../../Utils/Map.object.pl',
    '../../Utils/ListUtils.pl',
    '../../Utils/BetterWrite.pl'
]).

population_size(N):-map:map("gen_popsize", N).
:-population_size(10).

max_generations(G):-map:map("gen_maxgens", G).
:-max_generations(60).

mutation_probability(P):-map:map("gen_mutprob", P).
:-mutation_probability(0.03).

mutation_predicate(P):-map:map("gen_mutation_predicate", P).

crossover_probability(P):-map:map("gen_crossprob", P).
:-crossover_probability(0.7).

crossover_predicate(P):-map:map("gen_crossover_predicate", P).

evaluation_predicate(P):-map:map("gen_evalutation_predicate", P).

last_generations(L):-map:map("gen_last_generations",L).
:-last_generations([]).

genetic(
        InitialPopulation,
        MaxGenerations, PopulationSize,
        CrossoverPredicate, MutationPredicate, EvalutationPredicate,
        CrossoverProbability, MutationProbability, 
        FinalPopulation
    ):-
        crossover_probability(CrossoverProbability),
        mutation_probability(MutationProbability),

        crossover_predicate(CrossoverPredicate),
        mutation_predicate(MutationPredicate),
        evaluation_predicate(EvalutationPredicate),

        population_size(PopulationSize),

        generateInitialPopulation(InitialPopulation, InitialPopulationEval),

        genetic1(InitialPopulationEval, MaxGenerations, 0,  FinalPopulation).


% genetic1(Population,_,_,Final):-
%     [Best|_] = Population,
%     nCranes(NCranes),
%     vesselSequenceDelay(Best, NCranes, _, Delay),
%     Delay==0,!,Population=Final.

genetic1(P,G,G,P):-!.
genetic1(P,_,G,P):-
    [(Fitness,_)|_] = P,
    bw(["G: ", G, "| F: ", Fitness,"\n"]),
    Fitness=0, !.
genetic1(Population, MaxGenerations, CurrentGeneration, Final):-
    
    genNewPopulation(Population, NewPopulation),
    mutatePopulation(NewPopulation,MutatedPop),

    evalutateAndTrimPopulation(MutatedPop, NewGen),    

    C1 is CurrentGeneration+1,
    genetic1(NewGen, MaxGenerations, C1, Final).

generateInitialPopulation(InitialPop, InitialPopEval):-
    findall((Eval,El), 
        (member(El,InitialPop), evaluate(El, Eval)),
        InitialPopEval).

mutatePopulation(Population, MutatedPop):-
    findall(M,
        (
            mutatePopulation1(Population,M)
        ), 
        MutatedPop).

mutatePopulation1(Population, Mutated):-
    select(El, Population, _), 
    mutateElement(El,Mutated).

mutateElement((F,El), (MutatedFit, MutatedEl)):-
    random(PROB),
    mutation_probability(MUTATE_PROB),
    (
        ((PROB =< MUTATE_PROB) -> 
            (
                mutation_predicate(MutatePred),!,
                call(MutatePred, El, MutatedEl),
                evaluate(MutatedEl, MutatedFit)
            )
        ;
            MutatedEl = El,
            MutatedFit = F
        )
    ).

genNewPopulation(Population, NewPopulation):-
    population_size(PSize),
    findall(Out, 
        (   
            range(PSize, SL),
            member(_Count, SL),
            genNewPopulation1(Population, Out)
        ), 
        Bag),
    append(Bag, NewPopulation_temp),
    append(Population, NewPopulation_temp, NewPopulation).

genNewPopulation1(Population, OUT):-
    
    TournamentSize = 4,

    tournamentSelect(Population, TournamentSize, Parent1),
    select(Parent1, Population, PopulationRest),
    tournamentSelect(PopulationRest, TournamentSize, Parent2),

    (_, El1) = Parent1,
    (_, El2) = Parent2,

    random(PROB),
    crossover_probability(CROSS_PROB),

    ((PROB =< CROSS_PROB) ->
        crossover_predicate(CrossoverPredicate),
        call(CrossoverPredicate, El1,El2,F1,F2),!,

        evaluate(F1, Eval1),
        evaluate(F2, Eval2),

        OUT = [(Eval1, F1), (Eval2, F2)]
    ;
        OUT = [Parent1, Parent2]
    ).

% tournament_select(+Population, +K, -Winner)
tournamentSelect(Pop, K, Winner) :-
    random_subset(Pop, K, Contestants),
    best_individual(Contestants, Winner).


random_subset([], _, []):-!.
random_subset(_, 0, []):-!.
random_subset(L, N, [S|Rest]):-
    random_select(S, L, LRest),
    N1 is N-1,
    random_subset(LRest, N1, Rest).
    

% best_individual(+List, -Best)
best_individual([X], X).
best_individual([(F1,El1),(F2,El2)|T], Best) :-
    (F1 >= F2 -> Max = (F1,El1) ; Max = (F2,El2)),
    best_individual([Max|T], Best).

evalutateAndTrimPopulation(Population, NewPopulation):-
    population_size(PopulationSize),
    
    sort(0, @=<, Population, PopSorted), % @=< para nao eliminar elementos iguais

    listutils:getFirstXOfList(PopulationSize,PopSorted,NewPopulation).



evaluate(El, Eval):-
    evaluation_predicate(EvalutationPredicate),
    call(EvalutationPredicate, El, Eval).