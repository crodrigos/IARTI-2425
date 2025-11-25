
:- module(branchAndBound, [branchAndBound/6]).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../vessels.pl',
    '../../CraneScheduling.pl',
    '../../vars.pl',
    '../GraphTraversal/vesselAdj.pl'
]).

nCranes(NCranes):-map:map("nCranes", NCranes).
:-nCranes(1).




branchAndBound(NCranes, VisionRange, Vessels, Depth, Seq, Delay):-
    visionRange(VisionRange),
    branchAndBound(NCranes, Vessels, Depth, Seq, Delay).

branchAndBound(NCranes, Vessels, Depth, SSeq, SDelay):-
    shortest_delay([],9999999),
    longest_delay([],0),
    medium_delay(0,0,0),
    nPermutations(0),

    nCranes(NCranes),!,

    (branchAndBoundExplore1([(0,[Vessels])], Depth, 0);true),!,

    shortest_delay(SSeq, SDelay).
    % longest_delay(LSeq, LDelay),
    % medium_delay(_,_,MDelay),
    % nPermutations(N),

    % bw("   Shortest Delay: ", SDelay),
    % bw("    Longest Delay: ", LDelay),
    % bw("     Medium Delay: ", MDelay),

    % bw(" Permutation Made: ", N).

    
    
branchAndBoundExplore1(_, MaxDepth, MaxDepth):-!.
branchAndBoundExplore1([(_,Caminho)|Outros], MaxDepth, Depth):-
    [CurrVesselL|_] =  Caminho,
    % bw("Depth: ", Depth),
    % bw("Custo: ", Custo),
    % bw("First: ", CurrVesselL),
    compareAll(CurrVesselL),!.

branchAndBoundExplore1([(Custo,Caminho)|Outros], MaxDepth, Depth):-
    [CurrVesselL|_] =  Caminho,
    nCranes(NCranes),
    findall(
        (NewCusto, [PermutationSeq | Caminho]),
        (
            adjacentWeighedPermutation(CurrVesselL,NCranes,PermutationSeq, W),
            \+ member(PermutationSeq,Caminho),
            NewCusto is Custo+W
        ),
        Novos
    ),
    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    
    pruneExcess(TodosOrd, TodosPruned),

    D1 is Depth + 1,
    branchAndBoundExplore1(TodosPruned, MaxDepth, D1).
    

compareAll(Vessels):-
    nCranes(NCranes),
    sequenceTemporization(Vessels, NCranes, Seq),
    sumDelays(Seq, D),

    compareShortestDelay(Seq, D),
    compareLongestDelay(Seq, D),
    compareMediumDelay(D),

    (fail;(D==0)).


pruneExcess(Full, Pruned) :-
    Max=120,         % user-defined limit
    length(Full, Len),
    (   Len =< Max
    ->  Pruned = Full
    ;   % otherwise cut the list
        Half is Max // 2,
        length(Pruned, Half),
        append(Pruned, _, Full)
    ).


    

