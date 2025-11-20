:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../vessels.pl',
    '../../CraneScheduling.pl',
    '../vars.pl',
    '../vesselAdj.pl'
]).

nCranes(NCranes):-map:map("nCranes", NCranes).
:-nCranes(1).




branchAndBoundExplore(NCranes, VisionRange, Vessels, SSeq, LSeq):-
    visionRange(VisionRange),
    branchAndBoundExplore(NCranes, Vessels, Seq, Delay).

branchAndBoundExplore(NCranes, Vessels, SSeq, SDelay):-
    shortest_delay([],9999999),
    longest_delay([],0),
    medium_delay(0,0,0),
    nPermutations(0),

    nCranes(NCranes),
    random_permutation(Vessels, VesselsR),!,
    (branchAndBoundExplore1([(0,[VesselsR])], 30, 0);true),!,

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
    branchAndBoundExplore1(TodosOrd, MaxDepth, D1).
    

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


    

test(NV, NC, VRange):- visionRange(VRange), test(NV, NC).    
test(NV, NC):-

    range(NV, NVList),

    nCranes(NC),
    
    ProgramTimerID is 0001,
    reset_timer(ProgramTimerID), start_timer(ProgramTimerID),!,

    ((
        member(NVessels, NVList),
        allVessels(NVessels, ListOfVessels),
        bw("\n", "----------------------"),
        bw("Number of Vessels: ", NVessels),

        VesselTimerID = "vesselID", reset_timer(VesselTimerID), start_timer(VesselTimerID),

        ((
            test1(NC,ListOfVessels,Delay),
            get_elapsed_time(VesselTimerID, Time),
            bw(["\n\nCalculation Time for ", NVessels, " Vessels (ms): ", Time,"\n"])
        );true),
        
        fail
        
    ); true), bw("\n\n").

test1(NC,ListOfVessels,Delay):-
    range(NC,NCTEMP),!, member(NCranes,NCTEMP),

    CraneTimerID = "CraneTimerID", 
    reset_timer(CraneTimerID), start_timer(CraneTimerID),
    
    format("Cranes: ~|~d~3+ ", [NCranes]),
    branchAndBoundExplore(NCranes, ListOfVessels,_,Delay),
    format("; Delay: ~|~d~5+", [Delay]),
    
    
    get_elapsed_time(CraneTimerID, TimeCr),
    format("; Calc. Time: ~t~d~8| ms~n", [TimeCr]),

    Delay=<0,!.

    