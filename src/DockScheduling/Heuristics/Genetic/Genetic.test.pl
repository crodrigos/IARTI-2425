:- use_module([
    "Genetic.pl", 
    "../../vessels.pl",
    "../../../Utils/Timer.object.pl"
]).

mutationValues(VL):-
    %gen_list(0.1, 1, 0.1, L1),
    %gen_list(0.01,0.1,0.01, L2),
    %append(L1, L2, L).
    gen_list(0,0.21,0.05, L),
    select(0, L, VL),!.
    

population_size(L):-
    gen_list(10, 50, 10, L).

crossoverValues(L):-
    gen_list(0.4, 0.9, 0.1, L).

generations(L):-
    %gen_list(30,120,30,L).
    L = [50,80].

cranesX([
    1
]).

situations(L):-
    findall(V, vessels:situation(V,_,_),L).

test:-
    situations(SituationsL),
    cranesX(CraneXL),
    crossoverValues(CrossL),
    mutationValues(MutationL),
    generations(GenerationsL),
    population_size(PopSizeL),

    format("Title,Vessels,Cranes,Generations,PopulationSize,Crossover,Mutation,BestDelay,TimeMS~n"),!,

    genetic:range(2,RepeatL),

    findall(_,
        (
            member(Sit, SituationsL),
            vessels:situation(Sit, Title, VesselList),
            
            length(VesselList, NVessels),
            
            member(NCranes,CraneXL),
            member(MaxGeneration, GenerationsL),
            member(PopSize, PopSizeL),
            member(CrossProb, CrossL),
            member(MutProb, MutationL),

            member(_M, RepeatL),

            testsingle(
                VesselList, NCranes, 
                PopSize, MaxGeneration, 
                CrossProb, MutProb, 
                Title, 
                Best, Delay, 
                Time
            )
        ), 
        Bag
    ).
    


testsingle(
        VesselList, NCranes, 
        PopSize, MaxGeneration, 
        CrossProb, MutProb, 
        Title,
        Best, Delay, 
        Time
    ):-
    
    reset_timer, start_timer,
    geneticDock:genetic(
        VesselList, NCranes,
        MaxGeneration, PopSize, 
        CrossProb, MutProb,
         _, Delay
    ),
    get_elapsed_time(Time),

    length(VesselList, NVessels),

    format("\"~w\",~w,~w,~w,~w,~2f,~2f,~w,~w~n", [Title,NVessels, NCranes, MaxGeneration,PopSize,CrossProb, MutProb, Delay, Time]).


toFile:-
    get_time(T),format_time(string(Text)," %d-%e-%Y %H-%M",T),
    format(atom(FileName), "reports/Genetic/Genetic~w.csv", [Text]),
    writeln(FileName),
    tell(FileName),
    test,
    halt.














%%%%%%%%%%%%%


gen_list(Start, End, Step, List) :-
    gen_list_acc(Start, End, Step, [], Rev),
    reverse(Rev, Raw),
    round_list(Raw, List).

gen_list_acc(Current, End, _, Acc, Acc) :-
    Current > End, !.

gen_list_acc(Current, End, Step, Acc, List) :-
    Next is Current + Step,
    gen_list_acc(Next, End, Step, [Current | Acc], List).

round_list(L, Rounded) :-
    maplist(round2, L, Rounded).

round2(X, Y) :-
    Y is round(X*100) / 100.
