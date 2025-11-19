:- use_module(["Genetic.pl", "../../../Utils/Timer.object.pl"]).

mutationValues(L):-
    gen_list(0, 1, 0.02, L1),
    gen_list(0,0.1,0.01, L2),
    append(L1, L2, L).
    

crossoverValues(L):-
    gen_list(0, 1, 0.05, L).

generations(L):-
    gen_list(30,120,30,L).

cranesX([
    0.2,
    0.3,
    0.5,
    0.7,
    1
]).

% ("title",VESSEL, CRANE, GENERATIONS)
situacions([
    % ("Few Vessels, Some Cranes, Good Generations",10,10,10), 
    % ("Some Vessels, Few Cranes, Few Generations",20,5,10),
    % ("Some Vessels, Some Cranes, Good Generations",20,10,50), 
    % ("Some Vessels, Few Cranes, Very Good Generations",20,15,200),
    % ("A Lot of Vessels, Not enough cranes, Very Good Generations",50,20,200),
    ("Situation 1",50,_,_)
    % ("Situation 1",50,20,200)
]).

test:-
    situacions(SituacionsL),

    cranesX(CraneXL),

    crossoverValues(CrossL),
    mutationValues(MutationL),

    generations(GenerationsL),

    format("Title,Vessels,Cranes,Generations,Crossover,Mutation,BestDelay,TimeMS~n"),!,

    genetic:range(1,RepeatL),

    findall(_,
        (
            member((Title, NVessels, _, _), SituacionsL),
            genetic:allVesselsRandom(NVessels, VesselList),

            member(MaxGeneration, GenerationsL),

            member(CraneX,CraneXL),
            NCranes is round(CraneX*NVessels),

            member(CrossProb, CrossL),
            member(MutProb, MutationL),

            member(M_, RepeatL),

            testsingle(VesselList, NCranes, MaxGeneration, CrossProb, MutProb, Title, Best, Delay, Time)
        ), 
        Bag
    ).
    


testsingle(VesselList, NCranes, 
        MaxGeneration, CrossProb, MutProb, Title,
        Best, Delay, Time):-
    reset_timer, start_timer,

    genetic(VesselList, NCranes, MaxGeneration, CrossProb, MutProb, _, Delay),
    
    get_elapsed_time(Time),

    length(VesselList, NVessels),

    format("\"~w\",~w,~w,~w,~2f,~2f,~w,~w~n", [Title,NVessels, NCranes, MaxGeneration, CrossProb, MutProb, Delay, Time]).


toFile:-
    get_time(T),format_time(string(Text),"%d-%e-%Y %H-%M",T),
    format(atom(FileName), "reports/Genetic~w.csv", [Text]),
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
