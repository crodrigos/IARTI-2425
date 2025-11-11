:- use_module(["Genetic.pl", "../../../Utils/Timer.object.pl"]).

mutationValues([
    0,
    0.01,
    0.02,
    0.05,
    0.1,
    0.2,
    0.3,
    0.4,
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1    
]).

crossoverValues([
    0,
    0.01,
    0.02,
    0.05,
    0.1,
    0.2,
    0.3,
    0.4,
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1  
]).

nVessels([
    50,
    100,
    130
]).

nCranes([10,20]).

nGenerations([
    20,
    50,
    100    
]).

test:-
    nGenerations(NGenerationL),
    nVessels(NVesselsL),
    nCranes(NCranesL),
    crossoverValues(CrossL),
    mutationValues(MutationL),

    format("Vessels,Cranes,Generations,Crossover,Mutation,BestDelay,TimeMS~n"),!,


    findall(_,
        (
            member(MaxGeneration,NGenerationL),

            member(NVessels, NVesselsL),
            genetic:allVessels(NVessels, VesselList),

            member(NCranes, NCranesL),

            member(CrossProb, CrossL),
            member(MutProb, MutationL),

            testsingle(VesselList, NCranes, MaxGeneration, CrossProb, MutProb, Best, Delay, Time)
            % sleep(0.5)
            
        ), 
        Bag
    ).
    


testsingle(VesselList, NCranes, 
        MaxGeneration, CrossProb, MutProb,
        Best, Delay, Time):-
    reset_timer, start_timer,

    genetic(VesselList, NCranes, MaxGeneration, CrossProb, MutProb, _, Delay),
    
    get_elapsed_time(Time),

    length(VesselList, NVessels),

    format("~w,~w,~w,~2f,~2f,~w,~w~n", [NVessels, NCranes, MaxGeneration, CrossProb, MutProb, Delay, Time]).


toFile:-
    get_time(T),format_time(string(Text),"%d-%e-%Y %H-%M",T),
    format(atom(FileName), "reports/Genetic~w.csv", [Text]),
    writeln(FileName),
    tell(FileName),
    test,
    halt.
