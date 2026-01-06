
:- module(createScheduleService, [getSchedule/6]).

:- use_module([
    library(http/http_server),
    library(http/http_header),
    library(http/http_client),
    library(http/http_json),
    library(http/json),
    library(debug)
]).

:- use_module([
    "../Scheduling/vessels.pl",
    "../Scheduling/PortScheduling/Heuristic/Genetic/Genetic.pl",
    "../Scheduling/Utils/Map.object.pl",
    "../Scheduling/Utils/Timer.object.pl",
    "../Scheduling/Utils/DictPlus.pl",
    "../Scheduling/DockScheduling/LinearPermutations/LinearPermutations.pl",
    "../Scheduling/PortScheduling/MultipleDocks.pl"
]).

docks_list(DockList):-map:map("schedule_service_dock_list", DockList).
:-docks_list([1]).

heuristicmethod(HeuristicMethod):-map:map("schedule_service_heuristic_method", HeuristicMethod).
:-heuristicmethod(eat).

method(Method):-map:map("schedule_service_method", Method).
:-method(genetic).

genetic_settings(GeneticSettings):-map:map("schedule_servic_settingse_genetic", GeneticSettings).


getSchedule(DataDict, Method, Reason, Schedule, Delay, TimeTaken):-
    debug(schedule, '~n Schedule Service ~n', []),
    readInputData(DataDict),


    chooseMethod,
    method(Method),

    reset_timer,start_timer,
    useMethod(Method, ScheduleTemp, Delay, Reason),
    get_elapsed_time(TimeTaken),

    makeReadableSchedule(ScheduleTemp, Schedule),

    debug(schedule, 'Schedule: ~w', [Schedule]),
    debug(schedule, 'Delay: ~d', [Delay]),
    debug(schedule, 'Time Taken (ms): ~d', [TimeTaken]).

makeReadableSchedule(Schedule, ReadableSchedule):-
    scheduleTemporization(Schedule, ReadableSchedule).

chooseMethod:-
    docks_list(Docks),
    length(Docks, NDocks),

    allVessels(Vessels),
    length(Vessels, NVessels),

    chooseMethod1(NDocks, NVessels).

%! chooseMethod1(-NDocks,-NVessels) is det
chooseMethod1(1,NVessels):-
    NVessels=<6,
    method(optimal).

chooseMethod1(1,NVessels):-
    NVessels=<8,
    method(heuristic).

chooseMethod1(_,_):-
    method(genetic).





useMethod(genetic, Schedule, Delay, Reason):-
    debug(schedule, 'Using method: ~w', [genetic]),
    useGenetic(Schedule, Delay, Reason).

useMethod(optimal, Schedule, Delay, Reason):-
    debug(schedule, 'Using method: ~w', [optimal]),
    useOptimal(Schedule, Delay, Reason).

useMethod(heuristic, Schedule, Delay, Reason):-    
    debug(schedule, 'Using method: ~w', [heuristic]),
    useHeuristic(Schedule, Delay, Reason).

useGenetic(Schedule, Delay, Reason):-
    genetic_settings(Args),
    allVessels(Vessels),
    docks_list(Docks),

    append(Args, [Schedule, Delay, Reason], ArgsWithOut1),
    append([Vessels, Docks], ArgsWithOut1, ArgsWithOut),

    % apply = call, mas usando lista como argumentos
    apply(geneticPort:genetic, ArgsWithOut).

useOptimal(Schedule, Delay, Reason):-
    docks_list(Docks),
    Docks = [Dock|_],
    
    allVessels(Vessels),
    
    Reason = "Reach optimal solution",
    linearPermutations:getSchedule(Vessels, Dock, Schedule, Delay).

useHeuristic(Schedule, Delay, Reason):-
    docks_list(Docks),
    Docks = [Dock|_],

    heuristicmethod(HeuristicMethod),
    allVesselsHeuristic(HeuristicMethod, Vessels),

    format(atom(Msg), "Reach optimal solution, using heuristic method: ~w", [HeuristicMethod]),
    
    debug(schedule, 'Using heuristic method: ~w', [HeuristicMethod]),
    linearPermutations:getSchedule(Vessels, Dock, Schedule, Delay).

    
readInputData(DataDict):-
    debug(schedule, 'Reading Input Data: ~w~n', [DataDict]),
    % Mandatory Settings 
    % Vessels
    readVessels(DataDict),
    % Docks
    readDocks(DataDict),

    % Optional Settings
    % Specified Method
    readSpecifiedMethod(DataDict),
    % Heuristic Method
    readHeuristicMethod(DataDict),
    % Genetic Settings
    readGeneticSettings(DataDict).

readVessels(DataDict):-
    retractall(vessel(_,_,_,_,_)),
    get_dict(vessels, DataDict, Vessels),   
    findall(VesselRef, (
        member(VesselDict, Vessels),
        VesselRef = VesselDict.ref,
        assertz(vessel(
            VesselDict.ref,
            VesselDict.arrivalTime,    
            VesselDict.departureTime,
            VesselDict.unloadingTime,
            VesselDict.loadingTime
        ))
    ), Refs),
    length(Refs, Len),
    debug(schedule, 'Asserted ~d vessels: ~w', [Len, Refs]).

readDocks(DataDict):-
    get_dict_or_var(docks, DataDict, DockList),
    docks_list(DockList),
    debug(schedule, 'Asserted docks: ~w', [DockList]).

readSpecifiedMethod(DataDict):-
    get_dict_or_var(method, DataDict, Method),
    method(Method),
    debug(schedule, 'Asserted method: ~w', [Method]).

readHeuristicMethod(DataDict):-
    heuristicmethod(eat), % Default
    get_dict_or_var(heuristicmethod, DataDict, HeuristicMethod),
    heuristicmethod(HeuristicMethod),
    debug(schedule, 'Asserted heuristic method: ~w', [HeuristicMethod]).

readGeneticSettings(DataDict):-

    get_dict_or_var(genetic, DataDict, GenDict),
    
    ((\+ var(GenDict))->
        get_dict_or_var(maxgenerations, GenDict, MaxGen),
        get_dict_or_var(populationsize, GenDict, PopSize),
        get_dict_or_var(crossoverprobability, GenDict, CrossProb),
        get_dict_or_var(mutationprobability, GenDict, MutProb),
        get_dict_or_var(stagnationminimum, GenDict, StagnMin),
        get_dict_or_var(stagnationanalysislength, GenDict, StagnLen),
        get_dict_or_var(maxtime, GenDict, MaxTime),!
    ;true),  
    % To avoid error if genetic key does not exist
     % To avoid error if genetic key does not exist
    
    Settings = [MaxGen, PopSize, CrossProb, MutProb, StagnMin, StagnLen, MaxTime],
    genetic_settings(Settings),
    debug(schedule, 'Asserted genetic settings: ~w', [Settings]).