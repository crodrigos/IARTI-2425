
:- module(createScheduleService, [getSchedule/4]).

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
    "../Scheduling/Utils/DictPlus.pl"
]).

docks_list(DockList):-map:map("schedule_service_dock_list", DockList).
:-docks_list([1]).

heuristicmethod(HeuristicMethod):-map:map("schedule_service_heuristic_method", HeuristicMethod).

method(Method):-map:map("schedule_service_method", Method).

genetic(GeneticSettings):-map:map("schedule_service_genetic", GeneticSettings).


getSchedule(DataDict, Schedule, Delay, TimeTaken):-
    debug(schedule, '~n Schedule Service ~n', []),
    readInputData(DataDict),

    reset_timer,start_timer,
    useGenetic(Schedule, Delay),
    get_elapsed_time(TimeTaken).

    
useGenetic(Schedule, Delay):-
    genetic(Args),
    allVessels(Vessels),
    docks_list(Docks),

    append(Args, [Schedule, Delay], ArgsWithOut1),
    append([Vessels, Docks], ArgsWithOut1, ArgsWithOut),

    % apply = call, mas usando lista como argumentos
    apply(geneticPort:genetic, ArgsWithOut),
    debug(schedule, 'Delay: ~d', [Delay]).


    
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
    heuristicmethod(_),
    get_dict_or_var(heuristicmethod, DataDict, HeuristicMethod),
    heuristicmethod(HeuristicMethod),
    debug(schedule, 'Asserted heuristic method: ~w', [HeuristicMethod]).

readGeneticSettings(DataDict):-
    GenDict = DataDict.genetic,
    get_dict_or_var(maxgenerations, GenDict, MaxGen),
    get_dict_or_var(populationsize, GenDict, PopSize),
    get_dict_or_var(crossoverprobability, GenDict, CrossProb),
    get_dict_or_var(mutationprobability, GenDict, MutProb),
    get_dict_or_var(stagnationminimum, GenDict, StagnMin),
    get_dict_or_var(stagnationanalysislength, GenDict, StagnLen),
    Settings = [MaxGen, PopSize, CrossProb, MutProb, StagnMin, StagnLen],
    genetic(Settings),
    debug(schedule, 'Asserted genetic settings: ~w', [Settings]).