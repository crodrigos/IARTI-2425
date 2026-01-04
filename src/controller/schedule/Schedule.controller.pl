
:- module(scheduleController, []).


:- use_module([
    library(http/http_server),
    library(http/http_header),
    library(http/thread_httpd),
    library(http/http_dispatch),
    library(http/http_client),
    library(http/http_json),
    library(http/json)
]).

:- use_module([
    "../../service/Schedule.service.pl" ,
    "../../Scheduling/Utils/DictPlus.pl" 
]).

:- http_handler(
    root(schedule), 
    schedule_route(post),
    [method(post)]  
).

schedule_route(post, Request):-
    http_read_json_dict(Request, DataRaw, [to(atom)]),

    atom_json_dict(DataRaw, Data, [value_string_as(string)]),
    debug(schedule, 'Recieved Data: ~w', [Data]),

    parseResponseData(Data, DataForService),

    with_output_to(string(_), 
        createScheduleService:getSchedule(DataForService, Method, Reason, Schedule, Delay, TimeTaken)
    ),  
    debug(schedule, 'Returned Schedule: ~w', [Schedule]),
    
    scheduleToJson(Method, Reason, Schedule, Delay, TimeTaken, ScheduleJsonDict),
    buildResponseMessage(status(success), ScheduleJsonDict, StandardResponse),
    reply_json_dict(StandardResponse).


schedule_route(post,_Request):-
    debug(schedule, 'error ocurred', []),
    buildResponseMessage(status(error), "500 Internal Server Error", ErrorResponse),
    http_status_reply(
        server_error("Parsing Error"), ErrorResponse, _, _
    ).


scheduleToJson(
    Method, 
    Reason,
    TimedSchedule, 
    Delay, 
    TimeTaken, 
    Dict
):-
    findall(
        DockJson, 
        (
            member(Dock, TimedSchedule),
            findall(
                json{ref: Ref, arrival: Arr, departure: Dep},
                member((Ref, Arr, Dep), Dock),
                DockJson
            )
        ),
        ScheduleJson
    ),

    Dict = dict{
        delay: Delay, 
        timetaken: TimeTaken,
        method: Method,
        reason: Reason,
        schedule: ScheduleJson
    }.

%! buildResponseMessage(+Status, +Data, -ResponseMessage) is det
buildResponseMessage(status(success), Data, json{status: "success", data: Data}).
buildResponseMessage(status(error), ErrorMessage, json{status: "error", message: ErrorMessage}).

% prolog_to_json(+Term, -JSONString)
% Converts a Prolog Dict or json([Name=Value]) list into a JSON string.
prolog_to_json(Term, JSONString) :-
    % 1. Use with_output_to to capture stream output into a string variable
    with_output_to(string(JSONString), (
        % 2. Check if the Term is a native Dict
        ( is_dict(Term) ->
            json_write_dict(current_output, Term, [width(0)])
        ; 
            % 3. Fallback for older json([pair]) format
            json_write(current_output, Term, [width(0)])
        )
    )).


parseResponseData(Data, Data):-
    parseVessels(Data, _),
    parseDocks(Data, _),
    parseSpecifiedMethod(Data, _),
    parseHeuristicMethod(Data, _),
    parseGeneticSettings(Data, _).

parseVessels(Data, VesselDataList):-
    findall(
        [Ref, ArrivalTime, DepartureTime, UnloadingTime, LoadingTime],
        (
            member(VesselDict, Data.vessels),
            Ref = VesselDict.ref,
            ArrivalTime = VesselDict.arrivalTime,
            DepartureTime = VesselDict.departureTime,
            UnloadingTime = VesselDict.unloadingTime,
            LoadingTime = VesselDict.loadingTime
        ),
        VesselDataList
    ),
    length(VesselDataList, Len),
    debug(schedule, 'Parsed ~d Vessels: ~w', [Len, VesselDataList]).

parseDocks(Data, Docks):-
    get_dict_or_var(docks, Data, Docks),
    debug(schedule, 'Parsed Docks: ~w', [Docks]).

parseSpecifiedMethod(Data, Method):-
    get_dict_or_var(method, Data, Method),
    debug(schedule, 'Parsed Method: ~w', [Method]).

parseHeuristicMethod(Data, HeuristicMethod):-
    get_dict_or_var(heuristicmethod, Data, HeuristicMethod),
    debug(schedule, 'Parsed Heuristic Method: ~w', [HeuristicMethod]).

parseGeneticSettings(Data, GeneticSettings):-
    get_dict_or_var(genetic, Data, GeneticSettings),
    debug(schedule, 'Parsed Genetic settings: ~w', [GeneticSettings]).