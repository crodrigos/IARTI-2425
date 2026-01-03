
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

% /schedule route

% :- http_handler(
%     root(schedule), 
%     schedule_route(get),
%     [method(get)]
% ).

% schedule_route(get, _Request):-
%     reply_html_page(
%         title('Scheduler'),
%         [ 
%             h1('Hello world!'),
%             p("Shcedule")
%         ]
%     ).

:- http_handler(
    root(schedule), 
    schedule_route(post),
    [method(post)]  
).
    
schedule_route(post, Request):-

    http_read_data(Request, DataRaw, [to(atom)]),
    debug(schedule, 'Request: ~w', [DataRaw]),

    atom_json_dict(DataRaw, Data, [value_string_as(string)]),
    debug(schedule, 'Recieved Data: ~w', [Data]),

    parseResponseData(Data, DataForService),

    with_output_to(string(_), 
        createScheduleService:getSchedule(DataForService, Schedule, Delay, TimeTaken)
    ),  
    debug(schedule, 'Returned Schedule: ~w', [Schedule]),
    
    scheduleToJson(Schedule, Delay, TimeTaken, ScheduleJsonDict),
    debug(schedule, 'Schedule JSON DICT: ~w', [ScheduleJsonDict]),
    
    prolog_to_json(ScheduleJsonDict, ReplyData),
    debug(schedule, 'Schedule JSON RAW: ~w', [ReplyData]),
    
    format('Content-type: text/json~n~n', []),
    format('~w', [ReplyData]).



schedule_route(post,_Request):-
    debug(schedule, 'error ocurred', []),
    reply_json_dict(
        _{
            error: "An error has ocurred"  
        }
        ,[json_prolog_object(true)]
    ).


scheduleToJson(
    Schedule, 
    Delay, 
    TimeTaken, 
    dict{delay: Delay, schedule: ScheduleJson, timetaken: TimeTaken}
):-
    findall(
        dict{cranes: Cranes, vessels: VesselsInDock}, 
        (
            member((Cranes, VesselsInDock), Schedule)
        ),
        ScheduleJson
    ).

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