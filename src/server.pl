
:-use_module([
    library(http/http_server),
    library(http/http_header),
    library(http/http_dispatch),
    library(http/http_client),
    library(http/http_json)
]).

:-use_module([
    './controller/root/root.controller.pl',
    './controller/schedule/Schedule.controller.pl'    
]).



devmode:-
    format("Dev mode~n",[]),
    debug(_).

list_all_routes :-
    format("Registred Routes",[]),
    forall(http_current_handler(Path, Closure),
        format('~w handles ~w', [Path, Closure])
    ).

:- list_all_routes, initialization(http_server([port(8080)])).

