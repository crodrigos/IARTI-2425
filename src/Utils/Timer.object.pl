% ----------------------------
% Timer Utils
% ----------------------------

:- module(timer, [
    start_timer/2,
    start_timer/1,
    start_timer/0,
    get_elapsed_time/2,
    get_elapsed_time/1,
    reset_timer/1,
    reset_timer/0
]).

:- ['Map.object.pl'].

% Optional; SWI already has statistics/2 built-in
% but it doesn’t hurt
:- use_module(library(statistics)).

start_timer(ID):-start_timer(ID,_).
start_timer(ID, Start):-
    statistics(runtime, [Start|_]),
    mapset(ID, Start).

get_elapsed_time(ID, TimeTaken):-
    mapget(ID, Start),
    statistics(runtime, [End|_]),
    TimeTaken is End - Start.

reset_timer(ID):- 
    mapset(ID, 0).

start_timer:- 
    start_timer("DEFAULT_TIMER", _).

get_elapsed_time(TimeTaken):- 
    get_elapsed_time("DEFAULT_TIMER", TimeTaken).

reset_timer:- 
    reset_timer("DEFAULT_TIMER").
