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

:- use_module('Map.object').
:- use_module(library(statistics)).

timermapid("TIMER_MODULE_DEFAULT_ID").

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

% Non ID associated predicates

start_timer:- 
    timermapid(TimerID),
    start_timer(TimerID, _).

get_elapsed_time(TimeTaken):- 
    timermapid(TimerID),
    get_elapsed_time(TimerID, TimeTaken).

reset_timer:- 
    timermapid(TimerID),
    reset_timer(TimerID).
