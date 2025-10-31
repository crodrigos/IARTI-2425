% ----------------------------
% Dynamic facts for tracking
% ----------------------------
:- dynamic calculation_time/1.
:- dynamic calculation_count/1.

calculation_time(0).
calculation_count(0).

% ----------------------------
% TIME TRACKING UTILITIES
% ----------------------------
start_timer :-
    statistics(runtime, [Start|_]),
    retractall(calculation_time(_)),
    asserta(calculation_time(Start)).

get_elapsed_time(TimeTaken) :-
    calculation_time(Start),
    statistics(runtime, [End|_]),
    TimeTaken is End - Start.

reset_timer :-
    retractall(calculation_time(_)),
    asserta(calculation_time(0)).

% ----------------------------
% COUNTER UTILITIES
% ----------------------------
start_count :-
    retractall(calculation_count(_)),
    asserta(calculation_count(0)).

increment_count :-
    calculation_count(C0),
    C1 is C0 + 1,
    retractall(calculation_count(_)),
    asserta(calculation_count(C1)).

get_count(C) :-
    calculation_count(C).

reset_count :-
    retractall(calculation_count(_)),
    asserta(calculation_count(0)).
