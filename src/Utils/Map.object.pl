
:- dynamic keyvalue/2.
mapset(Key, Value):-
    retractall(keyvalue(Key,_)),
    asserta(keyvalue(Key,Value)).
mapget(Key, Value):-
    keyvalue(Key, Value).
mapremove(Key):- retractall(keyvalue(Key,_)).
mapclean:- retractall(keyvalue(_,_)).
