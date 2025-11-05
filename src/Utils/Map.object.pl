
:- module(map, [
    mset/2,  
    mget/2,  
    mremove/1,  
    mclean/0,

    mapset/2,
    mapget/2,
    mapremove/1,
    mapclean/0
]).

:- dynamic keyvalue/2.

mset(Key, Value):- mapset(Key, Value).
mget(Key, Value):- mapget(Key, Value).
mremove(Key):- mapremove(Key).
mclean:- mapclean.

mapset(Key, Value):-
    retractall(keyvalue(Key,_)),
    asserta(keyvalue(Key,Value)).
mapget(Key, Value):-
    keyvalue(Key, Value).
mapremove(Key):- retractall(keyvalue(Key,_)).
mapclean:- retractall(keyvalue(_,_)).

