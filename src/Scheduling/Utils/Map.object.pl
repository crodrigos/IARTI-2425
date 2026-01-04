
:- module(map, [
    mset/2,  
    mget/2,  
    mremove/1,  
    mclean/0,
    mgetset/2,
    map/2,

    mapset/2,
    mapget/2,
    mapremove/1,
    mapclean/0,
    mapgetset/2
]).

:- dynamic keyvalue/2.


% predicados mais curtos porque tenho preguiça de escrever
mset(Key, Value):- mapset(Key, Value).
mget(Key, Value):- mapget(Key, Value).
mremove(Key):- mapremove(Key).
mclean:- mapclean.
mgetset(K,V):- mapgetset(K,V).
map(K,V):- mapgetset(K,V).

% mapgetset(Key,Value):-
%     (
%         ( var(Value),!, mapget(Key, Value) );
%         mapset(Key,Value), !
%     ).

mapgetset(Key,Value):-mapget(Key,ValueTemp), Value = ValueTemp, !.
mapgetset(Key,Value):-mapset(Key,Value).

mapset(Key, Value):-
    retractall(keyvalue(Key,_)),
    asserta(keyvalue(Key,Value)).
mapget(Key, Value):-
    keyvalue(Key, Value), !.
mapremove(Key):- retractall(keyvalue(Key,_)).
mapclean:- retractall(keyvalue(_,_)).

