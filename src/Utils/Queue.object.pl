
:- module(queue, [
    queue_push/3,
    queue_pop/3    
]).

queue_push(List, Value, NewList):-
    push(List, Value, NewList).

queue_pop(List, Value, NewList):-
    pop(List, Value, NewList).

push(L,P,[P|L]).
pop(L,Last,Rest):-
    length(L, Length),
    nth1(Length, L, Last, Rest).