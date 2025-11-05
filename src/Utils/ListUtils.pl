
range(N,Out):-
    range1(N,Out1),
    reverse(Out1, Out).

range1(0,[]):-!.
range1(N,[N|Out]):-
   N1 is N-1,
   range1(N1,Out). 

isSet(List) :-
    \+ ( select(Element, List, Rest),
        member(Element, Rest)
    ).

createMatrixFromListNonEmpty1([],[]).
createMatrixFromListNonEmpty1(L,[L2|M]):-
    append(L1,L2,L),
    createMatrixFromListNonEmpty1(L1,M).


createUniqueSublist(L,Height,R):-
    length(L, Length),
    N is Length-Height,
    concatElementsInList1(L,N,R).

concatElementsInList1(L,N,L1):-N=<0,!,allToList(L,L1).
concatElementsInList1(L,N,[V|Rest2]):-
    N1 is N - 1,
    concatElementsInList1(L, N1, Temp),
    select(V1, Temp, Rest1),
    select(V2, Rest1, Rest2),
    compareElem1(V1,V2),     % enforce canonical order so we never merge V2 before V1
    append(V1, V2, V).

compareElem1(V1,[FirstV2|_]):-
    last(V1, LastV1),
    LastV1 @< FirstV2.


normalize_to_list(V, V) :- is_list(V),!.
normalize_to_list(V, [V]).

allToList([],[]).
allToList([H|T],[H1|T1]):-
    normalize_to_list(H,H1),
    allToList(T, T1).