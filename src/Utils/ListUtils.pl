
:- module(listutils, [
    range/2,
    createUniqueSublist/3,
    getFirstXOfList/3
]).

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

comb(0,_,[]).
comb(N,[X|T],[X|Comb]):-N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-N>0,comb(N,T,Comb).

split_k_parts(List, K, P) :-
    split_k_parts(List, K, [], P).

% Base case — all parts selected, no elements left
split_k_parts([], 0, Acc, Parts) :-
    reverse(Acc, Parts).
split_k_parts(List, K, Acc, Parts) :-
    K > 0,
    length(List, Len),
    MinSize is 1,
    MaxSize is Len - (K - 1),   % ensure room for remaining parts
    between(MinSize, MaxSize, Take),

    comb(Take, List, Part),     % your combination predicate (unordered)
    remove_all(Part, List, Rest),

    K1 is K-1,
    split_k_parts(Rest, K1, [Part|Acc], Parts).

remove_all([], L, L).
remove_all([H|T], L, R) :-
    select(H, L, L1),
    remove_all(T, L1, R).


createEmptyList(N,[]):- N=<0,!.
createEmptyList(N,[_|R]):-
    N1 is N-1,
    createEmptyList(N1, R).

subset_range(List, Start, End, Sub) :-
    Start =< End,
    Len is End - Start + 1,
    length(Prefix, Start),
    append(Prefix, Rest, List),
    length(Sub, Len),
    append(Sub, _, Rest).

getFirstXOfList(_, [], []) :- !.
getFirstXOfList(0, _, []) :- !.
getFirstXOfList(X, [H|T], [H|R]) :-
    X > 0,
    X1 is X - 1,
    getFirstXOfList(X1, T, R).

