
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


createMatrixFromListNonEmpty1(Height, List, Height, [List]).
createMatrixFromListNonEmpty1(Height, List, CurrHeight, [Head|Matrix]):-
    CurrHeight1 is CurrHeight+1,!,
    append(Head, Tail, List),
    Head\=[],Tail\=[],
    createMatrixFromListNonEmpty1(Height, Tail, CurrHeight1,Matrix).

createMatrixFromListNonEmpty(List, Height, Matrix):-
    permutation(List,ListTemp),
    createMatrixFromListNonEmpty1(Height, ListTemp, 0, Matrix).

checkMatrixRepeats:-
    findall(M, createMatrixFromListNonEmpty([1,2,3,4], 3, M), LM).
    % FIXME: 



same_matrix(M1, M2) :-
    length(M1, L1),
    length(M2, L2),
    L1 =:= L2,               % must have same number of sublists
    permutation(M1, M2),!.   % check if one is a permutation of the other