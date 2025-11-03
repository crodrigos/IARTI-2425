
divideListInXDif(L,R):-

genNonEmptyUniqueMatrix(L,H,M):-
    genNonEmptyUniqueMatrix1(L,N,0,M).

genNonEmptyUniqueMatrix1(_,H,H,[]).
genNonEmptyUniqueMatrix1(L,N,H,[L2|Rest]):-
    N1 is N+1,
    append(L1,L2,L),
    
    genNonEmptyUniqueMatrix1(L1,N1,H,Rest).