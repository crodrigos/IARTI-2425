
:- module(vesselGraph, [
    visionRange/1,
    adjacentWeighedPermutation/4,
    adjacentWeighedPermutation/5,
    adjacentPermutation/2,
    neighbourPermutation/3
]).

:- use_module([
    '../CraneScheduling.pl',
    'vars.pl'
]).

visionRange(N):-map:map("neighbor_range", N).
:-visionRange(2).

adjacentWeighedPermutation(Vessels,NCranes,Perm,W):-
    visionRange(AdjDist),
    adjacentWeighedPermutation(Vessels,NCranes,AdjDist,Perm,W).

adjacentWeighedPermutation(Vessels,NCranes,AdjDist,Perm,W):-
    sequenceTemporization(Vessels,NCranes,VesselsSeq),
    
    neighbourPermutation(Vessels,Range,Perm), %adjacentPermutationDeg
    sequenceTemporization(Perm,NCranes, PermutationSeq),

    sumDelays(VesselsSeq,D1),
    sumDelays(PermutationSeq,D2),
    W is D2-D1.

neighbourPermutation(List,MaxDistance,Permutation):-
    neighbourPermutation1(MaxDistance,0,[List],PermutationList),
    delete(PermutationList, List, PermList),!,
    member(Permutation, PermList).
    

neighbourPermutation1(D,D,Visited,Permutation):-!,Visited=Permutation.
neighbourPermutation1(MaxDepth, Depth, Visited, Permutation):-
    Depth1 is Depth+1, 
    allNextNeighbors(Visited, NextNeighbors),
    neighbourPermutation1(MaxDepth, Depth1, NextNeighbors, Permutation).

allNextNeighbors(Visited, NextNeighbors):-
    findall(
        P,
        (
            member(Node, Visited),
            adjacentPermutation(Node, P),
            incPerms
        ),
        Next_temp 
    ),
    sort(Next_temp, NextNeighbors).
    
    

adjacentPermutation([X,Y|T], [Y,X|T]).
adjacentPermutation([H|T], [H|R]):-
    adjacentPermutation(T, R).
adjacentPermutation([V], [V]).


    
    