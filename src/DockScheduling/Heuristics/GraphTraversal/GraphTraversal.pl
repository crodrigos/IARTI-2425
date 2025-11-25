
:- use_module([
    'vesselAdj.pl',
    '../../CraneScheduling.pl' ,
    '../../../Utils/BetterWrite.pl'
]).




search(Start, End, NCranes, Path, Cost):-
    vesselSequenceDelay(End,NCranes, _, D),
    search1((D, End), NCranes, [(_,0, [Start])],(Cost, Path)).

search1((_,Dest), _, [(_,C,[Dest|T])|_], (C,RPath)):-
    reverse([Dest|T], RPath).    
search1((F, Dest), NCranes, [(_, Cost, Path)|PathRest], Return):-
    [First|_] = Path,
    findall((CE, C, [Perm|Path]), 
        (
            Dest\==First,
            adjacentWeighedPermutation(First, NCranes, 1, Perm, W),
            \+ member(Perm, Path),
            vesselSequenceDelay(First, NCranes, _, D),
            C is W + Cost,
            CE is C+D-F
        ), 
    NewPaths),
    append(PathRest, NewPaths, AllPaths),
    sort(AllPaths, OrderedPaths),
    OrderedPaths = [(_,C,_)|_], length(OrderedPaths, S),
    bw([C, " ", S, "\n"]),
    search1((F, Dest), NCranes, OrderedPaths, Return).
    
