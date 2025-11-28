
:- module(astar, [aStar/5]).


aStar(
    NextPredicate, EstimationPredicate,
    Start, MaxGenerations, Best
) :-
    aStar1(
        NextPredicate,EstimationPredicate,
        [(0,0,[Start])], MaxGenerations, 0,
        [Return|_]
    ),
    Return = (_, Cost, [Best|_]).

aStar1(_,_,Paths,MaxGenerations,MaxGenerations,Paths):-!.
aStar1(
    NextPredicate, EstimationPredicate,
    [(_,BestCost,BestPath)|Rest], 
    MaxGenerations, Generations, Return
):-
    [First|_] = BestPath,
    findall(
        (EstCost, Cost, [Next|BestPath]),
        (
            call(NextPredicate, First, Next, Cost),
            \+ member(Next,BestPath),
            call(EstimationPredicate, First, EstimationCost),
            EstCost is EstimationCost + Cost + BestCost
        ),
        New  
    ),
    
    append(Rest, New, All),
    sort(All, AllSorted),

    % AllSorted = [(E,C,P)|_],
    % write("Cost: "), writeln(C),

    D1 is Generations+1,
    aStar1(NextPredicate, EstimationPredicate, AllSorted, MaxGenerations, D1, Return).