
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