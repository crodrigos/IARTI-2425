
eval(Pop, X):-
    random(0, 1000, X).
    

b(Population,EvalGoal,Result):-
    findall(
        (V,El), 
        (
            member(El, Population),
            call(EvalGoal,El, V)
        ), 
        R1
    ),
    sort(R1, R1S),
    findall(
        El,
        (
            member((V,El), R1S)
        ),
        Result
    ).
    
