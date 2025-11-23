:- use_module([
    '../vessels.pl'
]).

all(LV,EAT,EDT,SOT,SST):-
    earliestArrivalTime(LV,EAT),
    earliestDepartureTime(LV,EDT),
    shortestOperationTime(LV,SOT),
    shortestSlackTime(LV,SST).

earliestArrivalTime(LV,R):-
    findall(
        (Arrival, V),
        (
            member(V, LV),
            vessel(V, Arrival, _, _, _)
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), R).

earliestDepartureTime(LV,R):-
    findall(
        (Departure,V),
        (
            member(V, LV),
            vessel(V,_,Departure,_,_)
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), R).

shortestOperationTime(LV, R):-
    findall(
        (PT, V),
        (
            member(V, LV),
            vessel(V, _, _, UT, LT),
            PT is UT + LT
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), R).

shortestSlackTime(LV,R):-
    findall(
        (K, V),
        (
            member(V, LV),
            vessel(V, A, D, UT, LT),
            PT is UT + LT,
            ST is A+D,
            K is ST-PT
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), R).