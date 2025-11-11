
:- module(vars, [
    nPermutations/1,
    incPerms/0,
    shortest_delay/2,
    longest_delay/2,
    medium_delay/3,
    compareShortestDelay/2,
    compareLongestDelay/2,
    compareMediumDelay/1
]).

:- use_module([
    '../../Utils/Map.object.pl'
]).


nPermutations(N):-map:map("nPermutations",N).
incPerms:-nPermutations(N), N1 is N+1, nPermutations(N1).
:-nPermutations(0).

shortest_delay(Sequence, Delay):- mapgetset("craneShortest", (Sequence, Delay)).
:- shortest_delay([], 999999).

longest_delay(Sequence, Delay) :- mapgetset("craneLongest", [Sequence, Delay]).
:- longest_delay([], 0).

medium_delay(DelaySum, N, Medium):- mapgetset("craneMedium", [DelaySum, N, Medium]).
:- medium_delay(0,0,0).

compareShortestDelay(TripletSequence, Delay):-
    shortest_delay(_, ShortestDelay),
    ((
        Delay =< ShortestDelay, !, shortest_delay(TripletSequence, Delay)
    ); true).

compareLongestDelay(TripletSequence, Delay):-
    longest_delay(_, LongestDelay),
    ((
        Delay >= LongestDelay, !, longest_delay(TripletSequence, Delay)
    ); true).

compareMediumDelay(Delay):-
    medium_delay(Sum, N, Medium),
    N1 is N+1,
    Sum1 is Sum+Delay,
    Medium1 is Sum1/N1,
    medium_delay(Sum1, N1, Medium1).