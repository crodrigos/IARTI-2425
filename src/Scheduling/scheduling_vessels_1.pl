

% First support for IARTI project 2025/2026
% Scheduling Vessels Unload/Load

:-dynamic shortest_delay/2.

allVessels(N, Vessels):-
    allVessels(VL),
    findall(E, (nth1(I,VL,E), I =< N), Vessels).

allVessels(Vessels):- 
	findall(X, vessel(X,_,_,_,_), Vessels).

sequence_temporization(LV,SeqTriplets):-
    sequence_temporization1(0,LV,SeqTriplets).

sequence_temporization1(_,[],[]).
sequence_temporization1(EndPrevSeq,[V|LV],[(V,TInUnload,TEndLoad)|SeqTriplets]):-
    vessel(V,TIn,_,TUnload,TLoad),
    ((TIn> EndPrevSeq,!, TInUnload is TIn); TInUnload is EndPrevSeq+1),
    TEndLoad is TInUnload + TUnload+TLoad -1,
    sequence_temporization1(TEndLoad,LV,SeqTriplets).



sum_delays([],0).
sum_delays([(V,_,TEndLoad)|LV],S):-
	vessel(V,_,TDep,_,_),TPossibleDep is TEndLoad+1,
	((TPossibleDep>TDep,!,SV is TPossibleDep-TDep);SV is 0),
	sum_delays(LV,SLV),
	S is SV+SLV.


obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay):-
    (obtain_seq_shortest_delay1;true),
    retract(shortest_delay(SeqBetterTriplets, SShortestDelay)),!
    .

obtain_seq_shortest_delay1:-
    asserta(shortest_delay(_,100000)),
    allVessels(ListOfVessels),
    permutation(ListOfVessels,SeqV),
    sequence_temporization(SeqV,SeqTriplets),
    sum_delays(SeqTriplets,S),
    compare_shortest_delay(SeqTriplets,S),
    fail.

compare_shortest_delay(SeqTriplets,S):-
    shortest_delay(_,SLower),
    ((S<SLower,!,retract(shortest_delay(_,_)),asserta(shortest_delay(SeqTriplets,S)));true).


