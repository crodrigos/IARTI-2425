

% First support for IARTI project 2025/2026
% Scheduling Vessels Unload/Load

:-use_module('vessels').

:-dynamic shortest_delay/2.

sequence_temporization(OrderedVesselList, TripletSequence) :-
    sequence_temporization1(0, OrderedVesselList, TripletSequence).

sequence_temporization1(_, [], []) :- !.
sequence_temporization1(
    EndPrevSeq,
    [Vessel | LV],
    [(Vessel, TStartUnloading, TEndLoading) | Seq]
) :-
    vessel(Vessel, TExpectedArrival, _, DUnloading, DLoading),
    ((TExpectedArrival > EndPrevSeq, !, TStartUnloading is TExpectedArrival);
     TStartUnloading is EndPrevSeq + 1),
    OperationDuration is DUnloading + DLoading,
    TEndLoading is TStartUnloading + OperationDuration - 1,
    sequence_temporization1(TEndLoading, LV, Seq).




sum_delays([],0).
sum_delays([(Vessel,_,TEndLoading)|Rest],Sum):-
    vessel(Vessel,_,TExpectedDepart,_,_),
    TRealDeparture is TEndLoading+1,
    (
        (TRealDeparture>TExpectedDepart,!,Delay is TRealDeparture-TExpectedDepart);
        Delay is 0
    ),
    sum_delays(Rest,SumRest),
    Sum is Delay + SumRest.



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


