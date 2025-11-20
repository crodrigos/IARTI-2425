:- module(craneScheduling, [
    testForDifNumVessels/0,
    testForDifNumVessels/1,
    sequenceTemporization/3,
    sumDelays/2,
    vesselSequenceDelay/4
]).

:- use_module([
    '../Utils/Timer.object',
    '../Utils/Map.object',
    '../Utils/ListUtils.pl',
    '../Utils/BetterWrite.pl',
    'vessels.pl'
]).

% Funçoes heuristicas
:- use_module([
    'algorithms/LinearPermutations/LinearPermutations.pl'
]).



:-['Schedule.object.pl'].
:-['scheduling_vessels_1.pl']. % Given Code







%:- module(cranes, [testForDifNumVessels/0]).

% ---- PREDICADOS DINAMICOS ----

% Current Shortest Schedule and Interval
% shortest_delay(Sequence, Delay)
% WARNING: DO NOT DO SOMETHING LIKE shortest_delay(S, 9999999) 

max_cranes(N) :-
    mapgetset("maxCranes", N).
:- max_cranes(7).

sumDelays([],0).
sumDelays([(Vessel,_,TEndLoading)|Rest],Sum):-
    vessel(Vessel,_,TExpectedDepart,_,_),
    TRealDeparture is TEndLoading+1,
    (
        (TRealDeparture>TExpectedDepart,!,Delay is TRealDeparture-TExpectedDepart);
        Delay is 0
    ),
    sumDelays(Rest,SumRest),
    Sum is Delay + SumRest.

sequenceTemporization(OrderedVesselList, NCranes, TripletSequence):-
    sequenceTemporization1(0, NCranes, OrderedVesselList, TripletSequence).

sequenceTemporization1(_, _, [], []):- !.
sequenceTemporization1(
    EndPrevSeq, 
    NCranes,
    [Vessel|LV], 
    [(Vessel, TStartUnloading, TEndLoading)|Seq]
):-
    vessel(Vessel, TArrival, _, DUnloading, DLoading),
    ((TArrival > EndPrevSeq, !, TStartUnloading is TArrival); TStartUnloading is EndPrevSeq + 1),
    OperationDuration is (DUnloading + DLoading)//NCranes + 1,
    TEndLoading is TStartUnloading + OperationDuration,
    sequenceTemporization1(TEndLoading, NCranes, LV, Seq).

vesselSequenceDelay(VL, NCranes, VSeq, Delay):-
    sequenceTemporization(VL,NCranes,VSeq),
    sumDelays(VSeq, Delay).
