:- module(craneScheduling, [
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


:-['Schedule.object.pl'].
:-['scheduling_vessels_1.pl']. % Given Code

vesselSequenceDelay(VL, NCranes, VSeq, Delay):-
    sequenceTemporization(VL,NCranes,VSeq),
    sumDelays(VSeq, Delay).

sumDelays([],0).
sumDelays([(Vessel,_,TEndLoading)|Rest],Sum):-
    vessel(Vessel,_,TExpectedDepart,_,_),
    TRealDeparture is TEndLoading,
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
    ModTemp is (DUnloading + DLoading) mod NCranes,
    (
        (ModTemp=0,!, OperationDuration is (DUnloading + DLoading)/NCranes);
        OperationDuration is (DUnloading + DLoading) // NCranes + 1 
    ),
    TEndLoading is TStartUnloading + OperationDuration,
    sequenceTemporization1(TEndLoading, NCranes, LV, Seq).

scheduleWorkTime([], _, 0).
scheduleWorkTime([V|R],NCranes,T):-
    scheduleWorkTime(R,NCranes,T1),
    vessel(V,_,_,U,L),
    OT is U+L,
    T is OT+T1.