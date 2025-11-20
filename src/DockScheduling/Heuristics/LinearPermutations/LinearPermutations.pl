:- module(linearPermutations, [
    %obtainShortestSequence/2
]).

:- use_module([
    '../../../Utils/Timer.object',
    '../../../Utils/Map.object',
    '../../../Utils/ListUtils.pl',
    '../../../Utils/BetterWrite.pl',
    '../../vessels.pl',
    '../../CraneScheduling.pl',
    '../vars.pl'
]).
:- use_module('src/DockScheduling/CraneScheduling').
:- use_module('src/DockScheduling/algorithms/vars').
:- use_module('src/DockScheduling/vessels').
:- use_module('src/Utils/BetterWrite').
:- use_module('src/Utils/ListUtils').
:- use_module('src/Utils/Timer.object').
:- use_module(library(lists)).

% Vars

max_permutations(MaxPerms) :- map:map("linearPermutations_MaxPermutations", MaxPerms).
:- max_permutations(10000).

% Heuristicas
getAllHeuristicRefs(Refs):-Refs=[eat, edt, spt].

%! Early Time Arrival Hueristic
obtainShortestSequence(eat, ListOfVessels, NCranesAvailable, MaxPerms, Sequence, Delay):-
    findall(
        (Arrival, V),
        (
            member(V, ListOfVessels),
            vessel(V, Arrival, _, _, _)
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), VesselsSorted),
    obtainShortestSequence(VesselsSorted, NCranesAvailable, MaxPerms, Sequence, Delay).

obtainShortestSequence(edt, ListOfVessels, NCranesAvailable, MaxPerms, Sequence, Delay):-
    findall(
        (Departure,V),
        (
            member(V, ListOfVessels),
            vessel(V,_,Departure,_,_)
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), VesselsSorted),
    obtainShortestSequence(VesselsSorted, NCranesAvailable, MaxPerms, Sequence, Delay).

obtainShortestSequence(spt,ListOfVessels, NCranesAvailable, MaxPerms, Sequence, Delay):-
    findall(
        (PT, V),
        (
            member(V, ListOfVessels),
            vessel(V, _, _, UT, LT),
            PT is UT + LT
        ),
        VTemp
    ),
    sort(VTemp, VS),
    findall(V, member((_, V),VS), VesselsSorted),
    obtainShortestSequence(VesselsSorted, NCranesAvailable, Sequence, Delay).

obtainShortestSequence(ListOfVessels, NCranesAvailable, MaxPerms, Sequence, Delay):-
    shortest_delay([], 9999999),
    ((obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranesAvailable, MaxPerms)) -> ! ; !,true),
    shortest_delay(Sequence, Delay).


obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranes, MaxPerms):-
    CountMapRef = "linearPermutation_genN", map:map(CountMapRef, 0),

    permutation(ListOfVessels, NewOrderedVesselList),
    sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence),
    sumDelays(TripletSequence, Delay),
    compareShortestDelay(TripletSequence, Delay),
    compareLongestDelay(TripletSequence, Delay),
    compareMediumDelay(Delay),

    (
        map:map(CountMapRef, N),
        N1 is N+1,
        map:map(CountMapRef, N1),
        N1 >= MaxPerms, ! % If under MaxPerms, fail
    ).

obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranes):-
    permutation(ListOfVessels, NewOrderedVesselList),
    sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence),
    sumDelays(TripletSequence, Delay),
    compareShortestDelay(TripletSequence, Delay),
    compareLongestDelay(TripletSequence, Delay),
    compareMediumDelay(Delay),
    fail.


%! obtainShortestSequence(+ListOfVessels, +NCranesAvailable) is nondet
obtainShortestSequenceVerbose(ListOfVessels, NCranesAvailable):-
    ((
        shortest_delay([], 9999999),
        longest_delay([], 0),
        medium_delay(0,0,0),

        range(NCranesAvailable, CL), !,

        member(NCranes, CL),
        bw(["\n\nTesting for ", NCranes, " crane(s)\n"]),

        reset_timer, start_timer,

        (obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranes); true),

        shortest_delay(SSeq, SDelay),
        longest_delay(LSeq, LDelay),
        medium_delay(_,N,MDelay),
        get_elapsed_time(CalculationTime),

        % bw("Shortest Sequence: ", SSeq),
        % bw(" Longest Sequence: ", LSeq),

        bw("   Shortest Delay: ", SDelay),
        bw("    Longest Delay: ", LDelay),
        bw("     Medium Delay: ", MDelay),

        bw(" Permutation Made: ", N),

        bw("Calculation Time (ms): ", CalculationTime),

        SDelay =< 0, !
    ); true).


getSchedule:-
    allVessels(5, VL),
    obtainShortestSequence(VL, 1, Seq, D),
    bw("Vessels: ", VL),
    bw("Delay: ", D),
    craneScheduling:writeDockDailySchedules(Seq).
