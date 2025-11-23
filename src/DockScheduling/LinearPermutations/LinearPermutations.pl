:- module(linearPermutations, [
    %obtainShortestSequence/2
]).

:- use_module([
    '../../Utils/Timer.object',
    '../../Utils/Map.object',
    '../../Utils/ListUtils.pl',
    '../../Utils/BetterWrite.pl',
    '../vessels.pl',
    '../CraneScheduling.pl',
    '../vars.pl'
]).

% Vars

max_permutations(MaxPerms) :- map:map("linearPermutations_MaxPermutations", MaxPerms).
:- max_permutations(10000).


obtainShortestSequence(ListOfVessels, NCranesAvailable, Sequence, Delay):-
    shortest_delay([], 9999999),
    ((obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranesAvailable)) -> ! ; !,true),
    shortest_delay(Sequence, Delay).

obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranes):-
    permutation(ListOfVessels, NewOrderedVesselList),
    sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence),
    sumDelays(TripletSequence, Delay),
    compareShortestDelay(TripletSequence, Delay),
    compareLongestDelay(TripletSequence, Delay),
    compareMediumDelay(Delay),
    fail.

obtainAllSequencesWithDelay(ListOfVessels, NCranes, RSorted):-
    findall((Delay,TripletSequence),(
        permutation(ListOfVessels, NewOrderedVesselList),
        sequenceTemporization(NewOrderedVesselList, NCranes, TripletSequence),
        sumDelays(TripletSequence, Delay)
    ),R),
    sort(R, RSorted).


%! obtainShortestSequence(+ListOfVessels, +NCranesAvailable) is nondet
% obtainShortestSequenceVerbose(ListOfVessels, NCranesAvailable):-
%     ((
%         shortest_delay([], 9999999),
%         longest_delay([], 0),
%         medium_delay(0,0,0),

%         range(NCranesAvailable, CL), !,

%         member(NCranes, CL),
%         bw(["\n\nTesting for ", NCranes, " crane(s)\n"]),

%         reset_timer, start_timer,

%         (obtainShortestSequence_LinearPermutation1(ListOfVessels, NCranes); true),

%         shortest_delay(SSeq, SDelay),
%         longest_delay(LSeq, LDelay),
%         medium_delay(_,N,MDelay),
%         get_elapsed_time(CalculationTime),

%         % bw("Shortest Sequence: ", SSeq),
%         % bw(" Longest Sequence: ", LSeq),

%         bw("   Shortest Delay: ", SDelay),
%         bw("    Longest Delay: ", LDelay),
%         bw("     Medium Delay: ", MDelay),

%         bw(" Permutation Made: ", N),

%         bw("Calculation Time (ms): ", CalculationTime),

%         SDelay =< 0, !
%     ); true).


getSchedule:-
    allVessels(5, VL),
    obtainShortestSequence(VL, 1, Seq, D),
    bw("Vessels: ", VL),
    bw("Delay: ", D),
    craneScheduling:writeDockDailySchedules(Seq).
