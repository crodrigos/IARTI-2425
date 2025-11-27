:- use_module([
    '../Utils/Timer.object',
    '../Utils/Map.object',
    '../Utils/ListUtils.pl',
    '../Utils/BetterWrite.pl',
    '../DockScheduling/CraneScheduling.pl',
    '../DockScheduling/vessels.pl',
    '../DockScheduling/Heuristics/vesselAdj.pl'
]).


%! Predicado para transformar list de barcos numa possivel solução
% getPortSolution(-VesselList, -NDocks, +PortSchedule):-
splitVesselListInDocks(V, [ND], [(ND,V)]):-!.
splitVesselListInDocks(VesselList, [NDocks|NDT], [(NDocks,V1)|Rest]):-
    append(V1, V2, VesselList),
    splitVesselListInDocks(V2, NDT, Rest).

splitVesselListInDocksRand(V, [ND], [(ND,V)]):-!.
splitVesselListInDocksRand(VesselList, [NDocks|NDT], [(NDocks,V1)|Rest]):-
    random_subseq(VesselList,V1,V2),
    splitVesselListInDocksRand(V2, NDT, Rest).



% Make sequence for each dock
scheduleTemporization([], []):-!.
scheduleTemporization([(ND,VL)|T], [Seq|TSeq]):-
    sequenceTemporization(VL, ND, Seq),
    scheduleTemporization(T,TSeq).

% Calculate delay for each dock and choose the worst one
scheduleDelay(ScheduleSequence, Delays, WorstDelay):-
    scheduleDelay(ScheduleSequence, Delays),
    max_list(Delays, WorstDelay).
scheduleDelay([], []):-!.
scheduleDelay([Seq|T], [D| DT]):-
    sumDelays(Seq, D),
    scheduleDelay(T, DT).
    
scheduleTemporizationAndDelay(Sch, Seq, Delays, Worst):-
    scheduleTemporization(Sch, Seq),
    scheduleDelay(Seq, Delays, Worst).






neighourSchedule(Schedule, Distance, Neighbour):-
    findall(
        N,
        (
            neighourSchedule1(Schedule, Distance, [Schedule], N);
            moveVesselDockToDock(Schedule, N)
        ), 
        AllNeighbours
    ),
    member(Neighbour, AllNeighbours).

neighourSchedule1(_, 0,_, _):-!,fail.
neighourSchedule1(SCH, Depth, Visited, Neighbour):-
    Depth1 is Depth-1,
    (
        neighourScheduleDock(SCH, Neighbour);
        neighourSchedule1(Neighbour, Depth1, [SCH|Visited], Neighbour)
    ).
    
neighourScheduleDock([],[]):-!.
neighourScheduleDock([(NC,LV)|Rest], [(NC,LVPerm)|RestPerm]):-!,
    neighbourPermutation(LV,1,LVPerm),
    neighourScheduleDock(Rest,RestPerm).

moveVesselDockToDock(Schedule, Result) :-
    nth0(SrcIndex, Schedule, (ValS, ListS)),
    nth0(DstIndex, Schedule, (ValD, ListD)),
    SrcIndex \= DstIndex, ListS\=[],                   

    select(Elem, ListS, NewListS),
    NewListD = [Elem | ListD],

    replace0th(Schedule, (ValS,NewListS), SrcIndex, Temp),
    replace0th(Temp, (ValD, NewListD), DstIndex, Result).

replace0th(List, NewVal, Index, Result):-
    replace0th1(List, NewVal, Index, 0, Result).
replace0th1([L|T], NewVal, Target, Target, [NewVal|T]):-!.
replace0th1([L|T], NewVal, Target, Pos, [L|Result]):-
    Pos1 is Pos+1,
    replace0th1(T, NewVal, Target, Pos1, Result).


testNS:-
    situation(s1,_,LV),
    Docks=[1,2,3],
    splitVesselListInDocks(LV, Docks, Solution),
    findall(SL,(
        neighourSchedule(Solution,1, SL),
        writeln(SL)
    ), ALL),nl,
    writeln(Solution),
    length(ALL, L),
    bw("Neighbours: ", L).


test(Solution, Seq, Delays, Worst):-
    situation(s1,_,LV),
    Docks=[1,1,1],
    splitVesselListInDocks(LV, Docks, Solution),
    scheduleTemporizationAndDelay(Solution, Seq, Delays, Worst).