:- module(aStarPort, [next/3, estimate/2]).

:- use_module([
    '../../../Heuristic/AStar/AStar.pl',
    '../../MultipleDocks.pl'
]).

next(Schedule, Next, W):-
    neighbourSchedule(Schedule,1,Next),
    scheduleTemporizçationAndDelay(Schedule, Seq1, Delay1, Worst1),
    scheduleTemporizationAndDelay(Next, Seq2, Delay2, Worst2),
    W is Worst2-Worst1.

% FIXME: AStar está a agir como Branch and bound pois h' é 0
estimate(Schedule, W):- 
    scheduleTemporizationAndDelay(Schedule,_,DockDelayL,_),
    sum_list(DockDelayL, W).

aStarImpl(VL, DockL, MaxGens, Best, DockDelayL, Delay):-
    splitVesselListInDocksRand(VL,DockL,Schedule),
    aStar(
        aStarPort:next, aStarPort:estimate,
        Schedule, MaxGens, Best
    ),
    scheduleTemporizationAndDelay(Best, _, DockDelayL, Delay).

aStarBest(VL, DockL, MaxGens, Best, Delay):-
    findall(D, increaseDocks(DockL,D),MultDockL),
    ((
        member(DL,MultDockL),
        write("DockList: "), write(DL),
        aStarImpl(VL, DL, MaxGens, Best, _, Delay),
        write(" | BestDelay: "), write(Delay), nl,
        Delay=<0,!
    );true).    


increaseDocks(DockL, Result):-
    length(DockL, Len),

    length(IncDocks, Len),
    maplist(=(1), IncDocks),
    ((Result=IncDocks);increaseDocks1(IncDocks, DockL, Result)).

increaseDocks1(Base, Ref, Result):-
    findall(Res,increaseDocks2(Base,Ref,Res),RTemp),
    sort(RTemp, Results),
    (
        member(Result, Results)
    ; (
        last(Results, LastRes),
        increaseDocks1(LastRes, Ref, Result)
    )).
    
increaseDocks2(Base, Base, []):-!,fail.
increaseDocks2([],[],[]).
increaseDocks2([Base|TBase],[Ref|TRef],[Result|TResult]):-
    Ref>Base,!,
    Result is Base+1,
    ((TResult=TBase);increaseDocks2(TBase, TRef, TResult)).
increaseDocks2([Base|TBase],[Ref|TRef],[Base|TResult]):-
    increaseDocks2(TBase, TRef, TResult).

subtract([],[],[]).
subtract([A|T1], [B|T2], [R|T3]):-
    R is A-B,
    subtract(T1, T2, T3).