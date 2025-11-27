
% Module for Storing and managing input vessel info

:- module(vessels, [
    vessel/5,
    addVessel/5,
    allVessels/1,
    allVessels/2,
    allVesselsRandom/2,
    situation/3
]).

:- use_module(["Heuristics/startingPoints.pl"]).

:- dynamic vessel/5.

addVessel(VesselRef, ArrivingTime, DepartureTime, UnloadingDuration, LoadingDuration):-
    ((vessel(VesselRef,_,_,_,_));
        asserta(vessel(VesselRef, ArrivingTime, DepartureTime, UnloadingDuration, LoadingDuration))
    ).

allVessels(N, Vessels):-
    allVessels(VL),
    findall(E, (nth1(I,VL,E), I =< N), Vessels).

allVessels(Vessels):- 
	findall(X, vessel(X,_,_,_,_), Vessels).

allVesselsRandom(N, Vessels):-
    allVessels(VL),
    random_permutation(VL, Perm),
    findall(E, (nth1(I,Perm,E), I =< N), Vessels).
    

situation(s1, 
    "Varied and real impossible solution", 
    ["s1_Zeus", "s1_Poseidon", "s1_Graca", "s1_Marques", "s1_Onda", "s1_Cartografo", "s1_Dona Maria", "s1_Caçador"]
). % Best Delay 0

situation(s1_EAT, "Varied and real impossible solution - EAT", L):-
    situation(s1, _, LV),
    earliestArrivalTime(LV, L).
situation(s1_EDT, "Varied and real impossible solution - EDT", L):-
    situation(s1, _, LV),
    earliestDepartureTime(LV, L).
situation(s1_SOT, "Varied and real impossible solution - SOT", L):-
    situation(s1, _, LV),
    shortestOperationTime(LV, L).


situation(
    s2, 
    "Single Perfect Solution",   ["s2_Dona Maria", "s2_Marques", "s2_Poseidon", "s2_Onda", "s2_Caçador", "s2_Zeus", "s2_Graca", "s2_Cartografo"]
). % BEST DELAY 183

situation(s2_EAT, "Single Perfect Solution - EAT", L):-
    situation(s2, _, LV),
    earliestArrivalTime(LV, L).
situation(s2_EDT, "Single Perfect Solution - EDT", L):-
    situation(s2, _, LV),
    earliestDepartureTime(LV, L).
situation(s2_SOT, "Single Perfect Solution - SOT", L):-
    situation(s2, _, LV),
    shortestOperationTime(LV, L).

situation(s3, "Big Direct", ["s3_Zeus", "s3_Poseidon", "s3_Graca", "s3_Marques", "s3_Onda", "s3_Cartografo", "s3_Dona Maria", "s3_Caçador", "s3_Monstro", "s3_V1", "s3_V2", "s3_V3", "s3_V4"]).


%! vessel (-Ref, -ArrivingTime, -DepartureTime, -UnloadingTime, -LoadingTime)
vessel("s1_Zeus",6,63,10,16).
vessel("s1_Poseidon",23,50,9,7).
vessel("s1_Graca",8,40,5,12).
vessel("s1_Marques",10,30,0,8).
vessel("s1_Onda",36,70,12,0).
vessel("s1_Cartografo",15,55,8,10).
vessel("s1_Dona Maria",28,65,7,9).
vessel("s1_Caçador",45,80,6,11).

% Situation 2
vessel("s2_Zeus",1,10,4,4).
vessel("s2_Poseidon",11,20,4,4).
vessel("s2_Graca",21,30,4,4).
vessel("s2_Marques",31,40,4,4).
vessel("s2_Onda",41,50,4,4).
vessel("s2_Cartografo",51,60,4,4).
vessel("s2_Dona Maria",61,70,4,4).
vessel("s2_Caçador",71,80,4,4).


% sit 3
vessel("s3_Zeus",1,10,4,4).
vessel("s3_Poseidon",11,20,4,4).
vessel("s3_Graca",21,30,4,4).
vessel("s3_Marques",31,40,4,4).
vessel("s3_Onda",41,50,4,4).
vessel("s3_Cartografo",51,60,4,4).
vessel("s3_Dona Maria",61,70,4,4).
vessel("s3_Caçador",71,80,4,4).
vessel("s3_Monstro",81,90,4,4).
vessel("s3_V1",91,100,4,4).
vessel("s3_V2",101,110,4,4).
vessel("s3_V3",111,120,4,4).
vessel("s3_V4",121,130,4,4).

