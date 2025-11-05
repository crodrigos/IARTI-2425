
% Module for Storing and managing input vessel info

:- module(vessels, [
    vessel/5,
    addVessel/5,
    allVessels/1,
    allVessels/2
]).

:- dynamic vessel/5.

vessel(zeus,        6,  63, 10, 16).
vessel(poseidon,   10,  55, 12, 10).
vessel(marenostrum, 8,  50,  8, 14).
vessel(nautilus,    9,  45,  6, 12).
vessel(floating,   12,  60,  9, 10).

vessel(odyssey,     7,  48, 11, 15).   
vessel(atlantis,   11,  58, 10, 13).  
vessel(triton,     14,  62, 12, 14).   
vessel(neptune,    16,  65,  8, 12).   
vessel(aurora,      5,  40,  7, 11).   


addVessel(VesselRef, ArrivingTime, DepartureTime, UnloadingDuration, LoadingDuration):-
    ((vessel(VesselRef,_,_,_,_));
        asserta(vessel(VesselRef, ArrivingTime, DepartureTime, UnloadingDuration, LoadingDuration))
    ).

allVessels(N, Vessels):-
    allVessels(VL),
    findall(E, (nth1(I,VL,E), I =< N), Vessels).

allVessels(Vessels):- 
	findall(X, vessel(X,_,_,_,_), Vessels).


% ADDING VESSELS MUST BE AFTTER DEFINING addVessel/5 PREDICATE!!!!!
:- addVessel(lol,0,0,0,0).