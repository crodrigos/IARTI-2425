
% Module for Storing and managing input vessel info

:- module(vessels, [
    vessel/5,
    addVessel/5,
    allVessels/1,
    allVessels/2
]).

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



%! vessel (ref, ArrivingTime, DepartureTime, UnloadingTime, LoadingTime)
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



vessel(hamilton,     13,  68, 11, 15).
vessel(columbus,      7,  52,  9, 14).
vessel(vanguard,     10,  57, 10, 12).
vessel(titan,        15,  72, 12, 13).
vessel(leviathan,    17,  78, 13, 16).

vessel(elysium,       6,  46,  8, 10).
vessel(horizon,       9,  54,  9, 13).
vessel(phoenix,      12,  61, 10, 14).
vessel(atlas,        11,  59,  7, 12).
vessel(pegasus,       8,  49,  8, 11).

vessel(sirius,       14,  70, 11, 13).
vessel(centurion,    16,  75, 12, 15).
vessel(meridian,      5,  42,  6, 10).
vessel(calypso,      10,  56,  9, 12).
vessel(endeavour,     7,  50,  8, 11).

vessel(odysseus,     13,  67, 11, 14).
vessel(chronos,       9,  53,  7, 13).
vessel(aquila,       15,  73, 10, 14).
vessel(everest,      11,  60,  9, 12).
vessel(aurum,         6,  47,  7, 10).

vessel(v001, 10, 40, 5, 8).
vessel(v002, 12, 48, 7, 10).
vessel(v003, 14, 55, 9, 12).
vessel(v004, 18, 62, 11, 15).
vessel(v005, 20, 70, 13, 17).


vessel(v006, 11, 38, 6, 9).
vessel(v007, 13, 46, 8, 11).
vessel(v008, 16, 52, 10, 13).
vessel(v009, 19, 60, 12, 14).
vessel(v010, 22, 75, 14, 18).


vessel(v011, 15, 41, 7, 9).
vessel(v012, 17, 49, 9, 12).
vessel(v013, 21, 57, 11, 14).
vessel(v014, 23, 66, 12, 16).
vessel(v015, 25, 78, 15, 19).


vessel(v016, 12, 43, 6, 8).
vessel(v017, 14, 51, 7, 10).
vessel(v018, 18, 58, 10, 13).
vessel(v019, 20, 64, 12, 15).
vessel(v020, 24, 80, 16, 20).


vessel(v021, 11, 42, 5, 7).
vessel(v022, 15, 50, 8, 11).
vessel(v023, 17, 55, 9, 13).
vessel(v024, 19, 63, 11, 14).
vessel(v025, 22, 72, 13, 17).


vessel(v026, 13, 39, 6, 8).
vessel(v027, 16, 47, 7, 10).
vessel(v028, 18, 53, 8, 12).
vessel(v029, 21, 61, 11, 13).
vessel(v030, 26, 82, 17, 21).


vessel(v031, 14, 45, 6, 9).
vessel(v032, 18, 52, 8, 11).
vessel(v033, 20, 60, 10, 14).
vessel(v034, 22, 68, 12, 16).
vessel(v035, 28, 85, 18, 22).


vessel(v036, 12, 44, 5, 8).
vessel(v037, 16, 51, 8, 10).
vessel(v038, 19, 59, 10, 12).
vessel(v039, 23, 71, 14, 17).
vessel(v040, 27, 88, 19, 23).


vessel(v041, 13, 43, 6, 9).
vessel(v042, 17, 50, 9, 12).
vessel(v043, 21, 58, 11, 14).
vessel(v044, 24, 69, 14, 18).
vessel(v045, 29, 90, 20, 24).


vessel(v046, 11, 41, 5, 7).
vessel(v047, 15, 48, 7, 10).
vessel(v048, 18, 56, 9, 12).
vessel(v049, 22, 65, 12, 15).
vessel(v050, 30, 95, 21, 25).


vessel(v051, 12, 42, 6, 8).
vessel(v052, 16, 49, 7, 11).
vessel(v053, 19, 57, 9, 13).
vessel(v054, 23, 67, 13, 16).
vessel(v055, 31, 100, 22, 26).


vessel(v056, 13, 46, 6, 9).
vessel(v057, 17, 53, 8, 12).
vessel(v058, 20, 62, 10, 14).
vessel(v059, 25, 73, 15, 18).
vessel(v060, 33, 105, 24, 27).


vessel(v061, 14, 47, 7, 9).
vessel(v062, 18, 55, 9, 12).
vessel(v100, 48, 145, 35, 40).