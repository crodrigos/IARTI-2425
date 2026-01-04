
:- use_module([
    "BranchAndBound.pl", 
    "../../vessels.pl",
    "../../../Utils/Timer.object.pl",
    "../../../Utils/ListUtils.pl"
]).

cranesX([
    1
]).

visionRanges([1,2,3,4,5]).

situations([s3]):- findall(V, vessels:situation(V,_,_),L).

cranesX([1]).

generations(L):- L = [10,20,30,40,50].

test:-
    situations(SituationsL),
    cranesX(CraneL),
    visionRanges(VisionL),
    range(3,RepeatL),
    generations(GenL),

    format("Title,Vessels,Cranes,Generations,VisionRange,BestDelay,TimeMS~n"),!,

    findall(_,(
            member(SitRef, SituationsL),
            member(Gens, GenL),
            member(Vision, VisionL),
            member(NCranes,CraneL),
            member(_M, RepeatL),

            situation(SitRef, Title, Vessels),

            testsingle(Title, NCranes, Vision, Vessels, Gens, SSeq, LSeq)
        ),_).

testsingle(Title, NCranes, VisionRange, Vessels, Gens, S, D):-
    
    reset_timer, start_timer,
    branchAndBound(NCranes, VisionRange, Vessels, Gens, S, D),
    get_elapsed_time(Time),

    length(Vessels, NVessels),

    format("\"~w\",~w,~w,~w,~w,~w,~w~n", [Title,NVessels,NCranes,Gens, VisionRange, D, Time]).

    