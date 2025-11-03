:- initialization(main, main).

main :-
    use_module('Scheduling/MultipleCranes'),
    testForDifNumVessels,
    halt.
