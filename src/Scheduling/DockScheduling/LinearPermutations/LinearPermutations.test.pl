:- use_module([
   "LinearPermutations.pl",
   "../../Utils/WriteToFile.pl",
   "../../Utils/BetterWrite.pl",
   "../vessels.pl"
]).



obtainAllSequencesWithDelayBW(ListOfVessels,NCranes):-
    linearPermutations:obtainAllSequencesWithDelay(ListOfVessels,NCranes,R),
    bw("Delay"),
    findall(_,(member((D,S), R), bw([D]),nl),_).

run(LV,NCranes,FN):-
    get_time(T),format_time(string(Text),"%d-%e-%Y %H-%M",T),
    format(atom(FileName), "reports/LinearPermutations/~w_~w.csv", [FN,Text]),
    writeToFile:toFile(
        FileName, obtainAllSequencesWithDelayBW(LV, NCranes)
    ).