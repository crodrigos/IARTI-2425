:- module(writeToFile, [toFile/2]).


toFile(FileName,Goal):-
    writeln(FileName),
    tell(FileName),
    call(Goal),
    halt.

