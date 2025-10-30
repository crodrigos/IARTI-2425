bw([Next]):-
    write(Next),!.
bw([Next|Body]):-
    write(Next),
    bw(Body).

bw(Message):- 
    write(Message),
    write('\n'), !.
bw(Prefix, Message):- 
    write(Prefix), 
    write(Message), 
    write('\n'),!.