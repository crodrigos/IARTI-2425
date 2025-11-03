bw([]) :- !, true.

bw([Next|Body]) :- 
    !,
    write(Next),
    bw(Body),
    !, true.

bw(Message) :- 
    !,
    write(Message),
    nl,
    true.

bw(Prefix, Message) :- 
    !,
    write(Prefix),
    writeln(Message),
    true.
