% Base case for list: do nothing, succeed once.
bw([]) :- !, true.

% Recursive case for list: write each element.
bw([Next|Body]) :- 
    !,
    write(Next),
    bw(Body),
    !, true.

% Generic single argument: write and newline.
bw(Message) :- 
    !,
    write(Message),
    nl,
    true.

% Two-argument version: prefix + message.
bw(Prefix, Message) :- 
    !,
    write(Prefix),
    writeln(Message),
    true.
