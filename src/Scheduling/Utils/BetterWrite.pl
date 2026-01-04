:- module(betterwrite, [
   w/1,
   w/2,
   bw/1,
   bw/2
]).

bw(V):- w(V).
bw(S,P):- w(S,P).

w([]) :- !, true.

w([Next|Body]) :- 
    !,
    write(Next),
    w(Body),
    !, true.

w(Message) :- 
    !,
    write(Message),
    nl,
    true.

w(Prefix, Message) :- 
    !,
    write(Prefix),
    writeln(Message),
    true.
