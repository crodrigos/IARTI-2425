:- use_module([
    'ListUtils.pl',
    'Timer.object.pl',
    'BetterWrite.pl'
]).


divideListXParts(_, 0, []) :- !.
divideListXParts([], _, []) :- !.
divideListXParts(L, X, [Removed|Rest]) :-
    length(L, Len),
    PartSize is Len // X,
    removeFirstX(L, PartSize, Removed, Rest1),
    X1 is X - 1,
    divideListXParts(Rest1, X1, Rest).


removeFirstX([], _, [], []) :- !.
removeFirstX(L, 0, [], L) :- !.
removeFirstX([H|T], X, [H|Removed], Rest) :-
    X > 0,
    X1 is X - 1,
    removeFirstX(T, X1, Removed, Rest).

reorderlists(LL, []):-
    matrix_empty(LL),!.
reorderlists(ListOfList, [Min|MinL]):-
    getMin(ListOfList, Min, Rest),
    reorderlists(Rest, MinL).

matrix_empty([]).
matrix_empty([[]|Rows]) :-
    matrix_empty(Rows).

getMin([], _, []) :- fail.   % no rows at all → no minimum

getMin(Matrix, Min, RestMatrix) :-
    findall((H,T), (member(Row, Matrix), Row = [H|T]), Pairs),
    Pairs \= [],
    findall(H, member((H,_), Pairs), Heads),
    min_list(Heads, Min),
    rebuild(Matrix, Min, RestMatrix).

rebuild([], _, []).
rebuild([[Min|T]|Rs], Min, [T|Rest]) :- !,
    rebuild(Rs, Min, Rest).
rebuild([R|Rs], Min, [R|Rest]) :-
    rebuild(Rs, Min, Rest).




mergeSort([A],_,[A]):-!.
mergeSort([A,B],_,R):-!,
    order(A,B,C,D),
    R = [C,D].
mergeSort(L,D,R):-
    divideListXParts(L,D,SublistL),
    findall(ML, (
        member(Sublist, SublistL),
        mergeSort(Sublist,D, ML)
    ), RetSubList),
    reorderlists(RetSubList,R).

mergeSort(L,R):-
    mergeSort(L,2,R).

order(A,B, C, D):-
    A @< B,!,
    C=A, D=B.
order(A,B,B,A).





mergeSortTh([A],_) :- !,thread_exit([A]).

mergeSortTh([A,B],_) :- !,
    order(A,B,C,D),
    R = [C,D],
    thread_exit(R).

mergeSortTh(L, D) :-
    divideListXParts(L, D, SubLists),
    create_merge_threads(SubLists, D, ThreadIDs),
    collect_merge_results(ThreadIDs, Results),
    reorderlists(Results, R),
    thread_exit(R).


create_merge_threads([], _, []).
create_merge_threads([S|Ss], D, [Id|Ids]) :-
    thread_create(mergeSortTh(S, D), Id, [detached(false)]),
    create_merge_threads(Ss, D, Ids).

collect_merge_results([], []).
collect_merge_results([Id|Ids], [Result|Rs]) :-
    thread_join(Id, exited(Result)),
    collect_merge_results(Ids, Rs).


mergeSortTh(L,D,R):-
    create_merge_threads([L],D,Ids),
    collect_merge_results(Ids, R).

test(mergesort):-
    findall(_, (
        DL = [2,4,8,16,64,128,256,512,1024],

        range(3000, L),
        random_permutation(L, LR),

        member(D, DL),

        reset_timer, start_timer,
        mergeSort(LR, D, R),
        get_elapsed_time(T),

        bw("Divs: ", D),
        bw("Time: ", T), nl
    ), _).
    
test(mergethread):-
    findall(_, (
        DL = [2,4,8,16,64,128,256,512,1024],

        range(100, L),
        random_permutation(L, LR),

        member(D, DL),

        bw("Custom Sort"),
        reset_timer, start_timer,
        mergeSortTh(LR, D, R),
        get_elapsed_time(T),
        bw("Divs: ", D),
        bw("Time: ", T), nl,

        bw("Default Sort"),
        reset_timer, start_timer,
        sort(LR, R),
        get_elapsed_time(T),
        bw("Divs: ", D),
        bw("Time: ", T), nl
    ), _).