:- module(math_statistics, [
    mean/2,
    variance/2,
    standard_deviation/2
]).

mean(Sample, Median) :-
    sum_list(Sample, Sum),
    length(Sample, Len),
    Median is Sum/Len.

variance(Sample, Variance):-
    mean(Sample, Mean),
    length(Sample, Length),
    findall(Dev, (
        member(P, Sample),
        Dif is P-Mean,
        Dev is Dif*Dif
    ), VLTemp),
    sum_list(VLTemp, VLSum),
    Variance is VLSum/Length.

standard_deviation(Sample, StdDev):-
    variance(Sample, Variance),
    sqrt(Variance, StdDev).