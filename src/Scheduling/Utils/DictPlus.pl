:- module(DictPlus, [ 
    get_dict_or_var/3
]).


get_dict_or_var(Key, Dict, Value) :-
    (get_dict(Key, Dict, Value), !; var(Value)).