:- use_module(library(http/http_server)).

:- initialization(http_server([port(8080)])).


% index.html route
:- http_handler('/index.html', write_index, []).

% / Route 
:- http_handler(root(.), user(Method, User),
                [ 
                    method(Method),
                    methods([get,post,put])
                ]).

user(get, User, Request).
user(post, User, Request).