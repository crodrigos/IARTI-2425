:- module(rootController, []).


:-use_module([
    library(http/http_server),
    library(http/http_header),
    library(http/http_client),
    library(http/http_json)
]).

% % / route
% :- http_handler(
%     /,
%     http_redirect(moved, location_by_id(root_route)),
%     []).
% :- http_handler(
%     root(home), 
%     root_route, 
%     []
% ).

% root_route(_Request) :-
%     reply_html_page(
%         title('Scheduler'),
%         [ 
%             h1('Hello world!'),
%             a(
%                 href("/schedule"), 
%                 p("Schedule")
%             )
%         ]).