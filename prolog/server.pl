:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

:- http_handler(root(hello_world), say_hi, []).
:- http_handler(root(data), get_data, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
    reply_html_page(title('Hello, World!'),
                    [ h1('Hello, World!'), p('A paragraph...') ]).

get_data(_Request) :-
    reply_json(json([hello='world'])).
