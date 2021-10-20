:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(factorial).

:- http_handler(root(data), get_data, []).
:- http_handler(root('.'), http_reply_file('index.html', []), []).
:- http_handler(root('prolog.jpg'), http_reply_file('static/prolog.jpg', []), []).

:- initialization main.


main :-
    server(5000).


server(Port) :-
    http_server(http_dispatch, [port(Port)]).

get_data(Request) :-
    http_parameters(Request, [seed(Seed, [ between(0, 150) ])]),
    factorial(Seed,X),
    reply_json(json([value=X])).

