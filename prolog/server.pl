% Standard library
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

% Local modules
:- use_module(factorial).

% Routes
:- http_handler(root('.'), http_reply_file('dist/index.html', []), []).
:- http_handler(root(data), get_data, []).

% Static file server
:- multifile http:location/3.
:- dynamic http:location/3.

http:location(files, '/assets', []).

:- http_handler(files(.), http_reply_from_files(static, []), [prefix]).

% Start program
:- initialization main.

main :-
    server(5000).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

get_data(Request) :-
    http_parameters(Request, [seed(Seed, [ between(0, 150) ])]),
    factorial(Seed,X),
    reply_json(json([value=X])).
