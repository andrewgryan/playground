:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).

:- http_handler(root(hello_world), say_hi, []).
:- http_handler(root(data), get_data, []).
:- http_handler('/', http_reply_file('index.html', []), []).
:- http_handler('/prolog.jpg', http_reply_file('static/prolog.jpg', []), []).

:- initialization main.


main :-
    server(5000).


server(Port) :-
    http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
    reply_html_page(title('Hello, World!'),
                    [ h1('Hello, World!'), p('A paragraph...') ]).

get_data(_Request) :-
    reply_json(json([value='Hello, World!'])).

