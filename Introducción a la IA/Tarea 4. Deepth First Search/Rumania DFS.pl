

% Función principal %
go(Start, Goal) :-
    empty_stack(Empty_been_list),
    stack(Start, Empty_been_list, Been_list),
    path(Start, Goal, Been_list, Path, 0, TotalDistance),
    write('Path found: '), nl,
    reverse_print_path(Path, TotalDistance).

% Caso base: Encuentra la meta
path(Goal, Goal, Been_list, Path, Distance, Distance) :-
    reverse_stack(Been_list, Path).

% Caso recursivo: Busqueda
path(State, Goal, Been_list, Path, CurrentDistance, TotalDistance) :-
    connection(State, Next, Distance),  % Get next possible move
    not(member_stack(Next, Been_list)), % Prevent cycles
    stack(Next, Been_list, New_been_list),
    NewDistance is CurrentDistance + Distance,
    path(Next, Goal, New_been_list, Path, NewDistance, TotalDistance).

% Predicado auxiliar para devolver la lista.
reverse_stack(Stack, Reversed) :-
    empty_stack(Empty),
    reverse_stack_helper(Stack, Empty, Reversed).
reverse_stack_helper(Empty, Reversed, Reversed) :-
    empty_stack(Empty).
reverse_stack_helper(Stack, Temp, Reversed) :-
    stack(Element, Rest, Stack),
    stack(Element, Temp, NewTemp),
    reverse_stack_helper(Rest, NewTemp, Reversed).

% Imprimir el camino con las distancias.
reverse_print_path(Path, TotalDistance) :-
    write_path(Path),
    format('Total distance: ~w km~n', [TotalDistance]).
write_path([]).
write_path([City|Rest]) :-
    write(City), nl,
    (Rest \= [] -> write('  |\n  v\n') ; true),
    write_path(Rest).



% Define bidirectional connections between cities with distances
% Format: connection(CityA, CityB, Distance).

connection(oradea, zerind, 71).
connection(zerind, oradea, 71).
connection(zerind, arad, 75).
connection(arad, zerind, 75).
connection(arad, timisoara, 118).
connection(timisoara, arad, 118).
connection(timisoara, lugoj, 111).
connection(lugoj, timisoara, 111).
connection(lugoj, mehadia, 70).
connection(mehadia, lugoj, 70).
connection(mehadia, dobreta, 75).
connection(dobreta, mehadia, 75).
connection(dobreta, craiova, 120).
connection(craiova, dobreta, 120).
connection(craiova, pitesti, 138).
connection(pitesti, craiova, 138).
connection(pitesti, bucharest, 101).
connection(bucharest, pitesti, 101).
connection(bucharest, giurgiu, 90).
connection(giurgiu, bucharest, 90).
connection(bucharest, urziceni, 85).
connection(urziceni, bucharest, 85).
connection(urziceni, hirsova, 98).
connection(hirsova, urziceni, 98).
connection(hirsova, eforie, 86).
connection(eforie, hirsova, 86).
connection(urziceni, vaslui, 142).
connection(vaslui, urziceni, 142).
connection(vaslui, iasi, 92).
connection(iasi, vaslui, 92).
connection(iasi, neamt, 87).
connection(neamt, iasi, 87).
connection(bucharest, fagaras, 211).
connection(fagaras, bucharest, 211).
connection(fagaras, sibiu, 99).
connection(sibiu, fagaras, 99).
connection(sibiu, rimnicu_vilcea, 80).
connection(rimnicu_vilcea, sibiu, 80).
connection(rimnicu_vilcea, craiova, 146).
connection(craiova, rimnicu_vilcea, 146).
connection(rimnicu_vilcea, pitesti, 97).
connection(pitesti, rimnicu_vilcea, 97).
connection(arad, sibiu, 140).
connection(sibiu, arad, 140).
% New connections from the modified map
connection(craiova, ganeasa, 47).
connection(ganeasa, craiova, 47).
connection(ganeasa, rimnicu_vilcea, 84).
connection(rimnicu_vilcea, ganeasa, 84).


%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property. We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs. Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

%%%%%%%%%%%%%%%%%%%% stack operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These predicates give a simple, list based implementation of stacks

% empty stack generates/tests an empty stack

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

empty_stack([]).

% member_stack tests if an element is a member of a stack

member_stack(E, S) :- member(E, S).

% stack performs the push, pop and peek operations
% to push an element onto the stack
% ?- stack(a, [b,c,d], S).
% S = [a,b,c,d]
% To pop an element from the stack
% ?- stack(Top, Rest, [a,b,c]).
% Top = a, Rest = [b,c]
% To peek at the top element on the stack
% ?- stack(Top, _, [a,b,c]).
% Top = a 

stack(E, S, [E|S]).

%%%%%%%%%%%%%%%%%%%% queue operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These predicates give a simple, list based implementation of 
% FIFO queues

% empty queue generates/tests an empty queue

empty_queue([]).

% member_queue tests if an element is a member of a queue

member_queue(E, S) :- member(E, S).

% add_to_queue adds a new element to the back of the queue

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

% remove_from_queue removes the next element from the queue
% Note that it can also be used to examine that element 
% without removing it

remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :- 
    append(First, Second, Concatenation).

%%%%%%%%%%%%%%%%%%%% set operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These predicates give a simple, 
% list based implementation of sets

% empty_set tests/generates an empty set.

empty_set([]).

member_set(E, S) :- member(E, S).

% add_to_set adds a new member to a set, allowing each element
% to appear only once

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T) :- !.
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new), !.

union([], S, S).
union([H|T], S, S_new) :- 
    union(T, S, S2),
    add_to_set(H, S2, S_new). 

intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
    member_set(H, S),
    intersection(T, S, S_new),!.
intersection([_|T], S, S_new) :-
    intersection(T, S, S_new),!.

set_diff([], _, []).
set_diff([H|T], S, T_new) :- 
    member_set(H, S), 
    set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :- 
    set_diff(T, S, T_new), !.

subset([], _).
subset([H|T], S) :- 
    member_set(H, S), 
    subset(T, S).

equal_set(S1, S2) :- 
    subset(S1, S2), subset(S2, S1).

empty_sort_queue([]).
member_sort_queue(E, S) :- member(E, S).
remove_sort_queue(First, [First|Rest], Rest).


