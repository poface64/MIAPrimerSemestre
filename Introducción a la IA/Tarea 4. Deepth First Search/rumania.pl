% Función principal %
go(Start, Goal) :-
    path(Start, Goal, [Start], Path, 0, TotalDistance) ->
        (write('Path found: '), nl, reverse_print_path(Path, TotalDistance));
    write('No path exists between the cities.'), nl.

% Caso base: Encuentra la meta
path(Goal, Goal, Path, Path, Distance, Distance).

% Caso recursivo: Búsqueda
path(State, Goal, Been_list, Path, CurrentDistance, TotalDistance) :-
    connection(State, Next, Distance),
    \+ member(Next, Been_list), % Previene ciclos
    NewDistance is CurrentDistance + Distance,
    path(Next, Goal, [Next|Been_list], Path, NewDistance, TotalDistance).

% Imprimir el camino con las distancias.
reverse_print_path(Path, TotalDistance) :-
    reverse(Path, ReversedPath),
    write_path(ReversedPath),
    format('Total distance: ~w km~n', [TotalDistance]).

write_path([]).
write_path([_]).
write_path([City1, City2|Rest]) :-
    connection(City1, City2, Distance),
    format('~w -> (~w km) -> ', [City1, Distance]),
    write_path([City2|Rest]).

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
connection(craiova, ganeasa, 47).
connection(ganeasa, craiova, 47).
connection(ganeasa, rimnicu_vilcea, 84).
connection(rimnicu_vilcea, ganeasa, 84).


