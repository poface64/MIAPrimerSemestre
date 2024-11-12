% Definimos los hechos iniciales
estado(izquierda, [col, lobo, cabra]).

% Definimos las reglas para generar los movimientos válidos con el barco
valido(izquierda, [Obj], derecha, []) :- member(Obj, [col, lobo, cabra]).
valido(derecha, [Obj], izquierda, [Obj]) :- member(Obj, [col, lobo, cabra]).

% Definimos la regla para resolver el problema
resuelve(Inicio, Fin) :-
    estado(Inicio, [col, lobo, cabra]),
    recorre([[Inicio, [col, lobo, cabra]]], [[Fin, [col, lobo, cabra]]]).

recorre(Camino, [Fin, Fin]) :-
    reverse(Camino, [Fin, Fin]).
recorre(Camino, [Actual, Fin]) :-
    valido(Actual, Obj, Siguiente, NuevosObj),
    not(member([Siguiente, NuevosObj], Camino)),
    recorre([[Siguiente, NuevosObj] | Camino], [Fin, Fin]).

  
