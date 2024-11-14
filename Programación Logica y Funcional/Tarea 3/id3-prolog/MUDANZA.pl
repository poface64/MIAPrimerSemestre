% Estado inicial y estado objetivo
estado_inicial([dueño/casa1, perro/casa1, gato/casa1, hamster/casa1]).
estado_final([dueño/casa2, perro/casa2, gato/casa2, hamster/casa2]).

% Define el movimiento de cambio de casa
opuesto(casa1, casa2).
opuesto(casa2, casa1).

% Estado seguro: El dueño debe estar donde estén juntos el perro y el gato, o el gato y el hámster
estado_seguro([dueño/D, perro/P, gato/G, hamster/H]) :-
    (G = H -> D = G ; true), 
    (P = G -> D = P ; true).

% Movimientos posibles
mover([dueño/D, perro/D, gato/G, hamster/H], [dueño/D2, perro/D2, gato/G, hamster/H]) :- 
    opuesto(D, D2), 
    estado_seguro([dueño/D2, perro/D2, gato/G, hamster/H]).
    
mover([dueño/D, perro/P, gato/D, hamster/H], [dueño/D2, perro/P, gato/D2, hamster/H]) :- 
    opuesto(D, D2), 
    estado_seguro([dueño/D2, perro/P, gato/D2, hamster/H]).
    
mover([dueño/D, perro/P, gato/G, hamster/D], [dueño/D2, perro/P, gato/G, hamster/D2]) :- 
    opuesto(D, D2), 
    estado_seguro([dueño/D2, perro/P, gato/G, hamster/D2]).
    
mover([dueño/D, perro/P, gato/G, hamster/H], [dueño/D2, perro/P, gato/G, hamster/H]) :-  
    opuesto(D, D2), 
    estado_seguro([dueño/D2, perro/P, gato/G, hamster/H]).

% Resuelve el problema, guardando el camino
resolver(Camino) :-
    estado_inicial(Inicio),
    buscar_camino(Inicio, [Inicio], Camino).

% Busca el camino al objetivo recursivamente
buscar_camino(Estado, Camino, Camino) :- estado_final(Estado).
buscar_camino(Estado, Visitados, Camino) :-
    mover(Estado, SiguienteEstado),
    \+ member(SiguienteEstado, Visitados),
    buscar_camino(SiguienteEstado, [SiguienteEstado|Visitados], Camino).

% Imprime cada paso en el camino
mostrar_camino([]).
mostrar_camino([Paso|Resto]) :-
    writeln(Paso),
    mostrar_camino(Resto).

