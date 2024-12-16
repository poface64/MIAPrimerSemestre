%%% Definición del estado objetivo
objetivo([1, 2, 3, 4, 5, 6, 7, 8, 0]).

%%% Movimiento permitido: intercambiar el espacio vacío con un vecino
movimiento(Estado, NuevoEstado) :-
    nth0(Pos0, Estado, 0),
    vecino(Pos0, Pos1),
    nth0(Pos1, Estado, Valor),
    sustituir(Pos0, Pos1, 0, Valor, Estado, NuevoEstado).

%%% Definir vecinos (posiciones adyacentes) en la matriz 3x3
vecino(0, 1). vecino(0, 3).
vecino(1, 0). vecino(1, 2). vecino(1, 4).
vecino(2, 1). vecino(2, 5).
vecino(3, 0). vecino(3, 4). vecino(3, 6).
vecino(4, 1). vecino(4, 3). vecino(4, 5). vecino(4, 7).
vecino(5, 2). vecino(5, 4). vecino(5, 8).
vecino(6, 3). vecino(6, 7).
vecino(7, 4). vecino(7, 6). vecino(7, 8).
vecino(8, 5). vecino(8, 7).

%%% Sustituir elementos en una lista
sustituir(I1, I2, V1, V2, Lista, NuevaLista) :-
    same_length(Lista, NuevaLista),
    nth0(I1, NuevaLista, V2),
    nth0(I2, NuevaLista, V1),
    forall(nth0(I, Lista, V), (I \= I1, I \= I2 -> nth0(I, NuevaLista, V))).

%%% Distancia de Manhattan como heurística
heuristica(Estado, H) :-
    objetivo(Objetivo),
    findall(Distancia, (
        nth0(I, Estado, X), X \= 0,
        nth0(Ig, Objetivo, X),
        PosX is I mod 3, PosY is I // 3,
        PosXg is Ig mod 3, PosYg is Ig // 3,
        Distancia is abs(PosX - PosXg) + abs(PosY - PosYg)
    ), Distancias),
    sum_list(Distancias, H).

%%% A* Search
astar(EstadoInicial, Solucion) :-
    heuristica(EstadoInicial, H),
    astar([[EstadoInicial, 0, H, [EstadoInicial]]], Solucion).

astar([[_Estado, _G, _H, Camino]|_], Camino) :-
    Camino = [Ultimo|_],
    objetivo(Ultimo).

astar([Nodo|Otros], Solucion) :-
    Nodo = [Estado, G, _H, Camino],
    findall(
        [NuevoEstado, G1, H1, [NuevoEstado|Camino]],
        (
            movimiento(Estado, NuevoEstado),
            \+ member(NuevoEstado, Camino),
            G1 is G + 1,
            heuristica(NuevoEstado, H1)
        ),
        Sucesores
    ),
    append(Otros, Sucesores, Abiertos),
    sort(2, @=<, Abiertos, Ordenados),
    astar(Ordenados, Solucion).

%%% Ejemplo de uso
% Resolver un puzzle inicial
% ?- astar([1, 2, 3, 4, 0, 5, 7, 8, 6], Solucion).