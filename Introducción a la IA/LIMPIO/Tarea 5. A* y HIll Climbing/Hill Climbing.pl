
%%%% Puzzle de 8 piezas con algoritmo Hill Climbing %%%%

%% Representación del objetivo
objetivo([1, 2, 3, 4, 5, 6, 7, 8, 0]).

%% Heurística: distancia de Manhattan
heuristica(Tablero, Costo) :-
    objetivo(Objetivo),
    findall(Distancia, (nth0(I, Tablero, X), X \= 0, nth0(J, Objetivo, X), manhattan(I, J, Distancia)), Distancias),
    sum_list(Distancias, Costo).

% Distancia de Manhattan
manhattan(I1, I2, Distancia) :-
    fila_col(I1, F1, C1),
    fila_col(I2, F2, C2),
    Distancia is abs(F1 - F2) + abs(C1 - C2).

fila_col(Index, Fila, Columna) :-
    Fila is Index // 3,
    Columna is Index mod 3.

%% Movimientos posibles
mover(Tablero, NuevoTablero) :-
    nth0(PosVacia, Tablero, 0),
    vecino(PosVacia, PosVecino),
    intercambiar(Tablero, PosVacia, PosVecino, NuevoTablero).

vecino(Pos, Vecino) :-
    fila_col(Pos, F, C),
    (   (F2 is F - 1, C2 = C);   % Arriba
        (F2 is F + 1, C2 = C);   % Abajo
        (F2 = F, C2 is C - 1);   % Izquierda
        (F2 = F, C2 is C + 1)),  % Derecha
    F2 >= 0, F2 < 3, C2 >= 0, C2 < 3,
    Vecino is F2 * 3 + C2.

intercambiar(Tablero, Pos1, Pos2, NuevoTablero) :-
    nth0(Pos1, Tablero, X1),
    nth0(Pos2, Tablero, X2),
    setnth(Tablero, Pos1, X2, TempTablero),
    setnth(TempTablero, Pos2, X1, NuevoTablero).

setnth([_|T], 0, X, [X|T]).
setnth([H|T], N, X, [H|R]) :-
    N > 0, N1 is N - 1, setnth(T, N1, X, R).

%% Algoritmo Hill Climbing
hill_climbing(Estado, Camino) :-
    objetivo(Estado),
    Camino = [Estado].
hill_climbing(Estado, [Estado|Camino]) :-
    findall(NuevoEstado, mover(Estado, NuevoEstado), Vecinos),
    mejor_vecino(Vecinos, Estado, MejorEstado),
    MejorEstado \= Estado,
    hill_climbing(MejorEstado, Camino).

mejor_vecino(Vecinos, Estado, MejorEstado) :-
    heuristica(Estado, CostoActual),
    findall(Costo-Vecino, (member(Vecino, Vecinos), heuristica(Vecino, Costo)), CostosVecinos),
    keysort(CostosVecinos, [MejorCosto-MejorEstado|_]),
    (MejorCosto < CostoActual -> true ; MejorEstado = Estado).

%% Ejecución
resolver_hill_climbing(EstadoInicial, Camino) :-
    hill_climbing(EstadoInicial, Camino).




