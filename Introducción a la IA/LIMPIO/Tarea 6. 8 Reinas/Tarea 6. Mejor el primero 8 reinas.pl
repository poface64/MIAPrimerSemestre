
%%%% Función de primero el mejor %%%%

%%% Sucesor, heurística y meta son predicados dinámicos.
:- dynamic
       s/3,
       h/2,
       meta/1.
%%% Primero mejor modificado para encontrar solo la primera solución
% primeroMejor(Inicio, Sol): Sol es un camino de Inicio a la meta
% Asumimos 9999 > que todo f-valor
primeroMejor(Inicio, Sol) :-
  expandir([], l(Inicio,0/0), 9999, _, si, Sol).

% expandir(Camino, Arbol, Umbral, Arbol1, Resuelto, Sol):
%   Camino entre Inicio y Arbol,
%   Arbol1 es Arbol expandido bajo la Umbral,
%   Si meta es encontrada Sol es la solución y  Resuelto = si
%  Caso 1: nodo hoja meta, construir camino solución
expandir(Camino, l(Nodo, _), _, _, si, [Nodo|Camino]) :-
   meta(Nodo).

% Caso 2: nodo hoja, f-valor < Umbral
% Generar sucesores y expandir bajo la Umbral.
expandir(Camino, l(Nodo,F/G), Umbral, Arbol1, Resuelto, Sol)  :-
  F =< Umbral,
  ( bagof( Nodo2/C, (s(Nodo,Nodo2,C), \+ member(Nodo2,Camino) ), Succ), 
    !,                                    % N tiene sucesores
    listaSucs(G, Succ, As),               % Construir subárboles As
    mejorF(As, F1),                       % f-valor del mejor sucesor
    expandir(Camino, t(Nodo,F1/G,As), Umbral, Arbol1, Resuelto, Sol)
  ;
    Resuelto = nunca                      % N sin sucesores
  ).
%  Caso 3: no hoja, f-valor < Umbral
%  Expandir el subárbol más promisorio; dependiendo de 
%  resultados, continuar decidirá como proceder.
expandir(Camino, t(Nodo,F/G,[A|As]), Umbral, Arbol1, Resuelto, Sol)  :-
  F =< Umbral,
  mejorF(As, MejorF), min(Umbral, MejorF, Umbral1),          % Umbral1 = min(Umbral,MejorF)
  expandir([Nodo|Camino], A, Umbral1, A1, Resuelto1, Sol),
  continuar(Camino, t(Nodo,F/G,[A1|As]), Umbral, Arbol1, Resuelto1, Resuelto, Sol).

%  Caso 4: no hoja con subárboles vacío
%  Fallo
expandir( _, t(_,_,[]), _, _, nunca, _) :- !.
%  Caso 5:  f-valor > Umbral
%  Arbol debe crecer.
expandir( _, Arbol, Umbral, Arbol, no, _)  :-
  f(Arbol, F), F > Umbral.
% continuar( Camino, Arbol, Umbral, NuevoArbol, SubarbolResuelto, ArbolResuelto, Sol)
continuar( _, _, _, _, si, si, _).

continuar(Camino, t(Nodo,_/G,[A1|As]), Umbral, Arbol1, no, Resuelto, Sol)  :-
  insertar(A1, As, NAs),
  mejorF(NAs, F1),
  expandir(Camino, t(Nodo,F1/G,NAs), Umbral, Arbol1, Resuelto, Sol).

continuar(Camino, t(Nodo,_/G,[_|As]), Umbral, Arbol1, nunca, Resuelto, Sol)  :-
  mejorF(As, F1),
  expandir(Camino, t(Nodo,F1/G,As), Umbral, Arbol1, Resuelto, Sol).

% listaSucs(G0, [ Nodo1/Costo1, ...], [ l(MejorNodo,MejorF/G), ...]):
% ordena la lista de hojas por F-valor
listaSucs( _, [], []).
listaSucs(G0, [N/C|NCs], As)  :-
  G is G0 + C,
  h(N, H),                             % Heuristica h(N)
  F is G + H,
  listaSucs(G0, NCs, As1),
  insertar(l(N,F/G), As1, As).
% Insertar A en una lista de arboles As, preservando orden por
% f-valor.
insertar(A, As, [A|As])  :-
  f(A, F), mejorF(As, F1),
  F =< F1, !.
insertar(A, [A1|As], [A1|As1])  :-
  insertar(A, As, As1).

% Extraer f-value
f(l(_,F/_),F).        % f-valor de una hoja
f(t(_,F/_,_),F).      % f-valor de un árbol
mejorF([A|_],F)  :-   % mejor F de una lista de árboles
  f(A,F).
mejorF([],9999).       
min(X,Y,X)  :-
  X =< Y, !.
min(_,Y,Y).


%%%% Representación del problema de las 8 reinas %%%%
%%%% y su compaginación con primero el mejor %%%%
%%% Representación del estado inicial
estado_inicial([]).

%%% Meta: la meta es tener 8 reinas en el tablero sin conflictos.
meta(Tablero) :-
    length(Tablero, 8), 
    \+ en_conflicto(Tablero).

%%% Sucesores: coloca una reina en la siguiente fila
s(Tablero, [Fila/Columna | Tablero], 1) :-
    length(Tablero, N),                % Determina cuántas reinas están ya en el tablero
    Fila is N + 1,                     % Calcula la fila actual (siguiente vacía)
    between(1, 8, Columna),            % Intenta ubicar la reina en una columna válida
    \+ en_conflicto([Fila/Columna | Tablero]).

%%% Heurística: calcula el número de conflictos en el tablero
h(Tablero, Conflictos) :-
    findall(1, (member(F1/C1, Tablero), member(F2/C2, Tablero), F1 < F2, conflicto(F1, C1, F2, C2)), ConflictosList),
    length(ConflictosList, Conflictos).
%%% Reglas de conflicto: dos reinas están en conflicto si están en la misma columna o diagonal
conflicto(F1, C1, F2, C2) :-
    C1 =:= C2;                         % Mismo columna
    abs(F1 - F2) =:= abs(C1 - C2).     % Misma diagonal
%%% Verifica si hay conflictos en un tablero
en_conflicto(Tablero) :-
    member(F1/C1, Tablero),
    member(F2/C2, Tablero),
    F1 < F2,
    conflicto(F1, C1, F2, C2).
%%% Resetea el problema antes de una nueva ejecución
reset :-
    retractall(s/3),
    retractall(h/2),
    retractall(meta/1).
%%% Definir el problema de 8 reinas
iniciar :-
    reset,
    assert(estado_inicial([])),
    assert((s(Tablero, [Fila/Columna | Tablero], 1) :-
            length(Tablero, N), 
            Fila is N + 1, 
            between(1, 8, Columna),
            \+ en_conflicto([Fila/Columna | Tablero]))),
    assert((h(Tablero, Conflictos) :-
            findall(1, (member(F1/C1, Tablero), member(F2/C2, Tablero), F1 < F2, conflicto(F1, C1, F2, C2)), ConflictosList),
            length(ConflictosList, Conflictos))),
    assert((meta(Tablero) :-
            length(Tablero, 8), 
            \+ en_conflicto(Tablero))).

%%%% Auxiliar para la primer salida %%%%
primera_lista([], []).
primera_lista([Primera | _], Primera).

%%%% El problema se definio unicamente en terminos de las 8 reinas %%%%
%%%% A continuación se establece una consulta para verificar el funcionamiento %%%%
%%%% La salida de la función se da en terminos de filas y columnas, por lo que cada
%%%% elemento de la lista corresponde a ese arreglo, primero fila y luego columna tal que F/C
%%%% Para representar la posición de la reina en el tablero

%%%% Consulta para extraer la primer solución %%%%
%primeroMejor([], Sol),primera_lista(Sol, Sol1).

% Reulstado:
% Sol1 = [8/6, 7/4, 6/7, 5/1, 4/3, 3/5, 2/2, 1/8]

% Representación tipo tablero
% |F/C|1|2|3|4|5|6|7|8|
% |1  | | | | | | | |x|
% |2  | |x| | | | | | |
% |3  | | | | |x| | | |
% |4  | | |x| | | | | |
% |5  |x| | | | | | | |
% |6  | | | | | | |x| |
% |7  | | | |x| | | | |
% |8  | | | | | |x| | |




