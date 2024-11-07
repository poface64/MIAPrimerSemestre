%%% Universidad Veracruzana
%%% Instituto de Investigaciones en Inteligencia Artificial
%%% Maestría en Inteligencia Artificial
%%% Alejandro Guerra-Hernández
% progenitor(X,Y).
% Verdadero si X es progenitor de Y.
% Estos predicados están adaptados de Bratko(2012), cap1.
progenitor(pam,bob).
progenitor(tom,bob).
progenitor(tom,liz).
%progenitor(liz,rog). YO añadi un caso para probar tio rog hijo de liz
progenitor(bob,ann).
progenitor(bob,pat).
progenitor(pat,jim).


%%%% mujer(X). %%%% 
% Verdadero si X es mujer.
mujer(pam).
mujer(liz).
mujer(pat).
mujer(ann).

%%%% hombre(X). %%%% 
% Verdadero si X es hombre.
hombre(tom).
hombre(bob).
hombre(jim).
%hombre(rog). rog es hombre

%%%%  vastago(X,Y). %%%% 
% Verdadero si X es vástago de la persona Y.
%
% ?- vastago(bob,X).
% X = pam ;
% X = tom.
vastago(X,Y) :- progenitor(Y,X).

%%%% madre(X,Y). %%%%
% Verdadero si X es madre de Y.
%
%% ?- madre(X,bob).
%% X = pam ;
%% false.
madre(X,Y) :-
    progenitor(X,Y),
    mujer(X).

%%%% abuela(X,Y). %%%%
% Verdadero si X es abuela de Y.
%% ?- abuela(X, ann).
%% X = pam ;
%% false.
abuela(X,Y) :-
    progenitor(X,Z),
    progenitor(Z,Y),
    mujer(X).

%%%% hermana(X,Y). %%%%
% Verdadero si X es hermana de Y.
%% ?- hermana(ann,X).
%% X = pat.
hermana(X,Y) :-
    progenitor(Z,X),
    progenitor(Z,Y),
    mujer(X), % Se modifico la condición de mujer(y) a mujer(x)
    dif(X,Y).

%%%% ancestro(X,Z). %%%%
% Verdadero si X es ancestro/a de Z.
%
%% ?- ancestro(pam,X).
%% X = bob ;
%% X = ann ;
%% X = pat ;
%% X = jim ;
%% false.
ancestro(X,Z) :-
    progenitor(X,Z).

ancestro(X,Z) :-
    progenitor(X,Y),
    ancestro(Y,Z).

%%%% APORTACIONES DE ANGEL AL PROGRAMA %%%%

%%%% AGREGAR AL HERMANO %%%%
hermano(X,Y) :-
    progenitor(Z,X),
    progenitor(Z,Y),
    hombre(X), % Se modifico la condición de que sea hombre(X)
    dif(X,Y). % que no sea el mismo, no puede ser hermano de el mismo.

%%%% AGREGAR A LA TÍA %%%%

tia(X,Y):- %% X es tía de Y
    progenitor(Z,Y), % Encontrar el progenitor Z de Y
    hermana(X,Z). % Comprobar si X es hermana del progenitor Z


% tia(ann,Y). Computar la siguiente meta da como resultado:
% Ann es tía de Jim y nada más

% tia(X,Y). Computar la siguiente meta da como resultado:
% Liz es tía de Ann
% Liz es tía de Pat
% Ann es tía de Jim y nada más

%%%% AGREGAR AL LA TÍO %%%%

tio(X,Y):- %% X es tio de Y
    progenitor(Z,Y), % Encontrar el progenitor Z de Y
    hermano(X,Z). % Comprobar si X es hermano del progenitor Z

% tio(X,Y). Computar la siguiente meta da como resultado:
%false

% Cabe mencionar que en la familia de este ejemplo no hay tios, solo tias,
% Por ello es que prolog da false, porque no hay ninguna clausula en el programa
% que satisfaga el requisito para que un miembro se considere tio de otro.

%%%% Defina una meta para computar quienes son sobrinos en esa familia %%%%

% Asumiendo que se sabe que no hay tios, bastaria con ejecutar:
% tia(_,X). dicha meta devuelve:
% X = ann; X = pat; X = jim, false.

% Suponiendo que hay tias y tios, se puede computar la siguiente meta:
% tia(_,X);tia(_,X). dicha meta devuelve:
% X = ann; X = pat; X = jim, false.








