% Тут оголошуються факти, які вказують, хто є жінкою (female/1),
% а також правила для визначення чоловіків та людей.

% Визначення жінок
female(jane).
female(anna).
female(mary).
female(catlyn).
female(sarah).

% Визначення чоловіків через жінок
male(X) :- person(X), \+ female(X).

% Визначення людей
person(X) :- female(X).
person(john).
person(jim).
person(peter).
person(alex).
person(tom).

% Факти parent(X, Y) означають, що X є батьком або матір'ю Y. 
% Наприклад, parent(john, jim). означає, що john є батьком jim.

% Визначення батьківських відносин
parent(john, jim).
parent(jane, jim).
parent(jim, anna).
parent(jim, tom).
parent(anna, peter).
parent(tom, mary).

% Визначення матері та батька
mother(X, Y) :- female(X), parent(X, Y).
mother(X) :- mother(X, _).

father(X, Y) :- male(X), parent(X, Y).
father(X) :- father(X, _).

% Визначення предків
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Визначення нащадків
descendant(X, Y) :- ancestor(Y, X).

% Визначення кровної спорідненості
blood_relative(X, Y) :- ancestor(Z, X), ancestor(Z, Y).

% Визначення сестер та братів
sister(X, Y) :- parent(Z, X), parent(Z, Y), female(X), X \= Y.
brother(X, Y) :- parent(Z, X), parent(Z, Y), male(X), X \= Y.

% Визначення тіток
aunt(X, Y) :- parent(Z, Y), sister(X, Z).

% Визначення зятя
son_in_law(X, Y) :- married(X, D), parent(Y, D), female(D).

% Факти про шлюб
married(james, anna).

% Визначення племінників та племінниць
nephew(X, Y) :- parent(Y, Z), sister(Z, W), parent(W, X).
niece(X, Y) :- parent(Y, Z), brother(Z, W), parent(W, X).