female(jane).
female(anna).
female(mary).
female(catlyn).
female(sarah).
female(sasha).

male(john).
male(jim).
male(peter).
male(alex).
male(tom).
male(sasha).

%Демонстрація оберненого замикання бінарного відношення
reverse_closure(X, Y) :- parent(Y, X).

%Демонстрація рефлексивного замикання бінарного відношення
reflex(X, X). 

%Демонстрація симетричного замикання бінарного відношення
spouse(john, sarah).
%symmetric_closure(X, Y):-spouse(X, Y).
%symmetric_closure(Y, X):-spouse(X, Y).
symmetric_closure(Y, X):-spouse(X, Y).
symmetric_closure(Y, X):- symmetric_closure(X,Y).


%Демонстрація декартового добутку
couple(X,Y):-male(X), female(Y).

%Демонстрація проєкції
return_female(Y) :- couple(_, Y).

%Демонстрація об'єднання 
%person(X):-female(X);male(X).
person(X) :- female(X). 
person(X) :- male(X).

%Демонстрація тета-обмеження
birth_year(john, 1995).
birth_year(anna, 2001).
birth_year(jane, 1966).
birth_year(jim, 2010).

%young(X) :- birth_year(X, Y), Y > 2000.
young(X) :- birth_year(X, 2001).

%демонстрація тета-з'єднання
m(X, Y, Z) :- parent(X, Y), parent(Y, Z).

parent(jane, jim).
parent(jack, jim).
parent(jim, kate).

%parent(jane, jim).
%parent(jim, anna).
%parent(jim, tom).
%parent(anna, peter).
%parent(tom, mary).


%Демонcтрація транзитивного замикання
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y). 

%Демонстрація обертання бінарного відношення
descendant(X, Y) :- ancestor(Y, X). 

%Демонcтрація перетину
спільні_імена(X):- female(X), male(X). 