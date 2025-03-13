female(jane).
female(anna).
female(mary).
female(catlyn).
female(sarah).

person(X) :- female(X).
person(john).
person(jim).
person(peter).
person(alex).
person(tom).

%Демонстрація віднімання
male(X) :- person(X), \+ female(X).