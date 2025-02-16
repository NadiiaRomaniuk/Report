%Тут оголошуються факти, які вказують, хто є жінкою і 
% хто є чоловіком. Слугує основою для інших відношень.

% Define female relationship
female(jane).
female(anna).
female(mary).
female(catlyn).
female(sarah).

% Define male relationship
male(john).
male(jim).
male(peter).
male(alex).
male(tom).

% Факти parent(X, Y) означають, що X є батьком або матір'ю Y. 
% Наприклад, parent(john, jim). означає, що john є батьком jim.

% Define parent relationships
parent(john, jim).
parent(jane, jim).
parent(jim, anna).
parent(jim, tom).
parent(anna, peter).
parent(tom, mary).

% Define mother father relationship
%mother(X) :- parent(X, _), female(X).
mother(X, Y) :- female(X), parent(X, Y). % X є мамою Y, якщо вона є 'батьком' (тут і надалі - parent, а не father) та жінка;

%female(X) та parent(X, Y) - комутативні. Не впливає на результат, проте може вплинути на швидкість виконання зпиту
%У наявній базі даних ефектиіним буде саме порядок female, parent, адже female є 5 та parent є 6

mother(X) :- mother(X, _). % будь-хто є матір'ю, якщо існує дитина % унарний предикат (властивість)


%father(X) :- parent(X, _), male(X).
father(X, Y) :- parent(X, Y), male(X). % аналогічно з батьком
father(X) :- father(X, _).

% Define ancestor relationship
ancestor(X, Y) :- parent(X, Y). % X є предком Y, якщо X є його безпосереднім батьком
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y). % X є предком Y, якщо він є батьком когось, хто є предком Y
% ancestor(X, Y) :- ancestor(Z, Y), parent(X, Z). % так робити не можна - проограма не завершиться 

% Define descendant relationship
%descendant(X, Y) :- parent(Y, X). % X є нащадком Y, тобто Y є предком X.
%descendant(X, Y) :- parent(Z, X), descendant(Z, Y). Якщо Z є батьком X, і Z є нащадком Y, тоді X теж є нащадком Y.
descendant(X, Y) :- ancestor(Y, X). % X є нащадком Y, якщо Y є предком Х

% Define blood relative relationship Важливо: двоє людей є кровними родичами, якщо у них є спільний предок.
blood_relative(X, Y) :- ancestor(Z, X), ancestor(Z, Y).
%blood_relative(X, Y) :- descendant(Z, X), descendant(Z, Y). % саме тому цей рядок є зайвим

% Define sister brother relationship
sister(X, Y) :- parent(Z, X), parent(Z, Y), female(X), X \= Y. 
% означає, що X є сестрою Y, якщо в них один і той самий батько (Z), 
% X є жінкою і вони не є однією особою.
% 
%sister(X, Y) :- X \= Y, parent(Z, X), parent(Z, Y), female(X). 
% not(X = Y). предикат not() приймає знайчення істини лише тоді, коли X не може дорівнювати Y
%саме тому не варто міняти місцями

brother(X, Y) :- parent(Z, X), parent(Z, Y), male(X), X \== Y.
% аналогічно з братом

% Define aunt relationship
aunt(X, Y) :- parent(Z, Y), sister(X, Z). %X є тіткою Y, якщо X є сестрою одного з батьків Y.
%son-in-law(X, Y) :- parent(Z, Y), brother(X, Z).
%визначено неправильно. 

% Визначаємо, що X є зятем Y, якщо X одружений з дочкою Y
son_in_law(X) :- married(X, D), parent(_, D), female(D).
% Предикат для шлюбу
married(james, anna).

% Define nephew niece relationship
nephew(X, Y) :- parent(Y, Z), sister(Z, W), parent(W, X).
niece(X, Y) :- parent(Y, Z), brother(Z, W), parent(W, X).
%X — племінниця Y, якщо: Y є батьком/матір’ю Z, Z має брата W, W є батьком/матір’ю X.

person(X) :- female(X). %можна сказати 'об'єднання'
person(X) :- male(X).
%X є людиною, якщо X є або жінкою, або чоловіком.