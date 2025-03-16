чоловік('Василь'). 
чоловік('Тарас'). 
чоловік('Петро'). 
чоловік('Степан'). 

жінка('Ганна'). 
жінка('Єва'). 
жінка('Надія'). 

добуток(Ч, Ж) :- чоловік(Ч), жінка(Ж). 

% Вилучення з «повного» добутку коментарями «%», щоб ділення не було порожнім 
% Якщо прибрати всі коментарі, то буде «повний» декартів добуток з 12 кортежами 

%ділене('Василь', 'Ганна'). 
%ділене('Василь', 'Єва'). 
ділене('Василь', 'Надія'). 
ділене('Тарас', 'Ганна'). 
%ділене('Тарас', 'Єва'). 
ділене('Тарас', 'Надія'). 
ділене('Петро', 'Ганна'). 
ділене('Петро', 'Єва'). 
ділене('Петро', 'Надія'). 
ділене('Степан', 'Ганна'). 
ділене('Степан', 'Єва'). 
ділене('Степан', 'Надія'). 


добуток_всі_ч_х_ж(Ч, Ж) :- ділене(Ч, _), жінка(Ж). 
% Декартів добуток "повний": всі чоловіки х всі жінки 

погані_Ч(Ч) :- ділене(Ч, _), жінка(Ж), \+(ділене(Ч, Ж)). 
% від "повного» декартового добутку віднімаємо наявних в ділемому: залишаться "погані", 
% яких нема у повному декартовому добутку. 
% Якщо відсутній в парі хоча б з одною жінкою, % то "поганий" 

% залишається від всіх чоловіків відкинути "поганих". 
ділення(Ч) :- ділене(Ч, _), \+(погані_Ч(Ч)). 
 
 
% Варіант 2. 
% Основне відношення r(A, B) 
r(ivan, math). 
r(ivan, physics). 
r(maria, math). 
r(maria, physics). 
r(petro, math). 
% Відношення s(B) – предмети, які кожен має знати 
s(math). 
s(physics). 

% Виводить усіх A, які проходять перевірку 
division(A) :-  
    r(A, _),  % Беремо всіх можливих кандидатів A 
    \+ (s(B), \+ r(A, B)).
% фільтруємо по гарним студентам або те саме: віднімаємо поганих. 

ділення_з_forall(Ч) :-  
    чоловік(Ч),  
    forall(жінка(Ж), ділене(Ч, Ж)). 

/** <examples> 
?- добуток(Ч, Ж). 
?- ділене(Ч, Ж). 
?- добуток_всі_ч_х_ж(Ч, Ж). 
?- погані_Ч(Ч). 
?- ділення(Ч). % => Res 
?- division(A). 
?- ділення_з_forall(Ч).
*/ 