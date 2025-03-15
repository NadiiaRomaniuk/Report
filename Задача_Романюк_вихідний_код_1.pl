% tree(+Tree).
% Оголошуємо базу даних для дерева:
% Порожнє дерево (nil) або вузол з коренем і двома піддеревами.
tree(nil).  
tree(t(_, Left, Right)) :- tree(Left), tree(Right).

% insert(-NewTree, +Tree, +Key).
% Предикат для вставки нового елемента в дерево.
insert(X, nil, t(X, nil, nil)).  % Якщо дерево порожнє, вставляємо елемент X.
insert(X, t(Root, Left, Right), t(Root, NewLeft, Right)) :- X =< Root, insert(X, Left, NewLeft).  
% Якщо елемент X менший або рівний кореню, вставляємо в ліве піддерево.
insert(X, t(Root, Left, Right), t(Root, Left, NewRight)) :- X > Root, insert(X, Right, NewRight).  
% Якщо елемент X більший за корінь, вставляємо в праве піддерево.

% delete(-NewTree, +Tree, +Key).
% Предикат для видалення елемента з дерева.
delete(X, t(X, Left, nil), Left).  
% Якщо елемент X - це вузол з правим піддеревом порожнім, повертаємо ліве піддерево.
delete(X, t(X, nil, Right), Right).  
% Якщо елемент X - це вузол з лівим піддеревом порожнім, повертаємо праве піддерево.
delete(X, t(X, Left, Right), t(Max, NewLeft, Right)) :-  
    find_max(Left, Max),  % Знаходимо максимальний елемент у лівому піддереві.
    delete(Max, Left, NewLeft).  
% Якщо елемент X - це вузол з двома піддеревами, заміщаємо його максимальним елементом з лівого піддерева.
delete(X, t(Root, Left, Right), t(Root, NewLeft, Right)) :- X =< Root, delete(X, Left, NewLeft).  
% Якщо X менше або рівне кореню, видаляємо його з лівого піддерева.
delete(X, t(Root, Left, Right), t(Root, Left, NewRight)) :- X > Root, delete(X, Right, NewRight).  
% Якщо X більше кореня, видаляємо його з правого піддерева.

% find_max(+Tree, -Max).
% Предикат для знаходження максимального елемента в дереві.
find_max(t(Root, _, nil), Root).  % Якщо праве піддерево порожнє, максимальний елемент - це корінь.
find_max(t(_, _, Right), Max) :-  
    find_max(Right, Max).  % Рекурсивно шукаємо максимальний елемент в правому піддереві.

% find_min(+Tree, -Min).
% предикат для знаходження мінімального елемента в дереві.
find_min(t(Root, nil, _), Root).  % Якщо ліве піддерево порожнє, мінімальний елемент - це корінь.
find_min(t(_, Left, _), Min) :-  
    find_min(Left, Min).  % Рекурсивно шукаємо мінімальний елемент в лівому піддереві.

% search(+Key, +Tree).
% Предикат для пошуку елемента в дереві.
search(X, t(X, _, _)).  
% Якщо елемент знайдений в корені, повертаємо true.
search(X, t(Root, Left, _)) :- X =< Root, search(X, Left).  
% Якщо X менше або рівне кореню, шукаємо в лівому піддереві.
search(X, t(_, _, Right)) :- search(X, Right).  
% Якщо X більше кореня, шукаємо в правому піддереві.

% inorder(+Tree, -List).
% Предикат для отримання елементів дерева в порядку зростання.
inorder(nil, []).  
% Порожнє дерево повертає порожній список.
inorder(t(Key, Left, Right), SortedList) :-  
    inorder(Left, LeftList),  % Отримуємо елементи лівого піддерева.
    inorder(Right, RightList),  % Отримуємо елементи правого піддерева.
    append(LeftList, [Key|RightList], SortedList).  
% Об'єднуємо лівий список, корінь та правий список для отримання відсортованого списку.

% bst_build(+Lst, -Tree).
% Предикат для побудови бінарного дерева пошуку з списку елементів.
bst_build(List, Tree) :-  
    bst_build(List, nil, Tree).  
bst_build([], Tree, Tree).  
% Якщо список порожній, дерево не змінюється.
bst_build([Key|Keys], AccTree, Tree) :-  
    insert(Key, AccTree, UpdatedTree),  % Вставляємо елемент в дерево.
    bst_build(Keys, UpdatedTree, Tree).  
% Рекурсивно вставляємо елементи зі списку в дерево.

% tree_to_list(+Tree, -List).
% Предикат для перетворення дерева в список.
tree_to_list(nil, []).  
% Порожнє дерево перетворюється на порожній список.
tree_to_list(t(Key, Left, Right), List) :-  
    tree_to_list(Left, LeftList),  % Отримуємо елементи лівого піддерева.
    tree_to_list(Right, RightList),  % Отримуємо елементи правого піддерева.
    append(LeftList, [Key|RightList], List).  
% Об'єднуємо лівий список, корінь і правий список, щоб отримати повний список елементів дерева.

% Перетворення впорядкованого списку у збалансоване бінарне дерево
list_to_tree([], nil).  % База рекурсії: порожній список відповідає порожньому дереву.
list_to_tree(List, Tree) :-
    length(List, Len),        % Знаходимо довжину списку.
    MidIndex is Len // 2,     % Обчислюємо індекс середнього елемента.
    nth0(MidIndex, List, Root, Rest),  % Витягуємо середній елемент як корінь і отримуємо решту списку без нього.
    split_list(Rest, Root, LeftList, RightList),  % Ділимо список на ліву і праву частини відносно кореня.
    list_to_tree(LeftList, LeftTree),   % Рекурсивно створюємо ліве піддерево.
    list_to_tree(RightList, RightTree), % Рекурсивно створюємо праве піддерево.
    Tree = t(Root, LeftTree, RightTree).  % Формуємо дерево з отриманих піддерев.


% Розділення списку відносно елемента
split_list([], _, [], []).  % База рекурсії: порожній список, обидва підсписки порожні.
split_list([X|Xs], Pivot, [X|Left], Right) :- 
    X < Pivot,                    % Якщо елемент менший за Pivot, додаємо його до лівого підсписку.
    split_list(Xs, Pivot, Left, Right).  % Рекурсивно обробляємо решту списку.
split_list([X|Xs], Pivot, Left, [X|Right]) :- 
    X > Pivot,                    % Якщо елемент більший за Pivot, додаємо його до правого підсписку.
    split_list(Xs, Pivot, Left, Right).  % Рекурсивно обробляємо решту списку.
split_list([X|Xs], X, Left, Right) :- 
    split_list(Xs, X, Left, Right).  % Якщо елемент дорівнює Pivot, пропускаємо його і обробляємо решту списку.

% Перетин двох списків (елементи, що є в обох)
list_intersection([], _, []).  % База рекурсії: якщо один зі списків порожній, перетину немає.
list_intersection([X|Xs], Ys, [X|Zs]) :- 
    member(X, Ys),                 % Якщо елемент X є в другому списку, додаємо його до результату.
    list_intersection(Xs, Ys, Zs).  % Рекурсивно обробляємо решту списку.
list_intersection([_|Xs], Ys, Zs) :- 
    list_intersection(Xs, Ys, Zs).  % Якщо елемент не належить другому списку, пропускаємо його.

% Основний предикат знаходження перетину двох дерев
intersection(nil, _, nil).  % Якщо одне з дерев порожнє, перетину немає.
intersection(_, nil, nil).  % Якщо друге дерево порожнє, перетину немає.
intersection(Tree1, Tree2, IntersectionTree) :-  
    tree_to_list(Tree1, List1),       % Перетворюємо перше дерево в список.
    tree_to_list(Tree2, List2),       % Перетворюємо друге дерево в список.
    list_intersection(List1, List2, IntersectionList),  % Знаходимо перетин двох списків.
    list_to_tree(IntersectionList, IntersectionTree).  % Перетворюємо перетин у дерево.

% Видалення елементів другого списку з першого
list_difference([], _, []).  % База рекурсії: якщо перший список порожній, результат також порожній.
list_difference([X|Xs], Ys, Zs) :- 
    member(X, Ys),                % Якщо елемент X є в другому списку, пропускаємо його.
    list_difference(Xs, Ys, Zs).   % Рекурсивно обробляємо решту списку.
list_difference([X|Xs], Ys, [X|Zs]) :- 
    \+ member(X, Ys),             % Якщо елемент X немає в другому списку, додаємо його до результату.
    list_difference(Xs, Ys, Zs).   % Рекурсивно обробляємо решту списку.

% Основний предикат знаходження різниці двох дерев
difference(nil, _, nil).  % Якщо перше дерево порожнє, різниці немає.
difference(Tree1, nil, Tree1).  % Якщо друге дерево порожнє, різниці немає — перше дерево не змінюється.
difference(Tree1, Tree2, DifferenceTree) :-  
    tree_to_list(Tree1, List1),       % Перетворюємо перше дерево в список.
    tree_to_list(Tree2, List2),       % Перетворюємо друге дерево в список.
    list_difference(List1, List2, DifferenceList),  % Знаходимо різницю між двома списками.
    list_to_tree(DifferenceList, DifferenceTree).  % Перетворюємо результат різниці назад у дерево.


% merge(+Tree1, +Tree2, -NewTree).
% Функція для об'єднання двох дерев.
merge(nil, Tree2, Tree2).  
% Якщо перше дерево порожнє, повертаємо друге дерево.
merge(Tree1, nil, Tree1).  
% Якщо друге дерево порожнє, повертаємо перше дерево.
merge(t(Key, Left1, Right1), Tree2, MergedTree) :-  
    insert(Key, Tree2, UpdatedTree2),  
    merge(Left1, UpdatedTree2, MergedLeft),  
    merge(Right1, MergedLeft, MergedTree).  
% Для кожного елемента з першого дерева вставляємо його у друге дерево, а потім об'єднуємо ліве та праве піддерева.

% height(+Tree, -Height).
height(nil, 0).  % Якщо дерево порожнє, висота 0.
height(t(_, Left, Right), Height) :-  
    height(Left, LeftHeight),  
    height(Right, RightHeight),  
    Height is max(LeftHeight, RightHeight) + 1.  
% Для кожного піддерева знаходимо висоту і беремо максимальну, додаючи 1 для кореня.

% mirror(+Tree, -MirrorTree).
mirror(nil, nil).  
% Порожнє дерево в дзеркальному вигляді теж порожнє.

mirror(t(Key, Left, Right), t(Key, MirrorRight, MirrorLeft)) :-  
    mirror(Left, MirrorLeft),  
    mirror(Right, MirrorRight).  
% Рекурсивно міняємо місцями ліве і праве піддерева.



/** <examples>
?- bst_build([10, 5, 15, 3, 7, 12, 18], Tree).
Tree = t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))).

?- insert(8, t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), NewTree).
NewTree = t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, t(8, nil, nil)))).

?- delete(5, t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), NewTree).
NewTree = t(10, t(7, t(3, nil, nil), nil), t(15, t(12, nil, nil), t(18, nil, nil))).

?- search(12, t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil)))).
true.

?- find_max(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), Max).
Max = 18.

?- find_min(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), Min).
Min = 3.

?- inorder(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), SortedList).
SortedList = [3, 5, 7, 10, 12, 15, 18].

?- tree_to_list(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), List).
List = [3, 5, 7, 10, 12, 15, 18].

?- intersection(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), t(15, t(12, nil, nil), t(7, nil, nil)), IntersectionTree).
IntersectionTree = t(12,t(7,nil,nil),t(15,nil,nil))

?- difference(t(10, t(5, t(3, nil, nil), t(7, nil, nil)), t(15, t(12, nil, nil), t(18, nil, nil))), t(15, t(12, nil, nil), t(18, nil, nil)), DifferenceTree).
DifferenceTree = t(7,t(5,t(3,nil,nil),nil),t(10,nil,nil))

?- merge(t(10, t(5, nil, nil), t(15, nil, nil)), t(7, t(3, nil, nil), nil), MergedTree).
MergedTree = t(7,t(3,nil,t(5,nil,nil)),t(10,nil,t(15,nil,nil)))

?- height(t(1, t(2, nil, nil), t(3, t(4, nil, nil), nil)), H).
H = 3.

?- mirror(t(1, t(2, nil, nil), t(3, t(4, nil, nil), nil)), M).
M = t(1, t(3, nil, t(4)), t(2, nil, nil)).


*/