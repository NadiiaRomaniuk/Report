{-# OPTIONS_GHC -Wall #-}  
module Main where 

-- Описуємо структуру бінарного дерева
data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)  -- Вузол містить значення та два піддерева
                   deriving (Show, Eq)  -- Додаємо інстанції для Show (для виведення) та Eq (для порівняння на рівність)

-- Функція пошуку: перевіряє, чи існує значення в бінарному дереві
search :: (Ord a) => BinTree a -> a -> Bool
search EmptyB _ = False  -- Якщо дерево порожнє, значення не знайдено
search (Node v tl tr) n = n == v ||  -- Якщо значення відповідає вузлу, повертаємо True
          if n < v then search tl n  -- Якщо значення менше, шукаємо в лівому піддереві
                   else search tr n  -- Якщо значення більше, шукаємо в правому піддереві

-- Функція вставки: вставляє значення в бінарне дерево
insert :: (Ord a) => BinTree a -> a -> BinTree a 
insert EmptyB n = Node n EmptyB EmptyB  -- Якщо дерево порожнє, створюємо новий вузол з цим значенням
insert (Node v tl tr) n | v == n = Node v tl tr  -- Якщо значення вже є, нічого не змінюємо
                        | n < v = Node v (insert tl n) tr  -- Вставляємо в ліве піддерево
                        | n > v = Node v tl (insert tr n)  -- Вставляємо в праве піддерево

-- Функція видалення: видаляє значення з бінарного дерева
delete :: (Ord a) => BinTree a -> a -> BinTree a 
delete EmptyB _ = EmptyB  -- Якщо дерево порожнє, повертаємо порожнє дерево
delete (Node v tl tr) n | n < v = Node v (delete tl n) tr  -- Якщо значення менше, видаляємо в лівому піддереві
                           | n > v = Node v tl (delete tr n)  -- Якщо значення більше, видаляємо в правому піддереві
                           | tr == EmptyB = tl  -- Якщо праве піддерево порожнє, повертаємо ліве піддерево
                           | tl == EmptyB = tr  -- Якщо ліве піддерево порожнє, повертаємо праве піддерево
                           | otherwise = Node x tl (delete tr x)  -- Якщо обидва піддерева непорожні, знаходимо мінімум у правому піддереві та заміняємо
                             where x = findMin tr  -- Знаходимо мінімальне значення в правому піддереві

-- Допоміжна функція для знаходження мінімального значення в дереві (найлівіший вузол)
findMin :: (Ord a) => BinTree a -> a
findMin (Node v EmptyB _) = v  -- Найлівіший вузол містить мінімальне значення
findMin (Node _ tl _) = findMin tl  -- Рекурсивно шукаємо в лівому піддереві

-- Допоміжна функція для знаходження максимального значення в дереві (найправіший вузол)
findMax :: (Ord a) => BinTree a -> a
findMax (Node v EmptyB _) = v  -- Найправіший вузол містить максимальне значення
findMax (Node _ _ tr) = findMax tr  -- Рекурсивно шукаємо в правому піддереві

-- Функція для обхід дерева в порядку зростання: повертає список значень у впорядкованому вигляді
inorder :: (Ord a) => BinTree a -> [a]
inorder EmptyB = []  -- Якщо дерево порожнє, повертаємо порожній список
inorder (Node v tl tr) = inorder tl ++ [v] ++ inorder tr  -- Обходимо ліве піддерево, відвідуємо вузол, обходимо праве піддерево

-- Функція для побудови бінарного дерева з списку значень
buildTree :: (Ord a) => [a] -> BinTree a 
buildTree = foldl insert EmptyB  -- Використовуємо foldl для вставки кожного елементу в початково порожнє дерево

-- Функція для об'єднання двох бінарних дерев: поєднуємо всі елементи з обох дерев
merge :: (Ord a) => BinTree a -> BinTree a -> BinTree a
merge EmptyB tree = tree  -- Якщо одне дерево порожнє, повертаємо інше дерево
merge tree EmptyB = tree  -- Якщо інше дерево порожнє, повертаємо перше дерево
merge tree1 tree2 = foldr (\x acc -> insert acc x) tree1 (inorder tree2)  -- Проходимо деревом tree2 в порядку зростання і вставляємо елементи в дерево tree1

-- Функція перетину: повертає нове дерево з елементами, які є спільними в обох деревах
intersection :: (Ord a) => BinTree a -> BinTree a -> BinTree a
intersection EmptyB _ = EmptyB  -- Якщо одне дерево порожнє, перетину немає
intersection _ EmptyB = EmptyB  -- Якщо інше дерево порожнє, перетину немає
intersection tree1 tree2 = foldr (\x acc -> if search tree2 x then insert acc x else acc) EmptyB (inorder tree1)  -- Проходимо деревом tree1 в порядку зростання і вставляємо елементи, які є в обох деревах

-- Функція різниці: повертає нове дерево з елементами, які є тільки в першому дереві
difference :: (Ord a) => BinTree a -> BinTree a -> BinTree a
difference EmptyB _ = EmptyB  -- Якщо перше дерево порожнє, різниці немає
difference tree EmptyB = tree  -- Якщо друге дерево порожнє, повертаємо перше дерево
difference tree1 tree2 = foldr (\x acc -> if not (search tree2 x) then insert acc x else acc) EmptyB (inorder tree1)  -- Проходимо деревом tree1 в порядку зростання і вставляємо елементи, яких немає в tree2

-- Головна функція для демонстрації операцій
main :: IO ()
main = do
    let tree = insert (insert (insert EmptyB 10) 20) 15  -- Будуємо дерево з елементами 10, 20 та 15
    print tree  -- Виводимо дерево після вставок
    print $ search tree 15  -- Шукаємо значення 15 (поверне True)
    print $ search tree 5   -- Шукаємо значення 5 (поверне False)
    let tree' = delete tree 15  -- Видаляємо значення 15 з дерева
    print tree'  -- Виводимо дерево після видалення
    let tree'' = merge tree tree'  -- Об'єднуємо початкове дерево з модифікованим деревом
    print tree''  -- Виводимо об'єднане дерево
    print $ inorder tree''  -- Виводимо елементи об'єднаного дерева у впорядкованому вигляді
    let tree3 = insert (insert (insert EmptyB 10) 5) 25  -- Дерево з елементами 10, 5 та 25
    let tree4 = insert (insert (insert EmptyB 15) 25) 30  -- Дерево з елементами 15, 25 та 30

    print $ intersection tree3 tree4  -- Перетин між деревами tree3 та tree4 (поверне дерево з елементом 25)
    print $ difference tree3 tree4  -- Різниця між деревами tree3 та tree4 (поверне дерево з елементами 5 та 10)
