-- Імпортуємо необхідні модулі
import Data.List (nub) -- nub видаляє повторювані елементи у списку
import Control.Monad (guard) -- guard допомагає в спискових обчисленнях

-- Можливі зміни цифр при додаванні або видаленні сірників
-- Наприклад, '0' можна перетворити на '8', а '1' на '7'
matchstickChanges :: [(Char, [Char])]
matchstickChanges =
  [ ('0', "8"),  -- 0 може бути змінене на 8
    ('1', "7"),  -- 1 може бути змінене на 7
    ('3', "8"),  -- 3 може бути змінене на 8
    ('5', "6 8"),-- 5 може бути змінене на 6 або 8
    ('6', "8"),  -- 6 може бути змінене на 8
    ('9', "8"),  -- 9 може бути змінене на 8
    ('4', "9"),  -- 4 може бути змінене на 9
    ('7', "3"),  -- 7 може бути змінене на 3
    ('2', "8")   -- 2 може бути змінене на 8
  ]

-- Функція, що генерує всі можливі варіанти зміни виразу, змінюючи один символ
modifyExpression :: String -> [String]
modifyExpression expr = do
  (i, c) <- zip [0..] expr -- Поєднуємо індекси та символи у виразі
  let replacements = maybe [] id (lookup c matchstickChanges) -- Шукаємо можливі заміни для символу
  replacement <- replacements -- Беремо кожен можливий варіант заміни
  let (before, _:after) = splitAt i expr -- Розбиваємо рядок перед та після символу, що змінюється
  return (before ++ [replacement] ++ after) -- Повертаємо новий вираз із заміною

-- Генерує всі варіанти зміни двох символів у виразі
modifyExpressionTwice :: String -> [String]
modifyExpressionTwice expr = nub $ do
  firstChange <- modifyExpression expr -- Спочатку змінюємо один символ
  modifyExpression firstChange -- Потім ще раз застосовуємо зміну до отриманого варіанту

-- Функція перевіряє, чи є вираз правильним рівнянням
isValidExpression :: String -> Bool
isValidExpression expr = case break (== '=') expr of
  (left, '=':right) -> case break (== '+') left of
    (a, '+':b) -> case (reads a, reads b, reads right) of
      ([(x, "")], [(y, "")], [(z, "")]) -> x + y == z -- Перевіряємо, чи арифметична рівність правильна
      _ -> False -- Якщо не вдалося розпарсити числа, вираз неправильний
    _ -> False 
  _ -> False -- Якщо немає знаку '=', вираз некоректний

-- Пошук правильного рівняння шляхом зміни 1 або 2 символів
solvePuzzle :: String -> [String]
solvePuzzle expr =
  filter isValidExpression (modifyExpression expr ++ modifyExpressionTwice expr) -- Генеруємо всі варіанти та перевіряємо їх

-- Головна функція для взаємодії з користувачем
main :: IO ()
main = do
  putStrLn "Введіть арифметичний вираз (наприклад, 3+3=5):"
  input <- getLine -- Отримуємо введення користувача
  let solutions = solvePuzzle input -- Знаходимо всі можливі виправлення
  if null solutions
    then putStrLn "Рішення не знайдено" -- Якщо рішень немає, виводимо повідомлення
    else putStrLn $ "Можливі виправлення: " ++ unwords solutions -- Виводимо знайдені коригування