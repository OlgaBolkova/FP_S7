-- FP_S7
-- Боровикова Ольга, №5381
import Data.Char
import Data.List

-- 1 -------------------------------

id_ :: a->a
id_ x = x

eval :: (a -> b, a) -> b
eval (f, x) = f x
-- где f:: a->b

exchange :: (a, b) -> (b, a)
exchange (x, y) = (y, x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose x y z = x $ y z
-- x:: b->c 
-- y:: a->b
-- z:: a

curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f x y = f (x, y)

associate :: (a, (b, c)) -> ((a, b), c)
associate (x, (y, z)) = ((x, y), z)

-- 2 -------------------------------
minMax :: Ord a => [a] -> Maybe (a, a) -- (min, max)
-- функция для min
min_ :: (Ord a) => a->a->a
min_ x y = if x<= y then x else y
-- функция для max
max_ :: (Ord a) => a->a->a
max_ x y = if x>=y then x else y

min_max :: (Ord a) => (a->a->a)->[a] -> a
min_max func [x] = x
min_max func (x:xs) = func x (min_max func xs)

-- Определние для общей функции minMax
minMax [] = Nothing
minMax (x:xs) = Just(min_max min_ (x:xs), min_max max_ (x:xs))

-- 3 -------------------------------
-- Реализуйте функции, находящие сумму и количество цифр(в 10-ой системе счисления)
-- заданного целого числа

-- Основная функция
sum_and_count :: Integer -> (Integer, Integer)
sum_and_count x | x==0 = (0,1)
                        | otherwise = (sum n, toInteger (length n))
                                   where n = list (x, 0) 0
-- функция разбора числа для 10-ой системы счисления
list :: (Integer, Integer) -> Integer -> [Integer]
list (0, y) k = [y]
list (x, y) k | x<0 = list (divMod (-x) 10) (k+1)
       | otherwise = if k==0 then (list (divMod x 10) (k+1)) else (list (divMod x 10) k ) ++ [y]

-- 4 --------------------------------


-- Реализуйте функцию поиска преобладающего элемента списка. Элемент списка
-- длины n называется преобладающим, если он встречается в нем более чем n/2 раз.
-- Найдите решение с линейным временем работы — O(n).
-- функция - алгоритм
funct :: (Eq a) => a -> Integer -> [a] -> a
-- funct :: Char -> Integer -> [Char] -> Char
funct s c (x:xs) | c==0 = funct x (c+1) xs
                 | otherwise = if s==x then funct s (c+1) xs else funct s (c-1) xs
funct s c [] = s
-- функции для проверки результата - поиск утечки
coun_t :: (Eq a) => [a]->Int
-- изменила верхнюю функцию
coun_t xs = length $ filter (==(funct (head xs) 0 xs)) xs
-- основная функция 
sho_w :: (Eq a) => [a]-> Maybe a
sho_w xs = if ((coun_t xs)>((length xs) `div` 2)) then Just(funct (head xs) 0 xs) else Nothing
-- в результате по списку проходмся мах 3 раза (3n). Сам алгоритм требует одно прохождение
-- по списку, но так как существуют проверки число прохождений увеличивается до 3 раз

-- 5 ---------------------------------
f:: (a -> a) -> Int -> (a -> a)
f g n | n==0 = g
      | n < 0 = error "***Exception: n must be positive number"
      | otherwise = g . (f g (n-1))
-- где . - оператор композиции

-- 6 ----------------------------------
-- Релизуйте функцию, вычисляющую последнюю цифру (в 10-ой системе счисления)
-- n-го члена последовательности Фибоначчи
-- Основная функция 
last_fibb :: Integer -> Integer
last_fibb y = last(list (z,0) 0)
                 where z = fibb (0,0,1) y
-- функция list описана выше 
-- функция нахождения n-го члена последовательности Фибоначчи
fibb :: (Integer,Integer,Integer) -> Integer -> Integer
fibb (curr, x, y) n = if (curr<n) then fibb (curr+1, y, y+x) n else y
-- здесь каждый член последовательности вычисляется один раз, что улучшает скорость работы до линейной O(n)
-- нумерация чисел последовательности начинается с "0" - 0, 1, 2, ...

-- 7 -----------------------------------
-- Реализуйте рекурсивную функцию, которая определяет является ли палиндромом
-- переданная строка. Регистр при сравнении символов не учитывается.

-- функция, в которой рекурсия используется в подфункции
-- isPalindrome :: String -> Bool
-- IsPalindrome xs = if str == reverse str then True else False
--                      where str = string_l xs
-- string_l :: [Char]->[Char]
-- string_l [] = []
-- string_l (x:xs) = toLower x : (string_l xs)

-- функция, рекурсия в главной функции
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (x:xs) | xs==[] = True
                    | otherwise = if toLower x == toLower (last xs) then isPalindrome (take((length xs)-1) xs) else False
--каждый раз уменьшаем хвост строки на эл-т с конца c помощью функции take
