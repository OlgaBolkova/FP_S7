-- FP_S7
-- Боровикова Ольга, №5381

import Data.Char
import Data.List
import Data.Foldable
import Data.Ratio
import Data.Monoid 

-- 1 ------------------------------
circShiftL :: Int -> [a] -> [a]
circShiftL n [] = []
circShiftL 0 x = x
circShiftL n xs | n<0 = circShiftL (-n) xs
                | otherwise = circShiftL (n-1) ((last xs) : init xs)

-- 2 ------------------------------
indices :: [a] -> [(Integer, a)]
indices  xs = zip [0 ..] xs

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy xs p = map (\x -> if p x then x else mempty) xs

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer] 
triplewiseSum xs ys zs = zipWith (+) xs (zipWith (+) ys zs)

-- 3 ------------------------------- 
--'\NUL' - is minBound::Char
revRange :: (Char, Char) -> [Char] 
revRange = unfoldr fun 
fun (a, b) | b < a = Nothing
--           | b == '\NUL' = Just (b, (succ a, b)) -- в Just первый аргумент тот символ, которой пишется в список,
           | b == minBound = Just (b, (succ a, b)) -- в Just первый аргумент тот символ, которой пишется в список,
-- второй посылается на обработку функции
           | otherwise = Just (b, (a, pred b))
 
-- 4 -------------------------------
seriesK :: Int -> [Rational]
seriesK a = (1%1) : (f a 1)
        where f x n = y : ys 
                 where y = (1% toInteger(x^n))
                       ys= f x (n+1)

-- 5 ---------------------------------
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

instance Ord a => Semigroup (SortedList a) where 
    (SortedList xs) <> (SortedList ys) = SortedList (merge xs ys)
-- функция для слияния списков, слияние отсортированных несписков
        where   merge [] ys = ys
                merge xs [] = xs
                merge (x:xs) (y:ys) = if x > y 
                                        then y : merge (x:xs) ys
                                        else x : merge xs (y:ys)

instance Ord a => Monoid (SortedList a) where 
    mempty = SortedList []

-- 6 ---------------------------------
msort :: Ord a => [a] -> SortedList a
msort [] = mempty
-- массив единичного размера является отсортированным
msort [x] = SortedList [x] 
-- далее делим массив на примерно равные части
-- потом для каждой полученной пары применяем операцию слияния сортированных списков
msort xs = mappend (msort top) (msort bottom) where 
    (top, bottom) = splitAt ((length xs) `div` 2) xs 

-- 7 --------------------------------- 
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)
newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

-- Описание функций 
-- обход LNR 

instance Foldable Tree where
        foldMap f Nil = mempty  -- Если дерево пустое возвращаем mempty
-- Обходим рекурсивно левое поддерево, показываем поле данных текущего узла,
-- обходим рекурсивно правое поддерево
        foldMap f (Node left root right) = (foldMap f left) <> f root <> (foldMap f right)

-- pre обход (NLR)

instance Foldable Preorder where
        foldMap f (PreO Nil) = mempty -- Если дерево пустое возвращаем mempty
-- Показываем поле данных текущего узла, обходим рекурсивно левое дерево, обходим рекурсивно правое дерево
        foldMap f (PreO(Node left root right)) = f root <> (foldMap f (PreO left)) <> (foldMap f (PreO right))

-- post обход LRN

instance Foldable Postorder where
        foldMap f (PostO Nil) = mempty -- Если дерево пустое возвращаем mempty
-- Обходим рекурсивно левое поддерево, обходим рекурсивно правое поддерево, показываем поле данных текущего узла
        foldMap f (PostO (Node left root right)) = (foldMap f (PostO left)) <> (foldMap f (PostO right)) <> f root

--level-order (обход в ширину)
instance Foldable Levelorder where
         foldMap f tree = fold_map f (bfs [tree]) where
                fold_map f [] = mempty
                fold_map f (x:xs) = f x <> fold_map f xs
               -- поменяем порядок обработки
                -- fold_map f (x:xs) = fold_map f xs <> f x 

                bfs [] = []
                bfs xs = map idNode xs ++ bfs(concat(map lrn xs)) where
                        idNode (LevelO (Node _ a _)) = a
                        lrn (LevelO (Node Nil _ Nil)) = []
                        lrn (LevelO (Node Nil _ r)) = [LevelO r]
                        lrn (LevelO (Node l _ Nil)) = [LevelO l]
                      --  lrn (LevelO (Node l _ r)) = [LevelO l, LevelO r]
                        lrn (LevelO (Node l _ r)) = [LevelO r, LevelO l]

-- пример дерева и его обходы
-- foldMap id (PostO tree) -- вызов функции
--tree = Node(Node(Node Nil [4] Nil) [2] (Node Nil [5] Nil)) [1] (Node Nil [3] (Node Nil [6] Nil))
{--
LNR -> [4,2,5,1,3,6]
NLR -> [1,2,4,5,3,6]
LRN -> [4,5,2,6,3,1]
LevelO -> [1,2,3,4,5,6]
--}
