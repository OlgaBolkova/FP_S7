import Data.Char
import Data.List

-- 1 -------------------------------

id_ :: a->a
id_ x = x

eval :: (a -> b, a) -> b
eval (f, x) = f x
-- ��� f:: a->b

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
-- ������� ��� min
min_ :: (Ord a) => a->a->a
min_ x y = if x<= y then x else y
minimum_ :: (Ord a) => [a] -> a
minimum_ [x] = x
minimum_ (x:xs) = min_ x (minimum_ xs)
-- ������� ��� max
max_ :: (Ord a) => a->a->a
max_ x y = if x>=y then x else y
maximum_ :: (Ord a) => [a] -> a
maximum_ [x] = x
maximum_ (x:xs) = max_ x (maximum_ xs)
-- ���������� ��� ����� ������� minMax
minMax [] = Nothing
minMax (x:xs) = Just(minimum_ (x:xs), maximum_ (x:xs))

-- 3 -------------------------------
-- ���������� �������, ��������� ����� � ���������� ����(� 10-�� ������� ���������)
-- ��������� ������ �����

-- �������� �������
sum_and_count :: Integer -> (Integer, Integer)
sum_and_count x | x==0 = (0,1)
                        | otherwise = (sum n, toInteger (length n))
                                   where n = list x
-- ������� ������� ����� ��� 10-�� ������� ���������
list :: Integer -> [Integer]
list 0 = []
list x | x<0 = list (-x)
       | otherwise = list (x `div` 10) ++ [x `mod` 10]

-- 4 --------------------------------


-- ���������� ������� ������ �������������� �������� ������. ������� ������
-- ����� n ���������� �������������, ���� �� ����������� � ��� ����� ��� n/2 ���.
-- ������� ������� � �������� �������� ������ � O(n).
-- ������� - ��������
funct :: Char -> Integer -> [Char] -> Char
funct s c (x:xs) | c==0 = funct x (c+1) xs
                 | otherwise = if s==x then funct s (c+1) xs else funct s (c-1) xs
funct s c [] = s
-- ������� ��� �������� ���������� - ����� ������
coun_t :: [Char]->Int
coun_t xs = length $ filter (==(funct ' ' 0 xs)) xs
-- �������� ������� 
sho_w :: [Char]->[Char]
sho_w xs = if ((coun_t xs)>((length xs) `div` 2)) then [funct ' ' 0 xs] else "no simbols"
-- � ���������� �� ������ ��������� ��� 3 ���� (3n). ��� �������� ������� ���� ����������� -- �� ������, �� ��� ��� ���������� �������� ����� ����������� ������������� �� 3 ���

-- 5 ---------------------------------
f:: (a -> a) -> Int -> (a -> a)
f g n | n==0 = g
      | n < 0 = error "***Exception: n must be positive number"
      | otherwise = g . (f g (n-1))
-- ��� . - �������� ����������

-- 6 ----------------------------------
-- ��������� �������, ����������� ��������� ����� (� 10-�� ������� ���������)
-- n-�� ����� ������������������ ���������
-- �������� ������� 
last_fibb :: Integer -> Integer
last_fibb y = last(list z)
                 where z = fibb y
-- ������� list ������� ���� 
-- ������� ���������� n-�� ����� ������������������ ���������
fibb :: Integer -> Integer
fibb x = if (x==0 || x==1) then x else  fibb(x-1) + fibb(x-2)

-- 7 -----------------------------------
-- ���������� ����������� �������, ������� ���������� �������� �� �����������
-- ���������� ������. ������� ��� ��������� �������� �� �����������.

-- �������, � ������� �������� ������������ � ����������
-- isPalindrome :: String -> Bool
-- IsPalindrome xs = if str == reverse str then True else False
--                      where str = string_l xs
-- string_l :: [Char]->[Char]
-- string_l [] = []
-- string_l (x:xs) = toLower x : (string_l xs)

-- �������, �������� � ������� �������
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (x:xs) | xs==[] = True
                    | otherwise = if toLower x == toLower (last xs) then isPalindrome (take((length xs)-1) xs) else False
--������ ��� ��������� ����� ������ �� ��-� � ����� c ������� ������� take
