{-# LtNGUtGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List t = Nil |  Cons t (List t) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List t -> Nat
length Nil = natZero
length (Cons x (a))= Succ (length a)

-- Склеить два списка за O(length a)
(++) :: List t -> List t -> List t
Nil ++ b = b
a ++ Nil = a
a ++ (Cons x (b)) = Cons x (a) ++ b

-- Список без первого элемента
tail :: List t -> List t
tail Nil = error "!!: List is empty"
tail (Cons x Nil) = Nil
tail (Cons x a) = a

-- Список без последнего элемента
init :: List t -> List t
init Nil = error "!!: List is empty"
init (Cons _ (Nil)) = Nil
init (Cons x a) = Cons x (init a)


-- Первый элемент
head :: List t -> t
head Nil = error "!!: List is empty"
head (Cons x a) = x

-- Последний элемент
last :: List a -> a
last (Cons x Nil) = x
last (Cons x a) = last a 

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero l = Nil
take _ Nil = error "!!: List has no more elements"
take (Succ n) (Cons y (a)) = Cons y (take n a)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero l = l
drop _ Nil = error "!!: List has no more elements"
drop (Succ n) (Cons y (x)) = drop (n) (x)

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter f Nil = Nil
filter f (Cons x a) = if' (f x) (Cons x (filter f a)) (filter f a)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f Nil = Nil
gfilter f (Cons x a) = case (f x) of
	Just b -> (Cons b) (gfilter f a)
	Nothing -> gfilter f a


-- Копировать из списка в результат до первого нарушения предиката\
--takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile f Nil = Nil
takeWhile f (Cons x a) = if' (f x) ((Cons x) (takeWhile f a)) (Nil) 

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile f Nil = Nil
dropWhile f (Cons x a) = if' (f x) (Cons x a) (dropWhile f a)




-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span f Nil = Pair Nil Nil
span f (Cons x a) = if' (f x) (Pair (Cons x (fst (span f a))) (snd (span f a))) (Pair (Nil)(Cons x (snd (span f a))))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break f a = span (fNot f) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
Cons a l !! n = l !! (n -. natOne)

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse l = Cons (last l) (init l)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = undefined --(Cons (Cons x Nil) (Cons (init (Cons x xs)) (Cons xs (subsequences xs))) --неправильно, переделать

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat = (Cons a (repeat a))

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z _ = z
foldl f z (Cons x l) = foldl f (f z x) l

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scan _ z _ = Cons z Nil
scanl f z (Cons x l) = Cons (f z x) (foldl f (f z x) l)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z (Cons x l) = f x (foldr f z l)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr = Cons (f z x) $ f x (foldr f z l)

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f (Cons x l) = Cons (f x) $ map f l

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = 

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap = undefined

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a b = undefined

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = undefined
