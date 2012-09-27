{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read, error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)
neg Tri -> Tri
neg EQ = EQ
neg LT = GT
neg GT = LT

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero _ = LT
natCmp _ Zero = GT
natCmp (Succ x) (Succ y) = natCmp x y

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq x y = case natCmp x y of
	EQ -> True
	_ -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt x y = case natCmp x y of
	LT -> True
	_ -> False
	
natGt x y = case natCmp x y of
	GT -> True
	_ -> False
	
infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
x -. Zero = x
Zero -. _ = error "Value is not Natural." --Of course, Zero is not Natural too, but I'm doing as you say.
(Succ x) -. (Succ y) = (x -. y)

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "Division by zero."
natDivMod n m = if' (natGt n m) (Pair ((natDiv (n -. m) m) +. natOne) (natMod (n -. m) m)) (Pair Zero n) 

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd x y = if' (natEq x y) (x) (if' (natGt x y) (gcd (x -. y) y) (gcd (y -. x) x))

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Positive Nat | Negative Nat deriving (Show,Read)

intZero   = Positive natZero -- 0
intOne    = Positive natOne     -- 1
intNegOne = Negative natZero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Positive x) = Negative x
intNeg (Negative x) = Positive x

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Positive x) (Negative y) = GT
intCmp (Negative x) (Positive y) = LT
intCmp (Positive Zero) (Positive Zero) = EQ
intCmp (Positive Zero) (Positive _) = LT
intCmp (Positive _) (Positive Zero) = GT
intCmp (Positive Zero) (Negative _) = GT
intCmp (Negative _) (Positive Zero) = LT
intCmp (Positive (Succ x)) (Positive (Succ y)) = intCmp (Positive x) (Positive y)
intCmp (Negative (Succ x)) (Negative (Succ y)) = intCmp (Negative x) (Negative y)

intEq :: Int -> Int -> Bool
intEq x y = case (intCmp x y) of
	EQ -> True
	_ -> False

intLt :: Int -> Int -> Bool
intLt x y = case (intCmp x y) of
	LT -> True
	_ -> False
	
intGt :: Int -> Int -> Bool
intGt x y = case (intCmp x y) of
	GT -> True
	_ -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Positive x) .+. (Positive y) = Positive (x +. y)
(Negative x) .+. (Negative y) = Negative (Succ(Succ(x +. y)))
(Positive x) .+. (Negative y) = case (natCmp x (Succ y)) of
	EQ -> intZero
	GT -> Positive (x -. y)
	LT -> Negative (y -. x)
(Negative x) .+. (Positive y) = case (natCmp x (Succ y)) of
	EQ -> intNegOne
	LT -> Positive (x -. y)
	GT -> Negative (y -. x)

	

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. (Positive (Succ Zero)) = n
n .*. (Positive (Succ m)) = n .+. (n .*. (Positive m))
n .*. (Negative (Succ m)) = intNeg (n .+. (n .*. (Positive m)))

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Positive Zero) y) = error "Division by zero"
ratInv (Rat (Negative x) y) = Rat (Negative y) (x)
ratInv (Rat (Positive x) y) = Rat (Positive y) (x)

--Simplify the rational number
ratSim :: Rat -> Rat
ratSim (Rat (Positive x) y) = Rat (Positive (x `natDiv` (gcd x y))) (y `natDiv` (gcd x y))
ratSim (Rat (Negative x) y) = Rat (Negative (x `natDiv` (gcd x y))) (y `natDiv` (gcd x y))

--Check simple the number or not
ratCS :: Rat -> Bool
ratCS (Rat (Positive x) y) = if' (gcd x y `natEq` natOne) True False

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat (Positive x1) y1) (Rat (Positive x2) y2) = NatCmp (x1 *. y2) (x2 *. y1)
ratCmp (Rat (Negative x1) y1) (Rat (Negative x2) y2) = NatCmp (x1 *. y2) (x2 *. y1)
ratCmp (Rat (Positive _) _) (Rat (Negative _) _) = GT
ratCmp (Rat (Negative _) _) (Rat (Positive _) _) = LT


ratEq :: Rat -> Rat -> Bool
ratEq x y = case ratCmp x y of
	EQ -> True
	_ -> False

ratLt :: Rat -> Rat -> Bool
ratLt x y = case ratCmp x y of
	LT -> True
	_ -> False
	
infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x1 y1) %+ (Rat x2 y2) = natSim (Rat ((x1 .*. (Positive y2)) .+. (x2 .*. (Negative y1))) (y1 *. y2))

(%-) :: Rat -> Rat -> Rat
(Rat x1 y1) %+ (Rat x2 y2) = natSim (Rat ((x1 .*. (Positive y2)) .-. (x2 .*. (Negative y1))) (y1 *. y2))

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x1 y1) %* (Rat x2 y2) = Rat (x1 .*. x2) (y1 *. y2)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
