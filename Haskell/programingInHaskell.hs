-- プログラミングHaskell (Graham Hutton (著), 山本 和彦 (翻訳)) の練習問題 

-- =========== 5 ============

-- 5-1
sum [x^2 | x<-[1..100]]

-- 5-2
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- 5-3
-- n以下の要素のピタゴラス数のリストを返す
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 5-4
-- 与えられた上限までに含まれる完全数全てを算出する関数perfects
factors :: Int -> [Int]
factors n = [x | x <- [ 1 .. n], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], x == sum(init (factors x))]

-- 5-6
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [ v | (k', v) <- t, k == k']
-- > find 'b' [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
-- [2, 4]
-- > zip ['a', 'b', 'c'] [1, 2, 3, 4]
-- [('a',1),('b',2),('c',3)]
positions :: Eq a => a -> [a] -> [Int]
--positions x xs = [i | (x', i) <- zip xs [0..], x == x']
positions x xs = find x (zip xs [0..])
-- ghci > positions False [True, False, True, False]
-- [1, 3]

-- =========== 4.1 ====================
halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
            where n = length xs `div` 2
-- 実行例
--halve [1, 2, 3, 4, 5, 6]
--([1, 2, 3], [4, 5, 6])
 
-- drop n xs ... リストからn個の要素を取り除く
-- splitAt n xs = (take n xs, drop n xs) ... リストをn番目の要素のところで分割する


-- =========== 4.2 ====================
--safetail :: [a] -> [a]
-- tail(cdr)のように振る舞うが、違いは、空リストを与えた時に空リストを返すようにすること。
-- 実装方法は、それぞれ、a. 条件式、 b. ガード付きの等式、 c. パターンマッチ  でやること。

-- a. 条件式
safetail_A :: [a] -> [a]
safetail_A xs = if null xs then [] else tail xs

-- b. ガード付きの等式
safetail_B :: [a] -> [a]
safetail_B xs | null xs = []
              | otherwise = tail xs

-- c. パターンマッチ
safetail_C :: [a] -> [a]
safetail_C [] = []
safetail_C xs = tail xs

-- 論理和演算子 \/ .. 1つでもTrueがあったらTrue
-- これを4通りの方法で定義する。

-- a
(\/) :: Bool -> Bool -> Bool
True \/ True = True
True \/ False = True
False \/ True = True
False \/ False = False

-- b
(\./) :: Bool -> Bool -> Bool
True \./ _ = True
_ \./ y = y


(/\) :: Bool -> Bool -> Bool

--  True /\ True = True
--  _    /\ _    = False

x /\ y = if x == True 
        then
            if y == True then True else False
        else False

-- ========== 4.6 ===========
-- double x = x + x
double = \x -> x + x
 
--add x y = x + y
add = \x -> (\y -> x + y)
 
-- カリー化された関数 mult :: Num a => a -> a -> a -> a
-- mult x y z = x * y * z は、λ式を用いるとどのように表現できるか
mult :: Int -> ( Int -> ( Int -> Int ))
mult = \x -> (\y -> (\z -> x * y * z))


