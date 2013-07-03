import Data.List
import Data.Char
-- 問題はこちらにあります https://github.com/tokiwoousaka/YuruhuwaHaskell/blob/master/exercises/%E6%BC%94%E7%BF%92%E5%95%8F%E9%A1%8C_%E3%82%86%E3%82%8B%E3%81%B5%E3%82%8F%E7%B7%A8.md
-- 練習問題1はとばします(ちゃんとやりました!)

-- ======== 2.1 ＊で山を作ってみよう ============

---- 入力された単語を指定回数分繰り返す関数
repeatStr :: String -> Int -> String
repeatStr str n = take (n * length str) $ cycle str

repeatStr' :: String -> Int -> String
repeatStr' str = concat . flip replicate str 
---- 例：
--Prelude> repeatStr "abc" 3 
--"abcabcabc"

-- 演習1
-- ＊を指定回数分繰り返す関数を定義しましょう。
stars :: Int -> String
stars = repeatStr "*"

--演習2
--以下の三角形を画面上に表示させてください。

-- *
-- **
-- ***
-- ****
-- *****

-- 1. ["*", "**", "***",..., "*****"]をつくる
-- 2. 間に\nいれて連結
mountain :: Int -> String
mountain n = intercalate "\n" $ map stars [1..n]

-- 3. 出力
printMountain :: Int -> IO ()
printMountain = putStrLn . mountain

-- 演習3
-- 以下の様な山を画面上に表示させてください。

-- *
-- **
-- ***
-- ****
-- *****
-- *****
-- ****
-- ***
-- **
-- *

reverseMountain :: Int -> IO()
reverseMountain n = putStrLn $ mountain n ++ "\n" ++ ( reverse $ mountain n )

-- 演習4
-- 頂上が尖っている山を作って下さい
reverseMountain' n = putStrLn $ mountain n ++ "\n" ++ ( reverse $ mountain (n-1))

-- ======== 2.2 FizzBuzz ============
--１から順にインクリメントして、
--３で割りきれる時Fizz、
--５で割りきれる時Buzz、
--３と５で割りきれる時はFizzBuzz

fizzBuzz :: Int -> IO()
fizzBuzz n = putStrLn $ intercalate " " $ map fizzBuzzp [1..n]
    where
        fizzBuzzp :: Int -> String
        fizzBuzzp n
          | n `mod` 15 == 0 = "FizzBuzz"
          | n `mod` 5 == 0 = "Buzz"
          | n `mod` 3 == 0 = "Fizz"
          | otherwise = show n
          
-- ========= 3.1 CapsLock関数 =================

-- 実装せよ:capsLock関数
-- 入力: 文字列(半角英語)
-- 出力: 入力された文字列すべてを大文字にして出力
main :: IO()
main = getLine >>= putStr . map toUpper
