import Data.List

suffixes = ["" ,"thousand", "million", "billion", "trillion"]

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

(+++) :: String -> String -> String
"" +++ "" = ""
a +++ "" = a
"" +++ a = a
a +++ b = a ++ " " ++ b

digitGroups ::Int -> [(Int, Int, Int)]
digitGroups n = digitGroups' $ reverse $ toDigits $ n

digitGroups' :: [Int] -> [(Int, Int, Int)]
digitGroups' (a:b:c:ds) = (c,b,a):(digitGroups' ds)
digitGroups' (a:b:[]) = (0,b,a):[]
digitGroups' (a:[]) = (0,0,a):[]
digitGroups' [] = []

groupToWords :: (Int, Int, Int) -> String -> String
groupToWords digits suffix = (toWords3 digits) +++ suffix

toWords :: Int -> String
toWords 0 = "zero"
toWords n = intercalate " " (filter (\a -> a /= "") (reverse (zipWith groupToWords  (digitGroups n) suffixes)))

toWords3 :: (Int, Int, Int) -> String
toWords3 (0,a,b) = toWords2 (a,b)
toWords3 (a,b,c) = (toWords1 a) ++ " hundred " ++ (toWords2 (b,c))
--toWords3 (a,b,c) = (toWords1 a) +++ "hundred" +++ (toWords2 (b,c))

toWords2 :: (Int, Int) -> String
toWords2 (0,a) = toWords1 a
toWords2 (1,0) = "ten"
toWords2 (1,1) = "eleven"
toWords2 (1,2) = "twelve"
toWords2 (1,3) = "thirteen"
toWords2 (1,5) = "fifteen"
toWords2 (1,8) = "eighteen"
toWords2 (1,a) = (toWords1 a) ++ "teen"
toWords2 (2,a) = "twenty" +++ (toWords1 a) 
toWords2 (3,a) = "thirty" +++ (toWords1 a) 
toWords2 (4,a) = "fourty" +++ (toWords1 a) 
toWords2 (5,a) = "fifty" +++ (toWords1 a) 
toWords2 (6,a) = "sixty" +++ (toWords1 a) 
toWords2 (7,a) = "seventy" +++ (toWords1 a) 
toWords2 (8,a) = "eighty" +++ (toWords1 a) 
toWords2 (9,a) = "ninety" +++ (toWords1 a) 

toWords1 :: Int -> String
toWords1 0 = ""
toWords1 1 = "one"
toWords1 2 = "two"
toWords1 3 = "three"
toWords1 4 = "four"
toWords1 5 = "five"
toWords1 6 = "six"
toWords1 7 = "seven"
toWords1 8 = "eight"
toWords1 9 = "nine"