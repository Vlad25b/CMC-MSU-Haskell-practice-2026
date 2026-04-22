{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( cleanWord
    , wordStem
    , findDuplicates
    , countWordFrequencies
    , parseDate
    , calculateMean
    , clampToBounds
    , sortDescendingBySecond
    , currentYear
    ) where

import Data.List (sortBy, isSuffixOf)
import Data.Char (isPunctuation)
import Data.Function (on)
import qualified Data.Map as Map
import Types (Date(..))

--------------------------------------------------
-- константы
--------------------------------------------------

minYear, maxYear, minMonth, maxMonth, minDay, maxDay, currentYear :: Int
minYear = 1900
maxYear = 2100
minMonth = 1
maxMonth = 12
minDay = 1
maxDay = 31
currentYear = 2026

--------------------------------------------------
-- функции
--------------------------------------------------

cleanWord :: String -> String
cleanWord = filter (not . isPunctuation)

wordStem :: String -> String
wordStem = stemWord . cleanWord
  where
    stemWord w
        | "ться" `isSuffixOf` w = take (length w - 4) w
        | "тся" `isSuffixOf` w = take (length w - 3) w
        | "лся" `isSuffixOf` w = take (length w - 3) w
        | "лась" `isSuffixOf` w = take (length w - 4) w
        | "лось" `isSuffixOf` w = take (length w - 4) w
        | "лись" `isSuffixOf` w = take (length w - 4) w
        | "л" `isSuffixOf` w = take (length w - 1) w
        | "ла" `isSuffixOf` w = take (length w - 2) w
        | "ло" `isSuffixOf` w = take (length w - 2) w
        | "ли" `isSuffixOf` w = take (length w - 2) w
        | "ого" `isSuffixOf` w = take (length w - 3) w
        | "его" `isSuffixOf` w = take (length w - 3) w
        | "ому" `isSuffixOf` w = take (length w - 3) w
        | "ему" `isSuffixOf` w = take (length w - 3) w
        | "ым" `isSuffixOf` w = take (length w - 2) w
        | "им" `isSuffixOf` w = take (length w - 2) w
        | "ой" `isSuffixOf` w = take (length w - 2) w
        | "ей" `isSuffixOf` w = take (length w - 2) w
        | "ая" `isSuffixOf` w = take (length w - 2) w
        | "яя" `isSuffixOf` w = take (length w - 2) w
        | "ен" `isSuffixOf` w = take (length w - 2) w
        | "на" `isSuffixOf` w = take (length w - 2) w
        | "ое" `isSuffixOf` w = take (length w - 2) w
        | "ие" `isSuffixOf` w = take (length w - 2) w
        | "ые" `isSuffixOf` w = take (length w - 2) w
        | "ый" `isSuffixOf` w = take (length w - 2) w
        | "ий" `isSuffixOf` w = take (length w - 2) w
        | "ами" `isSuffixOf` w = take (length w - 3) w
        | "ов" `isSuffixOf` w = take (length w - 2) w
        | "ев" `isSuffixOf` w = take (length w - 2) w
        | "ем" `isSuffixOf` w = take (length w - 2) w
        | "ом" `isSuffixOf` w = take (length w - 2) w
        | "ах" `isSuffixOf` w = take (length w - 2) w
        | "ях" `isSuffixOf` w = take (length w - 2) w
        | "ам" `isSuffixOf` w = take (length w - 2) w
        | "ям" `isSuffixOf` w = take (length w - 2) w
        | "у" `isSuffixOf` w = take (length w - 1) w
        | "ю" `isSuffixOf` w = take (length w - 1) w
        | "е" `isSuffixOf` w = take (length w - 1) w
        | "и" `isSuffixOf` w = take (length w - 1) w
        | "о" `isSuffixOf` w = take (length w - 1) w
        | "ы" `isSuffixOf` w = take (length w - 1) w
        | "ь" `isSuffixOf` w = take (length w - 1) w
        | "я" `isSuffixOf` w = take (length w - 1) w
        | "а" `isSuffixOf` w = take (length w - 1) w
        | otherwise = w

findDuplicates :: Eq a => [a] -> [a]
findDuplicates [] = []
findDuplicates (x:xs) = if x `elem` xs then x : findDuplicates (filter (/= x) xs) else findDuplicates xs

countWordFrequencies :: [String] -> Map.Map String Int
countWordFrequencies = foldr (\w -> Map.insertWith (+) w 1) Map.empty

parseDate :: String -> Maybe Date
parseDate str =
    case split '-' str of
        [y, m, d] ->
            case (reads y, reads m, reads d) of
                ([(yearNum, "")], [(monthNum, "")], [(dayNum, "")]) ->
                    if isValidDate yearNum monthNum dayNum
                    then Just $ Date yearNum monthNum dayNum
                    else Nothing
                _ -> Nothing
        _ -> Nothing
  where
    split _ "" = []
    split sep s =
        let (a, b) = break (== sep) s
         in a : if null b then [] else split sep (drop 1 b)

    isValidDate y m d =
        y >= minYear && y <= maxYear &&
        m >= minMonth && m <= maxMonth &&
        d >= minDay && d <= daysInMonth y m

    daysInMonth y m =
        case m of
            2 -> if isLeapYear y then 29 else 28
            4 -> 30
            6 -> 30
            9 -> 30
            11 -> 30
            _ -> maxDay

    isLeapYear y =
        (y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0)

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

clampToBounds :: Double -> Double
clampToBounds x = max 0 $ min 5 x

sortDescendingBySecond :: [(a, Int)] -> [(a, Int)]
sortDescendingBySecond = sortBy (flip compare `on` snd)