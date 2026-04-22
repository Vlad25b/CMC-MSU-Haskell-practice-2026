{-# LANGUAGE OverloadedStrings #-}

module Scoring
    ( getWordWeight
    , computeBaseScore
    , evaluateReview
    , applyRepetitionPolicy
    , applyDatePolicy
    , penalizeByFrequency
    , limitByCount
    , penalizeByAge
    , computeAgePenalty
    , extractWordsWithStems
    ) where

import Data.List (find)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Types
import Utils

--------------------------------------------------
-- функции
--------------------------------------------------

getWordWeight :: SentimentDictionary -> String -> Maybe Double
getWordWeight (SentimentDictionary dict) w =
    case find (\ww -> word ww == w) dict of
        Just ww -> case sentiment ww of
            _ -> Just (weight ww)
        Nothing -> Nothing

computeBaseScore :: SentimentDictionary -> [String] -> Double
computeBaseScore dict ws = 
    let scores = catMaybes $ map (getWordWeight dict) ws
     in if null scores then 2.5 else calculateMean scores

extractWordsWithStems :: Review -> ([String], [String])
extractWordsWithStems r = 
    let originalWords = map (map toLower . cleanWord) $ words (reviewText r)
        stemmedWords = map wordStem originalWords
     in (originalWords, stemmedWords)

evaluateReview :: EvaluationRules -> SentimentDictionary -> DefectDictionary -> Review -> Double
evaluateReview rules dict _ review = 
    let (_, stemmedWords) = extractWordsWithStems review
        wordFreq = countWordFrequencies stemmedWords
        baseScore = computeBaseScore dict stemmedWords
        repPol = repetitionPolicy rules
        datePol = datePolicy rules
        repFactor = applyRepetitionPolicy repPol wordFreq
        dateFactor = applyDatePolicy datePol (reviewDate review)
     in clampToBounds $ baseScore * repFactor * dateFactor

applyRepetitionPolicy :: RepetitionPolicy -> Map.Map String Int -> Double
applyRepetitionPolicy IgnoreRepetition _ = 1.0
applyRepetitionPolicy (PenalizeRepetition f) freq = penalizeByFrequency f freq
applyRepetitionPolicy (LimitRepetition l) freq = limitByCount l freq

penalizeByFrequency :: Double -> Map.Map String Int -> Double
penalizeByFrequency f freq =
    let total = sum $ Map.elems freq
        maxFreq = maximum $ Map.elems freq
     in max 0.3 $ 1.0 - (fromIntegral maxFreq / fromIntegral total) * f

limitByCount :: Int -> Map.Map String Int -> Double
limitByCount l freq =
    let limited = sum $ map (min l) $ Map.elems freq
        original = sum $ Map.elems freq
     in if original > 0 then fromIntegral limited / fromIntegral original else 1.0

applyDatePolicy :: DatePolicy -> String -> Double
applyDatePolicy IgnoreDate _ = 1.0
applyDatePolicy (DatePenalty f) dateStr = penalizeByAge f dateStr

penalizeByAge :: Double -> String -> Double
penalizeByAge f dateStr =
    case parseDate dateStr of
        Just d -> computeAgePenalty f d
        Nothing -> 1.0

computeAgePenalty :: Double -> Date -> Double
computeAgePenalty f d = 
    let age = currentYear - year d
     in max 0.1 $ 1.0 - (fromIntegral age / 10.0) * f