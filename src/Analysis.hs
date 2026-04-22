{-# LANGUAGE OverloadedStrings #-}

module Analysis
    ( buildDefectDict
    , groupReviews
    , analyzeProduct
    , buildProductRating
    , computeOverallScore
    , sortReviewsIfWeighted
    , sortByDateDesc
    , compareDates
    , applyRatingFormula
    , arithmeticMean
    , weightedMean
    , generateWeights
    , bayesianMean
    , extractKeywords
    , findTopWordsBySentiment
    , findTopReview
    ) where

import Data.List (find, nub, sortBy, isInfixOf, maximumBy)
import Data.Char (toLower)
import Data.Function (on)
import qualified Data.Map as Map
import Types
import Utils
import Scoring

--------------------------------------------------
-- функции
--------------------------------------------------

buildDefectDict :: [Review] -> DefectDictionary
buildDefectDict reviews = 
    let defectKeywords = ["брак", "дефект", "слом", "полом", "глюч", "завис", "бит", "трещин"]
        isDefect text = let lowerText = map toLower text
                        in any (`isInfixOf` lowerText) defectKeywords
     in DefectDictionary $ nub $ map reviewProduct $ filter (isDefect . reviewText) reviews

groupReviews :: [Review] -> Map.Map String [Review]
groupReviews = foldr (\r -> Map.insertWith (++) (reviewProduct r) [r]) Map.empty

analyzeProduct :: EvaluationRules -> SentimentDictionary -> DefectDictionary -> Product -> [Review] -> Either AnalysisError ProductRating
analyzeProduct rules dict defectDict prod reviews
    | null reviews = Left $ ProductNotFound (productId prod)
    | otherwise = Right $ buildProductRating prod reviews rules dict defectDict

buildProductRating :: Product -> [Review] -> EvaluationRules -> SentimentDictionary -> DefectDictionary -> ProductRating
buildProductRating prod reviews rules dict defectDict = ProductRating
    { ratedProduct = prod
    , reviewCount = length reviews
    , overallScore = computeOverallScore reviews rules dict defectDict
    , positiveHighlights = extractKeywords Positive reviews dict
    , negativeHighlights = extractKeywords Negative reviews dict
    , mostHelpfulReview = findTopReview reviews rules dict defectDict
    }

computeOverallScore :: [Review] -> EvaluationRules -> SentimentDictionary -> DefectDictionary -> Double
computeOverallScore rs rules dict defectDict = 
    let sorted = sortReviewsIfWeighted rs rules
        scores = map (\r -> evaluateReview rules dict defectDict r) sorted
     in applyRatingFormula (ratingFormula rules) scores

sortReviewsIfWeighted :: [Review] -> EvaluationRules -> [Review]
sortReviewsIfWeighted rs rules = 
    case ratingFormula rules of
        Weighted -> sortByDateDesc rs
        _ -> rs

sortByDateDesc :: [Review] -> [Review]
sortByDateDesc = sortBy (flip compareDates `on` reviewDate)

compareDates :: String -> String -> Ordering
compareDates d1 d2 =
    case (parseDate d1, parseDate d2) of
        (Just a, Just b) -> compare a b
        _ -> EQ

applyRatingFormula :: RatingFormula -> [Double] -> Double
applyRatingFormula Average scores = arithmeticMean scores
applyRatingFormula Weighted scores = weightedMean scores
applyRatingFormula (Bayesian m) scores = bayesianMean m scores

arithmeticMean :: [Double] -> Double
arithmeticMean scores = sum scores / fromIntegral (length scores)

weightedMean :: [Double] -> Double
weightedMean scores = 
    let weights = generateWeights (length scores)
     in sum (zipWith (*) scores weights) / sum weights

generateWeights :: Int -> [Double]
generateWeights n = take n $ [1.0, 0.9, 0.8, 0.7, 0.6] ++ repeat 0.5

bayesianMean :: Double -> [Double] -> Double
bayesianMean m scores = (sum scores + m * 2.5) / (fromIntegral (length scores) + m)

extractKeywords :: Sentiment -> [Review] -> SentimentDictionary -> [String]
extractKeywords target rs dict = take 3 $ findTopWordsBySentiment target rs dict

findTopWordsBySentiment :: Sentiment -> [Review] -> SentimentDictionary -> [String]
findTopWordsBySentiment target rs dict = 
    let allReviewsData = map extractWordsWithStems rs
        allOriginalWords = concatMap fst allReviewsData
        allStemmedWords = concatMap snd allReviewsData
        
        wordStemPairs = zip allOriginalWords allStemmedWords
        
        wordSentiments = [(w, getStemSentiment stem dict) | (w, stem) <- wordStemPairs]
        
        filtered = filter ((== target) . snd) wordSentiments
        
        freqMap = Map.fromListWith (+) [(w, 1) | (w, _) <- filtered]
        
        sorted = sortDescendingBySecond (Map.toList freqMap)
        
        topWords = take 3 $ map fst sorted
     in topWords
  where
    getStemSentiment :: String -> SentimentDictionary -> Sentiment
    getStemSentiment stem (SentimentDictionary dict') = 
        case find (\ww -> word ww == stem) dict' of
            Just ww -> sentiment ww
            Nothing -> Neutral

findTopReview :: [Review] -> EvaluationRules -> SentimentDictionary -> DefectDictionary -> String
findTopReview rs rules dict defectDict = 
    let withScores = zip rs $ map (\r -> evaluateReview rules dict defectDict r) rs
        best = maximumBy (compare `on` snd) withScores
     in reviewText $ fst best