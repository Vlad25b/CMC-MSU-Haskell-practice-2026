{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runAnalysis
    , loadProducts
    , loadReviews
    , loadDictionary
    , loadRules
    , evaluateReview
    , analyzeProduct
    , saveResults
    , ProductRating(..)
    , AnalysisError(..)
    , InputFiles(..)
    , EvaluationRules(..)
    , RepetitionPolicy(..)
    , DatePolicy(..)
    , RatingFormula(..)
    , Product(..)
    , Review(..)
    , Sentiment(..)
    , WordWeight(..)
    , Date(..)
    ) where

import Types
import Json
import Analysis
import Scoring
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

--------------------------------------------------
-- Главная функция
--------------------------------------------------

runAnalysis :: InputFiles -> IO (Either AnalysisError [ProductRating])
runAnalysis inputs = do
    prods <- loadProducts $ productsFile inputs
    revs <- loadReviews $ reviewsFile inputs
    dict <- loadDictionary $ dictionaryFile inputs
    rules <- loadRules $ rulesFile inputs
    
    case (prods, revs, dict, rules) of
        (Left e, _, _, _) -> return $ Left e
        (_, Left e, _, _) -> return $ Left e
        (_, _, Left e, _) -> return $ Left e
        (_, _, _, Left e) -> return $ Left e
        (Right ps, Right rs, Right d, Right r) ->
            let defectDict = buildDefectDict rs
                revMap = groupReviews rs
                results = map (\p -> analyzeProduct r d defectDict p $ fromMaybe [] $ Map.lookup (productId p) revMap) ps
                validResults = [res | Right res <- results]
            in do
                saveResult <- saveDefectDictionary (defectOutputFile inputs) defectDict
                case saveResult of
                    Left err -> return $ Left err
                    Right () -> return $ Right validResults