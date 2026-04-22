{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Json
    ( loadJsonFile
    , loadProducts
    , loadReviews
    , loadDictionary
    , loadRules
    , saveDefectDictionary
    , saveResults
    ) where

import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text as T
import Types
import Utils (findDuplicates, parseDate)
import Text.Printf (printf)

--------------------------------------------------
-- JSON экземпляры
--------------------------------------------------

instance FromJSON Date where
  parseJSON = withText "Date" $ \t ->
    case parseDate (T.unpack t) of
      Just d  -> return d
      Nothing -> fail "Invalid date format, expected YYYY-MM-DD"

instance FromJSON Sentiment where
  parseJSON = withText "Sentiment" $ \t ->
    case T.unpack t of
      "positive" -> return Positive
      "negative" -> return Negative
      "neutral"  -> return Neutral
      _          -> fail "Invalid sentiment"

instance ToJSON Sentiment where
  toJSON Positive = "positive"
  toJSON Negative = "negative"
  toJSON Neutral  = "neutral"

instance FromJSON WordWeight where
  parseJSON = withObject "WordWeight" $ \v ->
    WordWeight <$> v .: "word"
               <*> v .: "sentiment"
               <*> v .: "weight"

instance FromJSON Product where
  parseJSON = withObject "Product" $ \v ->
    Product <$> v .: "productId"
            <*> v .: "productName"
            <*> v .: "productDescription"
            <*> v .: "productCategory"

instance FromJSON Review where
  parseJSON = withObject "Review" $ \v ->
    Review <$> v .: "reviewId"
           <*> v .: "productId"
           <*> v .: "author"
           <*> v .: "date"
           <*> v .: "text"
           <*> v .: "sentiment"

instance ToJSON DefectDictionary where
  toJSON (DefectDictionary defects) = toJSON defects

instance FromJSON RepetitionPolicy where
  parseJSON = withObject "RepetitionPolicy" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "ignore" -> return IgnoreRepetition
      "penalize" -> PenalizeRepetition <$> v .: "factor"
      "limit" -> LimitRepetition <$> v .: "limit"
      _ -> fail "Invalid policy"

instance FromJSON DatePolicy where
  parseJSON = withObject "DatePolicy" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "ignore" -> return IgnoreDate
      "penalty" -> DatePenalty <$> v .: "factor"
      _ -> fail "Invalid policy"

instance FromJSON RatingFormula where
  parseJSON = withObject "RatingFormula" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "average" -> return Average
      "weighted" -> return Weighted
      "bayesian" -> Bayesian <$> v .: "prior"
      _ -> fail "Invalid formula"

instance FromJSON EvaluationRules where
  parseJSON = withObject "EvaluationRules" $ \v ->
    EvaluationRules <$> v .: "repetitionPolicy"
                    <*> v .: "datePolicy"
                    <*> v .: "ratingFormula"

instance ToJSON ProductRating where
  toJSON (ProductRating prod count score pos neg helpful) = object
    [ "productId" .= productId prod
    , "productName" .= productName prod
    , "productCategory" .= productCategory prod
    , "productDescription" .= productDescription prod
    , "reviewCount" .= count
    , "overallScore" .= (fromIntegral (round (score * 100.0)) / 100.0 :: Double)
    , "positiveHighlights" .= pos
    , "negativeHighlights" .= neg
    , "mostHelpfulReview" .= helpful
    ]

--------------------------------------------------
-- Функции загрузки JSON
--------------------------------------------------

loadJsonFile :: FromJSON a => String -> IO (Either AnalysisError a)
loadJsonFile file = do
    exists <- doesFileExist file
    if not exists
        then return $ Left $ FileNotFound file
        else do
            content <- BL.readFile file
            return $ case eitherDecode content of
                Left err -> Left $ JsonError err
                Right val -> Right val

loadProducts :: String -> IO (Either AnalysisError [Product])
loadProducts = loadJsonFile

loadReviews :: String -> IO (Either AnalysisError [Review])
loadReviews = loadJsonFile

loadDictionary :: String -> IO (Either AnalysisError SentimentDictionary)
loadDictionary file = do
    result <- loadJsonFile file :: IO (Either AnalysisError [WordWeight])
    case result of
        Left err -> return $ Left err
        Right dict ->
            let duplicates = findDuplicates $ map word dict
             in if null duplicates
                then return $ Right $ SentimentDictionary dict
                else case duplicates of
                    (d:_) -> return $ Left $ DuplicateWord d
                    [] -> return $ Left $ ParseError "Unexpected empty duplicates list"

loadRules :: String -> IO (Either AnalysisError EvaluationRules)
loadRules = loadJsonFile

saveDefectDictionary :: String -> DefectDictionary -> IO (Either AnalysisError ())
saveDefectDictionary filename (DefectDictionary defectList) = do
    BL.writeFile filename (encode defectList)
    return $ Right ()

saveResults :: String -> [ProductRating] -> IO (Either AnalysisError ())
saveResults file ratings = do
    BL.writeFile file (encodePretty ratings)
    return $ Right ()