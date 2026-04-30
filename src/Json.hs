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
  parseJSON = withText "Date" $ \t -> do
    case parseDate (T.unpack t) of
      Just d  -> return d
      Nothing -> fail "Invalid date format, expected YYYY-MM-DD"

instance FromJSON Sentiment where
  parseJSON = withText "Sentiment" $ \t -> do
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
  parseJSON = withObject "WordWeight" $ \v -> do
    w <- v .: "word"
    s <- v .: "sentiment"
    wt <- v .: "weight"
    return $ WordWeight w s wt

instance FromJSON Product where
  parseJSON = withObject "Product" $ \v -> do
    pid <- v .: "productId"
    pname <- v .: "productName"
    pdesc <- v .: "productDescription"
    pcat <- v .: "productCategory"
    return $ Product pid pname pdesc pcat

instance FromJSON Review where
  parseJSON = withObject "Review" $ \v -> do
    rid <- v .: "reviewId"
    pid <- v .: "productId"
    auth <- v .: "author"
    d <- v .: "date"
    txt <- v .: "text"
    sent <- v .: "sentiment"
    return $ Review rid pid auth d txt sent

instance ToJSON DefectDictionary where
  toJSON (DefectDictionary defects) = toJSON defects

instance FromJSON RepetitionPolicy where
  parseJSON = withObject "RepetitionPolicy" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "ignore"   -> return IgnoreRepetition
      "penalize" -> do
        factor <- v .: "factor"
        return $ PenalizeRepetition factor
      "limit"    -> do
        limit <- v .: "limit"
        return $ LimitRepetition limit
      _ -> fail "Invalid policy"

instance FromJSON DatePolicy where
  parseJSON = withObject "DatePolicy" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "ignore"  -> return IgnoreDate
      "penalty" -> do
        factor <- v .: "factor"
        return $ DatePenalty factor
      _ -> fail "Invalid policy"

instance FromJSON RatingFormula where
  parseJSON = withObject "RatingFormula" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "average"  -> return Average
      "weighted" -> return Weighted
      "bayesian" -> do
        prior <- v .: "prior"
        return $ Bayesian prior
      _ -> fail "Invalid formula"

instance FromJSON EvaluationRules where
  parseJSON = withObject "EvaluationRules" $ \v -> do
    repPol <- v .: "repetitionPolicy"
    datePol <- v .: "datePolicy"
    ratForm <- v .: "ratingFormula"
    return $ EvaluationRules repPol datePol ratForm

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