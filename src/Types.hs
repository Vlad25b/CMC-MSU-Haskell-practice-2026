{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Date(..)
    , Product(..)
    , Review(..)
    , Sentiment(..)
    , WordWeight(..)
    , SentimentDictionary(..)
    , DefectDictionary(..)
    , RepetitionPolicy(..)
    , DatePolicy(..)
    , RatingFormula(..)
    , EvaluationRules(..)
    , ProductRating(..)
    , AnalysisError(..)
    , InputFiles(..)
    ) where

--------------------------------------------------
-- Типы данных
--------------------------------------------------

data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  }
  deriving (Show, Eq, Ord)

data Product = Product
  { productId          :: String
  , productName        :: String
  , productDescription :: String
  , productCategory    :: String
  }
  deriving (Show, Eq)

data Review = Review
  { reviewId       :: String
  , reviewProduct  :: String
  , reviewAuthor   :: String
  , reviewDate     :: String
  , reviewText     :: String
  , sentimentOfRev :: Sentiment
  }
  deriving (Show, Eq)

data Sentiment = Positive | Neutral | Negative
  deriving (Show, Eq)

data WordWeight = WordWeight
  { word      :: String
  , sentiment :: Sentiment
  , weight    :: Double
  }
  deriving (Show, Eq)

newtype SentimentDictionary = SentimentDictionary [WordWeight]
  deriving (Show, Eq)

newtype DefectDictionary = DefectDictionary [String]
  deriving (Show, Eq)

data RepetitionPolicy
  = IgnoreRepetition
  | PenalizeRepetition Double
  | LimitRepetition Int
  deriving (Show, Eq)

data DatePolicy
  = IgnoreDate
  | DatePenalty Double
  deriving (Show, Eq)

data RatingFormula
  = Average
  | Weighted
  | Bayesian Double
  deriving (Show, Eq)

data EvaluationRules = EvaluationRules
  { repetitionPolicy :: RepetitionPolicy
  , datePolicy       :: DatePolicy
  , ratingFormula    :: RatingFormula
  }
  deriving (Show, Eq)

data ProductRating = ProductRating
  { ratedProduct       :: Product
  , reviewCount        :: Int
  , overallScore       :: Double
  , positiveHighlights :: [String]
  , negativeHighlights :: [String]
  , mostHelpfulReview  :: String
  }
  deriving (Show, Eq)

data AnalysisError
  = DuplicateWord String
  | InvalidField String
  | InvalidRule String
  | ProductNotFound String
  | ParseError String
  | FileNotFound String
  | JsonError String
  deriving (Show, Eq)

data InputFiles = InputFiles
  { productsFile      :: String
  , reviewsFile       :: String
  , dictionaryFile    :: String
  , rulesFile         :: String
  , defectOutputFile  :: String
  }
  deriving (Show, Eq)