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

import System.Directory (doesFileExist)
import Data.List (find, nub, sortBy, maximumBy, isInfixOf)
import Data.Char (toLower)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Aeson.Types (Parser)
import Text.Printf (printf)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text as T

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

--------------------------------------------------
-- JSON экземпляры
--------------------------------------------------

instance FromJSON Date where
  parseJSON = withText "Date" $ \t ->
    case parseDate (T.unpack t) of
      Just d  -> return d
      Nothing -> fail "Invalid date format, expected YYYY-MM-DD"

instance ToJSON Date where
  toJSON (Date y m d) =
    let pad n = if n < 10 then '0' : show n else show n
     in String $ T.pack $ show y ++ "-" ++ pad m ++ "-" ++ pad d

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

instance ToJSON WordWeight where
  toJSON (WordWeight w s wt) = object
    [ "word" .= w
    , "sentiment" .= s
    , "weight" .= wt
    ]

instance FromJSON Product where
  parseJSON = withObject "Product" $ \v ->
    Product <$> v .: "productId"
            <*> v .: "productName"
            <*> v .: "productDescription"
            <*> v .: "productCategory"

instance ToJSON Product where
  toJSON (Product pid name desc cat) = object
    [ "productId" .= pid
    , "productName" .= name
    , "productDescription" .= desc
    , "productCategory" .= cat
    ]

instance FromJSON Review where
  parseJSON = withObject "Review" $ \v ->
    Review <$> v .: "reviewId"
           <*> v .: "productId"
           <*> v .: "author"
           <*> v .: "date"
           <*> v .: "text"
           <*> v .: "sentiment"

instance ToJSON Review where
  toJSON (Review rid pid author date text sentiment) = object
    [ "reviewId" .= rid
    , "productId" .= pid
    , "author" .= author
    , "date" .= date
    , "text" .= text
    , "sentiment" .= sentiment
    ]

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

instance ToJSON RepetitionPolicy where
  toJSON IgnoreRepetition = object ["type" .= ("ignore" :: String)]
  toJSON (PenalizeRepetition f) = object 
    [ "type" .= ("penalize" :: String)
    , "factor" .= f
    ]
  toJSON (LimitRepetition l) = object 
    [ "type" .= ("limit" :: String)
    , "limit" .= l
    ]

instance FromJSON DatePolicy where
  parseJSON = withObject "DatePolicy" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "ignore" -> return IgnoreDate
      "penalty" -> DatePenalty <$> v .: "factor"
      _ -> fail "Invalid policy"

instance ToJSON DatePolicy where
  toJSON IgnoreDate = object ["type" .= ("ignore" :: String)]
  toJSON (DatePenalty f) = object 
    [ "type" .= ("penalty" :: String)
    , "factor" .= f
    ]

instance FromJSON RatingFormula where
  parseJSON = withObject "RatingFormula" $ \v -> do
    typ <- v .: "type" :: Parser String
    case typ of
      "average" -> return Average
      "weighted" -> return Weighted
      "bayesian" -> Bayesian <$> v .: "prior"
      _ -> fail "Invalid formula"

instance ToJSON RatingFormula where
  toJSON Average = object ["type" .= ("average" :: String)]
  toJSON Weighted = object ["type" .= ("weighted" :: String)]
  toJSON (Bayesian p) = object 
    [ "type" .= ("bayesian" :: String)
    , "prior" .= p
    ]

instance FromJSON EvaluationRules where
  parseJSON = withObject "EvaluationRules" $ \v ->
    EvaluationRules <$> v .: "repetitionPolicy"
                    <*> v .: "datePolicy"
                    <*> v .: "ratingFormula"

instance ToJSON EvaluationRules where
  toJSON (EvaluationRules rp dp rf) = object
    [ "repetitionPolicy" .= rp
    , "datePolicy" .= dp
    , "ratingFormula" .= rf
    ]

instance ToJSON ProductRating where
  toJSON (ProductRating prod count score pos neg helpful) = object
    [ "productId" .= productId prod
    , "productName" .= productName prod
    , "productCategory" .= productCategory prod
    , "productDescription" .= productDescription prod
    , "reviewCount" .= count
    , "overallScore" .= (read (printf "%.2f" score) :: Double)
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
                else return $ Left $ DuplicateWord $ head duplicates

loadRules :: String -> IO (Either AnalysisError EvaluationRules)
loadRules = loadJsonFile

saveDefectDictionary :: String -> DefectDictionary -> IO (Either AnalysisError ())
saveDefectDictionary filename (DefectDictionary defectList) = do
    BL.writeFile filename (encode defectList)
    return $ Right ()

--------------------------------------------------
-- функции
--------------------------------------------------

findDuplicates :: Eq a => [a] -> [a]
findDuplicates [] = []
findDuplicates (x:xs) = if x `elem` xs then x : findDuplicates (filter (/= x) xs) else findDuplicates xs

countWordFrequencies :: [String] -> Map.Map String Int
countWordFrequencies = foldr (\w -> Map.insertWith (+) w 1) Map.empty

getWordWeight :: SentimentDictionary -> String -> Maybe Double
getWordWeight (SentimentDictionary dict) w =
    case find (\ww -> word ww == w) dict of
        Just ww -> case sentiment ww of
            Positive -> Just (weight ww)
            Negative -> Just (5 - weight ww)
            Neutral -> Just 2.5
        Nothing -> Nothing

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
         in a : if null b then [] else split sep (tail b)

    isValidDate year month day =
        year >= 1900 && year <= 2100 &&
        month >= 1 && month <= 12 &&
        day >= 1 && day <= daysInMonth year month

    daysInMonth year month =
        case month of
            2 -> if isLeapYear year then 29 else 28
            4 -> 30
            6 -> 30
            9 -> 30
            11 -> 30
            _ -> 31

    isLeapYear year =
        (year `mod` 400 == 0) || (year `mod` 4 == 0 && year `mod` 100 /= 0)

evaluateReview :: EvaluationRules -> SentimentDictionary -> DefectDictionary -> Review -> Double
evaluateReview rules dict _ review = 
    let wordsList = map (map toLower) $ words (reviewText review)
        wordFreq = countWordFrequencies wordsList
        baseScore = calculateBaseScore dict wordsList
        repFactor = applyRepetition (repetitionPolicy rules) wordFreq
        dateFactor = applyDate (datePolicy rules) (reviewDate review)
     in max 0 $ min 5 $ baseScore * repFactor * dateFactor
  where
    calculateBaseScore dict' ws = 
        let scores = catMaybes $ map (getWordWeight dict') ws
         in if null scores then 2.5 else sum scores / fromIntegral (length scores)
    
    applyRepetition IgnoreRepetition _ = 1.0
    applyRepetition (PenalizeRepetition f) freq =
        let total = sum $ Map.elems freq
            maxF = maximum $ Map.elems freq
         in max 0.3 $ 1.0 - (fromIntegral maxF / fromIntegral total) * f
    applyRepetition (LimitRepetition l) freq =
        let limited = sum $ map (min l) $ Map.elems freq
            original = sum $ Map.elems freq
         in if original > 0 then fromIntegral limited / fromIntegral original else 1.0
    
    applyDate IgnoreDate _ = 1.0
    applyDate (DatePenalty f) dateStr =
        case parseDate dateStr of
            Just d -> max 0.1 $ 1.0 - (fromIntegral (2026 - year d) / 10.0) * f
            Nothing -> 1.0

buildDefectDict :: [Review] -> DefectDictionary
buildDefectDict reviews = 
    let keywords = ["брак", "дефект", "сломал", "не работает", "поломка", "сломан"]
        isDefect text = any (`isInfixOf` map toLower text) keywords
     in DefectDictionary $ nub $ map reviewProduct $ filter (isDefect . reviewText) reviews

groupReviews :: [Review] -> Map.Map String [Review]
groupReviews = foldr (\r -> Map.insertWith (++) (reviewProduct r) [r]) Map.empty

analyzeProduct :: EvaluationRules -> SentimentDictionary -> DefectDictionary -> Product -> [Review] -> Either AnalysisError ProductRating
analyzeProduct rules dict defectDict product reviews = 
    if null reviews
        then Left $ ProductNotFound (productId product)
        else Right $ ProductRating
            { ratedProduct = product
            , reviewCount = length reviews
            , overallScore = calcScore reviews
            , positiveHighlights = extract Positive reviews dict
            , negativeHighlights = extract Negative reviews dict
            , mostHelpfulReview = findBest reviews
            }
  where
    compareDates date1 date2 =
        case (parseDate date1, parseDate date2) of
            (Just d1, Just d2) -> compare d1 d2
            _ -> EQ
    
    sortByDateDesc = sortBy (flip compareDates `on` reviewDate)
    
    calcScore rs = 
        let sortedRs = case ratingFormula rules of
                Weighted -> sortByDateDesc rs
                _ -> rs
            scores = map (evaluateReview rules dict defectDict) sortedRs
            n = fromIntegral $ length scores
         in case ratingFormula rules of
                Average -> sum scores / n
                Weighted -> 
                    let ws = [1.0, 0.9, 0.8, 0.7, 0.6] ++ repeat 0.5
                     in sum (zipWith (*) scores ws) / sum (take (length scores) ws)
                Bayesian m -> (sum scores + m * 2.5) / (n + m)
    
    extract targetSentiment rs (SentimentDictionary dict') = 
        let allWords = concatMap (words . map toLower . reviewText) rs
            uniq = nub allWords
            wordSent = [(w, getSentiment' w) | w <- uniq]
            filtered = filter ((== targetSentiment) . snd) wordSent
            top = take 3 $ map fst $ sortBy (flip compare `on` snd) 
                  [(w, length $ filter (== w) allWords) | w <- map fst filtered]
         in top
      where
        getSentiment' w = maybe Neutral sentiment $ find ((== w) . word) dict'
    
    findBest rs = 
        let scored = zip rs $ map (evaluateReview rules dict defectDict) rs
            best = maximumBy (compare `on` snd) scored
         in reviewText $ fst best

saveResults :: String -> [ProductRating] -> IO (Either AnalysisError ())
saveResults file ratings = do
    BL.writeFile file (encodePretty ratings)
    return $ Right ()

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
                _ <- saveDefectDictionary (defectOutputFile inputs) defectDict
                return $ Right validResults