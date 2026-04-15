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
import Data.List (find, nub, sortBy, maximumBy, isInfixOf, isSuffixOf)
import Data.Char (toLower, isPunctuation)
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
  toJSON (Review rid pid author date text revSentiment) = object
    [ "reviewId" .= rid
    , "productId" .= pid
    , "author" .= author
    , "date" .= date
    , "text" .= text
    , "sentiment" .= revSentiment
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
                else case duplicates of
                    (d:_) -> return $ Left $ DuplicateWord d
                    [] -> return $ Left $ ParseError "Unexpected empty duplicates list"

loadRules :: String -> IO (Either AnalysisError EvaluationRules)
loadRules = loadJsonFile

saveDefectDictionary :: String -> DefectDictionary -> IO (Either AnalysisError ())
saveDefectDictionary filename (DefectDictionary defectList) = do
    BL.writeFile filename (encode defectList)
    return $ Right ()

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

getWordWeight :: SentimentDictionary -> String -> Maybe Double
getWordWeight (SentimentDictionary dict) w =
    case find (\ww -> word ww == w) dict of
        Just ww -> case sentiment ww of
            Positive -> Just (weight ww)
            Negative -> Just (weight ww)
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
         in a : if null b then [] else split sep (drop 1 b)

    isValidDate y m d =
        y >= 1900 && y <= 2100 &&
        m >= 1 && m <= 12 &&
        d >= 1 && d <= daysInMonth y m

    daysInMonth y m =
        case m of
            2 -> if isLeapYear y then 29 else 28
            4 -> 30
            6 -> 30
            9 -> 30
            11 -> 30
            _ -> 31

    isLeapYear y =
        (y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0)

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

computeBaseScore :: SentimentDictionary -> [String] -> Double
computeBaseScore dict ws = 
    let scores = catMaybes $ map (getWordWeight dict) ws
     in if null scores then 2.5 else calculateMean scores

calculateMean :: [Double] -> Double
calculateMean xs = sum xs / fromIntegral (length xs)

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
    let age = 2026 - year d
     in max 0.1 $ 1.0 - (fromIntegral age / 10.0) * f

clampToBounds :: Double -> Double
clampToBounds x = max 0 $ min 5 x

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

sortDescendingBySecond :: [(a, Int)] -> [(a, Int)]
sortDescendingBySecond = sortBy (flip compare `on` snd)

findTopReview :: [Review] -> EvaluationRules -> SentimentDictionary -> DefectDictionary -> String
findTopReview rs rules dict defectDict = 
    let withScores = zip rs $ map (\r -> evaluateReview rules dict defectDict r) rs
        best = maximumBy (compare `on` snd) withScores
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