module Main (main) where

import Lib
import System.Environment (getArgs)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [prodFile, revFile, dictFile, ruleFile, defectOutFile, outFile] ->
            runProgram prodFile revFile dictFile ruleFile defectOutFile outFile
        
        _ -> do
            putStrLn "=== Review Analysis Program ==="
            putStrLn "Running in interactive mode"
            putStrLn ""
            runInteractiveMode

runProgram :: String -> String -> String -> String -> String -> String -> IO ()
runProgram prodFile revFile dictFile ruleFile defectOutFile outFile = do
    putStrLn "=== Review Analysis Program ==="
    putStrLn $ "Products file: " ++ prodFile
    putStrLn $ "Reviews file: " ++ revFile
    putStrLn $ "Dictionary file: " ++ dictFile
    putStrLn $ "Rules file: " ++ ruleFile
    putStrLn $ "Defect dictionary output: " ++ defectOutFile
    putStrLn $ "Results output file: " ++ outFile
    putStrLn ""
    
    let inputFiles = InputFiles prodFile revFile dictFile ruleFile defectOutFile
    executeAnalysis inputFiles outFile

runInteractiveMode :: IO ()
runInteractiveMode = do
    putStrLn "Please enter the required files (or press Enter to use default names):"
    putStrLn ""
    
    prodFile <- askFile "Products file" "products.json"
    revFile <- askFile "Reviews file" "reviews.json"
    dictFile <- askFile "Dictionary file" "dictionary.json"
    ruleFile <- askFile "Rules file" "rules.json"
    defectOutFile <- askFile "Defect dictionary output file" "defects.json"
    outFile <- askFile "Results output file" "results.json"
    
    putStrLn ""
    putStrLn "Starting analysis"
    putStrLn ""
    
    let inputFiles = InputFiles prodFile revFile dictFile ruleFile defectOutFile
    executeAnalysis inputFiles outFile

askFile :: String -> String -> IO String
askFile prompt defaultValue = do
    putStr $ prompt ++ " [" ++ defaultValue ++ "]: "
    hFlush stdout
    input <- getLine
    if null input
        then return defaultValue
        else return input

executeAnalysis :: InputFiles -> String -> IO ()
executeAnalysis inputFiles outFile = do
    result <- runAnalysis inputFiles
    
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error during analysis: " ++ show err
            exitFailure
        
        Right ratings -> do
            saveResult <- saveResults outFile ratings
            case saveResult of
                Left err -> do
                    hPutStrLn stderr $ "Error saving results: " ++ show err
                    exitFailure
                Right () -> do
                    putStrLn ""
                    putStrLn "========================================"
                    putStrLn "Analysis completed successfully!"
                    putStrLn $ "Results saved to: " ++ outFile
                    putStrLn $ "Defect dictionary saved to: " ++ defectOutputFile inputFiles
                    putStrLn $ "Total products analyzed: " ++ show (length ratings)
                    putStrLn ""
                    
                    putStrLn "Summary of analyzed products:"
                    putStrLn "----------------------------------------"
                    mapM_ printSummary ratings
                    
                    putStrLn ""
                    putStrLn "Note: Defect dictionary was automatically generated"
                    putStrLn "      from reviews containing defect keywords."

printSummary :: ProductRating -> IO ()
printSummary rating = do
    let score = overallScore rating
        stars = replicate (round score) '*'
        name = productName (ratedProduct rating)
    putStrLn $ "  " ++ name ++ ": " ++ printf "%.2f" score ++ "/5 " ++ stars
    putStrLn $ "    Reviews: " ++ show (reviewCount rating)
    putStrLn $ "    Positive: " ++ unwords (take 3 $ positiveHighlights rating)
    if not (null (negativeHighlights rating))
        then putStrLn $ "    Negative: " ++ unwords (take 3 $ negativeHighlights rating)
        else putStrLn "    Negative: (none)"
    putStrLn ""