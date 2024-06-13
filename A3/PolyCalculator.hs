{-# LANGUAGE CPP #-}

module PolyCalculator (repl, evaluate) where

#ifdef GRADESCOPE
import           AutograderUtils (getChar, getLine, print, putStrLn, readLn)
import           Prelude         hiding (getChar, getLine, print, putStrLn,
                                  readLn)
#endif
import           Parser          (Polynomial (..), expand, parse, polynomial,
                                  simplify)

{--

* Do NOT modify anything above this line.

                              IMPORTANT

If the first lines of your submission do not look like this,
your submission will NOT be graded properly and will receive ZERO marks.

{-# LANGUAGE CPP #-}

module PolyCalculator (repl, evaluate) where

#ifdef GRADESCOPE
import           AutograderUtils (getChar, getLine, print, putStrLn, readLn)
import           Prelude         hiding (getChar, getLine, print, putStrLn,
                                  readLn)
#endif
import           Parser          (Polynomial (..), expand, parse, polynomial,
                                  simplify)

----------------------------------------------------------------------------------------

Please, ONLY SUBMIT THIS FILE ALONGSIDE Parser.hs

----------------------------------------------------------------------------------------

* You are allowed to use anything available in Prelude and any syntax features

----------------------------------------------------------------------------------------

                            POLYNOMIAL CALCULATOR

In this task, you will create a REPL (Read-Evaluate-Print Loop) which will read polynomials
from the command line and perform various operations on them.

Your program should handle the following commands and reply with the corresponding messages.

Commands will be provided in a list with the following notation:

CMD describes the syntax of a command
ACT describes what the command should do
REP describes what should be printed as the response.
<whitespaces> denotes one or more consecutive whitespace characters (' ' aka ASCII 0x20 aka the longest key on your keyboard)
<polynomial> denotes any set of characters excluding newline characters
<number> denotes a non-negative integer

1. CMD load<whitespaces><polynomial>
   ACT Parse the provided polynomial and set it as current working polynomial.
   REP If the polynomial can not be parsed successfully, print "Could not parse the polynomial."
       Otherwise set the parsed polynomial as current and print "Polynomial loaded."

2. CMD evaluate<whitespaces><number>
   ACT Evaluate the current polynomial at the given point and print the result.
   REP If there is no current polynomial, print "No polynomial loaded."
       Otherwise evaluate the current polynomial at the given point and print one number - its value at that point.

3. CMD memorise
   ACT Save the current polynomial to memory.
   REP If there is no current polynomial, print "No polynomial loaded."
       Otherwise save the current polynomial to memory and print "Polynomial saved."

4. CMD recall
   ACT retrieve the saved polynomial from memory, replacing the current polynomial with it.
   REP If there is no polynomial saved in memory, print "No polynomial memorised."
       Otherwise print the saved polynomial using its Show instance.

5. CMD clear
   ACT Clear memory, removing the polynomial stored there (but not the current polynomial!).
   REP Do not print anything.

6. CMD reset
   ACT reset the calculator to the initial state as if it were just launched.
   REP Do not print anything.

7. CMD expand
   ACT expands the current polynomial using the expand function from A2 and makes it current
   REP If there is no current polynomial, print "No polynomial loaded."
       Otherwise print the expanded polynomial using its Show instance.

8. CMD simplify
   ACT Simplifies the current polynomial using the simplify function from A2 and makes it current
   REP If there is no current polynomial, print "No polynomial loaded."
       Otherwise print the simplified polynomial using its Show instance.

9. CMD quit
   ACT Stops the calculator.
   REP Do not print anything.

If any other command is entered, including if a correct command has unexpected parameters, print "Unbearable action!"

Your implementation should ignore any extra spaces after commands.

All messages you'll need to print are provided for you in constants. Please, use them.

----------------------------------------------------------------------------------------

To evaluate a polynomial, implement the function

    evaluate :: Polynomial -> Integer -> Integer

which returns the value of the given polynomial at the given point.

----------------------------------------------------------------------------------------

EXAMPLE SCENARIOS

The lines which YOU should input manually are marked with >
The responses given by your program are marked with <
DO NOT print these marks in your implementation! They are only given in examples for clarity.

Example 1.

ghci> repl
>hello
<Unbearable action!
>load
<Could not parse the polynomial.
>load p
<Could not parse the polynomial.
>load x^2    + x + x + x + 1 + x^2
<Polynomial loaded.
>evaluate 42
<3655
>expand
<x^2 + x + x + x + 1 + x^2
>simplify
<2x^2 + 3x + 1
>reset
>simplify
<No polynomial loaded.
>quit
ghci>

Example 2.

ghci> repl
>memorise
<No polynomial loaded.
>reset
>clear
>reset
>load c
<Could not parse the polynomial.
>load (x^3)(x^3 + 1) + 1
<Polynomial loaded.
>memorise
<Polynomial saved.
>evaluate 0
<1
>simplify
<x^6 + x^3 + 1
>evaluate 0
<1
>recall
<(x^3)(x^3 + 1) + 1
>quit
ghci>


This task is worth 10 POINTS.

--}

-- Constants for messages
parseFailed, polynomialLoaded, noPolynomialLoaded, polynomialSaved, noPolynomialMemorised, unbearable :: String
parseFailed = "Could not parse the polynomial."
polynomialLoaded = "Polynomial loaded."
noPolynomialLoaded = "No polynomial loaded."
polynomialSaved = "Polynomial saved."
noPolynomialMemorised = "No polynomial memorised."
unbearable = "Unbearable action!"

-- Your implementation of evaluate function
evaluate :: Polynomial -> Integer -> Integer
evaluate (Mono c d) x = c * (x ^ d)
evaluate (Add p1 p2) x = evaluate p1 x + evaluate p2 x
evaluate (Mul p1 p2) x = evaluate p1 x * evaluate p2 x

-- REPL function
repl :: IO ()
repl =  processCommand Nothing Nothing 

-- Function to process each command
processCommand :: Maybe Polynomial -> Maybe Polynomial -> IO ()
processCommand c s = do
   command <- getLine
   let cmd = words command
   case cmd of 
      ("load":xs) -> handleLoad xs c s
      ("evaluate":n:[]) -> handleEvaluate n c s 
      ("memorise":[]) -> handleMemorise c s 
      ("recall":[])   -> handleRecall c s 
      ("clear":[])    -> processCommand c Nothing
      ("reset":[])    -> processCommand Nothing Nothing
      ("expand":[])   -> handleExpand c s
      ("simplify":[]) -> handleSimplify c s
      ("quit":[])     -> return ()
      _ -> putStrLn unbearable >> processCommand c s

-- Function to handle the "load" command
handleLoad :: [String] -> Maybe Polynomial -> Maybe Polynomial -> IO ()
handleLoad xs c s = do
    let p = unwords xs
    case parse polynomial p of
        Just (poly, rest) | all (`elem` " \t\n") rest -> do
            putStrLn polynomialLoaded
            processCommand (Just poly) s
        _ -> do
            putStrLn parseFailed
            processCommand c s

-- Function to handle the "evaluate" command
handleEvaluate :: String -> Maybe Polynomial -> Maybe Polynomial -> IO ()
handleEvaluate n c s = case c of
        Nothing -> putStrLn noPolynomialLoaded >> processCommand c s
        Just poly -> do
            print (evaluate poly (read n :: Integer))
            processCommand c s  

-- Function to handle the "memorise" command
handleMemorise :: Maybe Polynomial -> Maybe Polynomial -> IO ()
handleMemorise Nothing _ = putStrLn noPolynomialLoaded >> processCommand Nothing Nothing
handleMemorise (Just poly) _ = putStrLn polynomialSaved >> processCommand (Just poly) (Just poly)

-- Function to handle the "recall" command
handleRecall :: Maybe Polynomial -> Maybe Polynomial -> IO ()
handleRecall c Nothing = putStrLn noPolynomialMemorised >> processCommand c Nothing
handleRecall c (Just poly) = print poly >> processCommand (Just poly) (Just poly)

-- Functions to handle the "expand" command
handleExpand :: Maybe Polynomial -> Maybe Polynomial -> IO ()
handleExpand Nothing s = putStrLn noPolynomialLoaded >> processCommand Nothing s
handleExpand (Just poly) s = do
    let expandedPoly = expand(poly)
    print expandedPoly
    processCommand (Just expandedPoly) s

-- Functions to handle the "simplify" command
handleSimplify :: Maybe Polynomial -> Maybe Polynomial -> IO ()
handleSimplify Nothing s = putStrLn noPolynomialLoaded >> processCommand Nothing s
handleSimplify (Just poly) s = do
    let simplifiedPoly = simplify poly
    print simplifiedPoly
    processCommand (Just simplifiedPoly) s
