module Main (main) where

import VocabularyData
import Database
import FreqTable
import Trainer

import Control.Exception (bracket)
import Control.Lens
import Control.Monad.Trans.State
import Data.Acid
import Data.Maybe (listToMaybe)
import Data.Char (toUpper)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    showHeadline
    let test = initTestState
    lang <- getSourceOrTarget "from"
    test' <- execStateT (source.=lang) test
    lang' <- getSourceOrTarget "to"
    test'' <- execStateT (target.=lang') test'
    bracket (openLocalState emptyLearntList)
            (closeAcidState)
            (\db -> command db test'')

command :: AcidState LearntList -> TestState -> IO ()
command db test = do putStrLn "+===================================================+"
                     putStrLn "|                                                   |"
                     putStrLn "| what to do next? (type: \"help\" for help screen)   |"
                     putStrLn "|                                                   |"
                     putStrLn "+===================================================+"
                     cmd <- getLine
                     control db test cmd

control :: AcidState LearntList -> TestState -> String -> IO ()
control db test "help"          = do print_help
                                     command db test

control db test "next"          = do len <- query db LengthVocabulary
                                     if (len <=0)
                                       then do putStrLn "No vocabulary in list."
                                               putStrLn "Use \"add word\" to insert."
                                               command db test
                                       else do idx <- randomListIndex (fromIntegral len)
                                               f   <- query db (LookupFrequency idx)
                                               test' <- execStateT (currentWord.=freqTable!!(f-1)) test
                                               -- putStrLn $ "vocabulary list len: "++show len      --   _   _  _       _  --
                                               -- putStrLn $ "random index: "++show idx             --  | \ |_ |_⟩ | | | _ --
                                               -- putStrLn $ "frequency to the index: "++show       --  |_/ |_ |_⟩ |_| |_| --
                                               -- print test'                                       --                     --
                                               guess db test'

control db test "change source" = do lang <- getSourceOrTarget "from"
                                     test' <- execStateT (source.=lang) test
                                     -- print test'
                                     command db test'

control db test "change target" = do lang <- getSourceOrTarget "to"
                                     test' <- execStateT (target.=lang) test
                                     -- print test'
                                     command db test'

control db test ('a':'d':'d':' ':'w':'o':'r':'d':xs) =
    do let times = maybeRead xs :: Maybe Int
       _repeat db test times

control db test "clear all"     = do putStrLn "Are you sure to delete all learnt vocabularies?"
                                     putStrLn "Type \"yes\" or \"no\" to confirm."
                                     yesNo <- getLine
                                     yesNoElse db test yesNo

control db _    "exit"          = do closeAcidState db
                                     exitSuccess

control db test "print db"      = do frqKnowList <- query db ViewAllVocabulary
                                     print frqKnowList
                                     command db test

control db test _               = do putStrLn "Invalid Input"
                                     command db test

guess :: AcidState LearntList -> TestState -> IO ()
guess db test = do putStr $ "What is ("++show (test^.source)++"): "
                   putStrLn $ vocab (test^.currentWord) (test^.source)
                   putStr $ "Your answer ("++show (test^.target)++") is: "
                   hFlush stdout
                   answer <- getLine
                   let is_hinted = (test^.hinted)
                       is_correct = correct (test^.currentWord) (test^.target) answer
                       f = test^.currentWord.frq
                   if is_hinted
                     then if is_correct
                             then do _ <- update db (UpdateKnowledge f 3)
                                     putStrLn "Correct, +3 Knowledge!"
                                     putStr "Full Answer: "
                                     putStrLn (vocab (test^.currentWord) (test^.target))
                                     command db test
                             else do _ <- update db (UpdateKnowledge f (-2))
                                     putStrLn "Wrong, -2 Knowledge!"
                                     putStr "Correct Answer: "
                                     putStrLn (vocab (test^.currentWord) (test^.target))
                                     test' <- execStateT (hinted.=False) test
                                     command db test'
                     else if is_correct
                             then do _ <- update db (UpdateKnowledge f 5)
                                     putStrLn "Correct, +5 Knowledge!"
                                     putStr "Full Answer: "
                                     putStrLn (vocab (test^.currentWord) (test^.target))
                                     command db test
                             else do test' <- execStateT (hinted.=True) test
                                     putStr "Hint: "
                                     putStrLn (hint (test'^.currentWord) (test'^.source))
                                     guess db test'


_repeat :: AcidState LearntList -> TestState -> Maybe Int -> IO ()
_repeat db test (Just n)| n<=0 = command db test
                                | otherwise = do _ <- update db AddVocabulary
                                                 _repeat db test (Just (n-1))
_repeat db test Nothing = do _ <- update db AddVocabulary
                             command db test


yesNoElse :: AcidState LearntList -> TestState -> String -> IO ()
yesNoElse db test "yes" = do _ <- update db ClearVocabulary;command db test
yesNoElse db test "no"  = command db test
yesNoElse db test  _    = control db test "clear all"

print_help :: IO ()
print_help = do putStrLn ""
                putStr "| |_| |" ; putStrLn "help          -> prints this text"
                putStr "| | | |" ; putStrLn ""
                putStr "|  _  |" ; putStrLn "next          -> next random vocabulary"
                putStr "| |_  |" ; putStrLn "add word      -> adds a new vocabulary to the list of learnt words"
                putStr "| |_  |" ; putStrLn "clear all     -> clears all vocabulary from the list of learnt words"
                putStr "|     |" ; putStrLn ""
                putStr "| |   |" ; putStrLn "change source -> changes the source language"
                putStr "| |_  |" ; putStrLn "change target -> changes the target language"
                putStr "|  _  |" ; putStrLn ""
                putStr "| |_| |" ; putStrLn "print db      -> prints the database"
                putStr "| |   |" ; putStrLn "exit          -> guess what \"exits the program\""
--                 putStrLn "print test    -> prints the current test"
initTestState :: TestState
initTestState = TestState { _currentWord = freqTable!!0
                          , _source      = F
                          , _target      = D
                          , _hinted      = False }

langOptions :: IO ()
langOptions = do putStrLn "\tF/f for Français/French/Französisch"
                 putStrLn "\tD/d for Allemande/German/Deutsch"
                 putStrLn "\tE/e for Anglais/English/Englisch"

getSourceOrTarget :: String -> IO Language
getSourceOrTarget toOrFrom = do
    putStrLn $ "Which language do you want to translate "++toOrFrom++"?"
    langOptions
    lang <- getLine
    case (maybeRead . map toUpper . take 1) lang of Just l  -> return l
                                                    Nothing -> do putStrLn "Invalid Input"
                                                                  getSourceOrTarget toOrFrom

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

hint :: Word -> Language -> String
hint w F = w^.phrase
hint w D = w^.satz
hint w E = w^.sentence

vocab :: Word-> Language -> String
vocab w F = w^.fra
vocab w D = w^.deu
vocab w E = w^.eng

correct :: Word-> Language -> String -> Bool
correct w F str = elem str $ (subst2 . words . subst) (w^.fra)
correct w D str = elem str $ (subst2 . words . subst) (w^.deu)
correct w E str = elem str $ (subst2 . words . subst) (w^.eng)

subst ::String -> String
subst = map subst_
      where subst_ :: Char -> Char
            subst_ ';' = ' '
            subst_ '.' = ' '
            subst_ ',' = ' '
            subst_ '/' = ' '
            subst_ a = a

subst2 :: [String] -> [String]
subst2 = map (map subst_)
       where subst_ :: Char -> Char
             subst_ '_' = ' '
             subst_ a = a

showHeadline :: IO ()
showHeadline = do
    putStrLn "_        __  ____       ___     __    ____   __      _"
    putStrLn "\\\\      / / /  _  \\    /  _    /  \\  |    \\  ||     //"
    putStrLn " \\\\    / / /  / \\  \\  /  /    / /\\ \\ | [] /  ||    //"
    putStrLn "  \\\\  / / /  /   \\  \\/  /    / /__\\ \\|    \\  ||   //"
    putStrLn "   \\\\/ /  \\  \\___/  /\\  \\_  / /   / /| []  \\ ||__//"
    putStrLn "    \\_/    \\_______/  \\___ /_/   /_/ |_____/ |___/ "
    putStrLn "      __        __  ____          ____    _____      __"
    putStrLn "      \\\\\\      /  \\ \\   \\  __    |    \\  |  __ \\    /  \\"
    putStrLn "       \\\\\\    / /\\ \\ \\  _\\/_ \\   | [] /  | |  \\ \\  / /\\ \\"
    putStrLn "       //\\\\  / /__\\ \\ \\ \\\\/ \\ \\  |    \\  | |  / / / /__\\ \\"
    putStrLn "      //  \\\\ \\ \\   \\ \\ \\ \\   \\ \\ | []  \\ | |_/ / / /   / /"
    putStrLn "     //    \\\\ \\_\\   \\_\\ \\_\\   \\_\\|_____/ |____/ /_/   /_/"
    putStrLn ""
