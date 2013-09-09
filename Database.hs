{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}

module Database where

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy

import Data.Typeable

import Data.List (sortBy)
import Data.Ord (comparing)

import VocabularyData

data LearntList= LearntList [(Frequency, Knowledge)]
    deriving (Typeable,Show)

$(deriveSafeCopy 0 'base ''LearntList)

emptyLearntList :: LearntList
emptyLearntList = LearntList []

addVocabulary :: Update LearntList ()
addVocabulary = do LearntList lst<- get
                   let f = aux lst
                   put (LearntList ((f,0):lst))
              where aux :: [(Frequency,a)] -> Frequency
                    aux [] = 1
                    aux xs = 1+(maximum . map fst) xs

clearVocabulary :: Update LearntList ()
clearVocabulary = put (LearntList [])

lengthVocabulary :: Query LearntList Int
lengthVocabulary = do LearntList lst <- ask
                      return $ length lst

viewVocabulary :: Int -> Query LearntList [(Frequency, Knowledge)]
viewVocabulary limit = do LearntList lst <- ask
                          return $ take limit lst

viewAllVocabulary :: Query LearntList [(Frequency, Knowledge)]
viewAllVocabulary = do LearntList lst <- ask
                       return lst

updateKnowledge :: Frequency -> Knowledge -> Update LearntList ()
updateKnowledge f k = do LearntList lst <- get
                         let new_lst = sortBySnd $ map (<+>(f,k)) lst
                         put (LearntList new_lst)
                    where sortBySnd = sortBy (comparing snd)
                          (x1,y1) <+> (x2,y2) |x1==x2    = (x1,max 0 (y1+y2))
                                              |otherwise = (x1,y1)

lookupFrequency :: Int -> Query LearntList Frequency
lookupFrequency n = do LearntList lst <- ask
                       return $ fst (lst !! n)

$(makeAcidic ''LearntList ['addVocabulary
                          ,'viewVocabulary
                          ,'viewAllVocabulary
                          ,'lengthVocabulary
                          ,'lookupFrequency
                          ,'clearVocabulary
                          ,'updateKnowledge])
