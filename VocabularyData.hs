{-# LANGUAGE TemplateHaskell #-}

module VocabularyData where

import Control.Lens
import Data.Map (Map)

type Francais  = String
type English   = String
type Deutsch   = String
type Frequency = Int
type Knowledge = Int

data Language = F|E|D deriving (Show,
                                Read)

data Use = Adjective [Feature]
         | Adverb [Feature]
         | Conjunction [Feature]
         | Determiner [Feature]
         | Interjection [Feature]
         | Noun [Feature]
         | NounOrAdjective [Feature]
         | Preposition [Feature]
         | Pronoun [Feature]
         | Verb [Feature] deriving (Show)

data Feature = Feminine
             | Invariable
             | Masculine
             | Plural
             | NoDistinctFeminine
             | NoDistinctPlural deriving (Show)

data Word = Word { _frq     :: Frequency
                 , _fra     :: Francais
                 , _eng     :: English
                 , _deu     :: Deutsch
                 , _uses    :: [Use]
                 , _phrase  :: Francais
                 , _sentence:: English
                 , _satz    :: Deutsch }

instance Show Word where
    show Word { _frq      = frq
              , _fra      = fra
              , _eng      = _
              , _deu      = _
              , _uses     = _
              , _phrase   = _
              , _sentence = _
              , _satz     = _ } = "Word ( freq: "++show frq++ ", vocab: "++ fra++")"


type VocMap= Map Frequency Word

data TestState = TestState { _currentWord :: Word
                           , _source      :: Language
                           , _target      :: Language
                           , _hinted      :: Bool} deriving (Show)

makeLenses ''Word
makeLenses ''TestState
