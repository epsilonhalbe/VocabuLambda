VocabuLambda
============

a vocabulary learning software written in haskell

Synopsis
--------

VocabuLambda has the vocabulary data stored in a frequency table where the
words are sorted by it's frequency of use in the French language.
for all words exists an English translation, the German translation is
incomplete, as of yet.

The learnt vocabulary are stored in an ACID-database persistently and state
transformations make heavy use of the lens library.

