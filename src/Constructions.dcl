definition module Constructions

from StdMaybe import :: Maybe

from Data.Map import :: Map

from TextFabric import :: DataSet

from Bible import :: Reference

:: Pattern =
	{ lexeme       :: !String       //* The lexeme that we search for
	, groups       :: ![Group]      //* How to group the occurrences
	, context_size :: !(!Int, !Int) //* Number of words to display before and after the search lexeme
	}

:: Group =
	{ word    :: !Int          //* Distance of the word we're grouping from the center (may be zero or negative)
	, feature :: !GroupFeature //* The feature to group on
	}

//* See https://etcbc.github.io/bhsa/features/0_home/#word-features.
:: GroupFeature
	= Lexeme
	| PartOfSpeech
	| PhraseDependentPoS
	| LexicalSet
	| Gender
	| Number
	| Person
	| State
	| VerbalStem
	| VerbalTense

:: Result =
	{ reference :: !Reference    //* Book, chapter, and verse
	, words     :: !{ResultWord} //* The word and its context
	}

:: ResultWord =
	{ word     :: !String                  //* Hebrew text
	, features :: !Map GroupFeature String //* Relevant features
	}

//* The text-fabric features required to use this module (use when calling `import_tf`).
required_features :: [String]

//* Check whether a pattern contains any errors.
check_pattern :: !Pattern -> Maybe String

//* Search for a pattern.
search :: !Pattern !DataSet -> [Result]
