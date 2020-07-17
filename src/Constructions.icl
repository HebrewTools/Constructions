implementation module Constructions

import StdEnv
import StdMaybe

import Control.Applicative
import Data.Func
import Data.Functor
import Data.List
from Data.Map import :: Map
import qualified Data.Map as Map
import Data.Maybe
from Text import class Text, instance Text String
import qualified Text as Text

import TextFabric
import TextFabric.BHSA
import TextFabric.Filters

import Bible

required_features :: [String]
required_features =
	[ "book"
	, "chapter"
	, "g_word_utf8"
	, "gn"
	, "lex"
	, "ls"
	, "nu"
	, "pdp"
	, "ps"
	, "sp"
	, "st"
	, "trailer_utf8"
	, "verse"
	, "vs"
	, "vt"
	]

instance toString GroupFeature
where
	toString Lexeme             = "lex"
	toString PartOfSpeech       = "sp"
	toString PhraseDependentPoS = "pdp"
	toString LexicalSet         = "ls"
	toString Gender             = "gn"
	toString Number             = "nu"
	toString Person             = "ps"
	toString State              = "st"
	toString VerbalStem         = "vs"
	toString VerbalTense        = "vt"

instance == GroupFeature
where
	(==) Lexeme             x = x=:Lexeme
	(==) PartOfSpeech       x = x=:PartOfSpeech
	(==) PhraseDependentPoS x = x=:PhraseDependentPoS
	(==) LexicalSet         x = x=:LexicalSet
	(==) Gender             x = x=:Gender
	(==) Number             x = x=:Number
	(==) Person             x = x=:Person
	(==) State              x = x=:State
	(==) VerbalStem         x = x=:VerbalStem
	(==) VerbalTense        x = x=:VerbalTense

instance < GroupFeature where (<) x y = toString x < toString y

//* A human-readable representation of a feature value.
pretty_feature :: !GroupFeature !String -> String
pretty_feature _ s = s // TODO

check_pattern :: !Pattern -> Maybe String
check_pattern {lexeme,groups,context_size=ctx=:(before,after)} =
	empty_lexeme <|>
	illegal_context <|>
	foldl (\ok g -> ok <|> check_group g) Nothing groups
where
	empty_lexeme = if (size lexeme == 0) (Just "The lexeme may not be empty.") Nothing

	illegal_context
		| before < 0
			= Just "The before-context may not be negative."
		| after < 0
			= Just "The before-context may not be negative."
			= Nothing

	check_group {word,feature}
		| (word < 0 && word < 0-before) || (word > 0 && word > after)
			= Just ("A group uses word "+++toString word+++" which is outside the context.")
		| word == 0 && feature=:Lexeme
			= Just "It is not necessary to group on (0, Lexeme)."
			= Nothing

search :: !Pattern !DataSet -> [Result]
search pattern data
	# node_refs = find pattern.lexeme data
	# results = [make_result features n data \\ n <|- node_refs]
	= flatten (group pattern.groups results)
where
	(before,after) = pattern.context_size

	lex = fromJust (get_node_feature_id (toString Lexeme) data)
	book = fromJust (get_node_feature_id "book" data)
	chapter = fromJust (get_node_feature_id "chapter" data)
	verse = fromJust (get_node_feature_id "verse" data)
	otype = fromJust (get_node_feature_id "otype" data)

	features =
		{!
			[ (feature, fromJust (get_node_feature_id (toString feature) data))
			\\ {word,feature} <- pattern.groups
			| word == i
			]
		\\ i <- [0-before..after]
		}

	find lexeme data = filter_node_refs
		(\_ n data ->
			get_node_feature lex n == lexeme &&
			get_node_feature otype n == "word")
		data

	make_result features node_ref data =
		{ reference =
			{ book    = fromString (get_node_feature book verse_ref)
			, chapter = toInt (get_node_feature chapter verse_ref)
			, verse   = toInt (get_node_feature verse verse_ref)
			}
		, words =
			{ make_word i n
			\\ n <- [node_ref-before..node_ref+after]
			& i <- [0..]
			| 0 <= n && n < size data.nodes &&
				get_node_feature otype data.nodes.[n] == "word"
			}
		}
	where
		(Just verse_ref) = get_first_ancestor_node_with (isOfType "verse") node_ref data

		make_word i node_ref =
			{ ResultWord
			| word     = 'Text'.trim ('Text'.concat [text \\ (_,text) <- get_text node_ref data])
			, features = 'Map'.fromList
				[ (feature, get_node_feature id node)
				\\ (feature,id) <- features.[i]
				]
			}
		where
			node = data.nodes.[node_ref]

	group_and_flatten rule results
		# groups = group rule results
		# groups = sortBy ((>) `on` length) groups
		= flatten groups

	group [] results
		= [results]
	group [rule:rules] results
		= [flatten (group rules g) \\ g <- groups rule results]

	groups _ []
		= []
	groups g=:{word,feature} [r:rs]
		# val = fromJust ('Map'.get feature r.words.[word+before].ResultWord.features)
		# (yes,no) = partition (\r -> fromJust ('Map'.get feature r.words.[word+before].ResultWord.features) == val) rs
		= [[r:yes]:groups g no]
