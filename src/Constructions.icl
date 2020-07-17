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
			{ make_word i main_node all_nodes
			\\ (main_node,all_nodes) <-
				reverse (find_words True (before+1) node_ref data) ++
				find_words False after (node_ref+1) data
			& i <- [0..]
			}
		}
	where
		(Just verse_ref) = get_first_ancestor_node_with (isOfType "verse") node_ref data

		// Each result tuple consists of the actual word, and a list of nodes
		// that can be used for displaying (the latter may for instance include
		// definite articles when these are skipped).
		find_words backwards n node data
			| n <= 0 ||
					node < 0 || node >= size data.nodes ||
					get_node_feature otype data.nodes.[node] <> "word"
				= []
			| not pattern.skip_article
				=
					[ (i, [i])
					\\ i <- if backwards [node-n..node] [node+1..node+n]
					| 0 <= i && i < size data.nodes &&
						get_node_feature otype data.nodes.[i] == "word"
					]
			| backwards
				| node-1 >= 0 && get_node_feature lex data.nodes.[node-1] == "H"
					= [(node, [node-1,node]) : find_words backwards (n-1) (node-2) data]
					= [(node, [node]) : find_words backwards (n-1) (node-1) data]
			| otherwise
				| node+1 < size data.nodes && get_node_feature lex data.nodes.[node] == "H"
					= [(node+1, [node,node+1]) : find_words backwards (n-1) (node+2) data]
					= [(node, [node]) : find_words backwards (n-1) (node+1) data]

		make_word i node_ref all_nodes =
			{ ResultWord
			| hebrew   = 'Text'.trim ('Text'.concat [text \\ n <- all_nodes, (_,text) <- get_text n data])
			, features = 'Map'.fromList
				[ (feature, get_node_feature id node)
				\\ (feature,id) <- features.[i]
				]
			}
		where
			node = data.nodes.[node_ref]

	group [] results
		= [results]
	group [rule:rules] results
		= sortBy
			((>) `on` length)
			[flatten (group rules g) \\ g <- groups rule results]

	groups _ []
		= []
	groups g=:{word,feature} [r:rs]
		# val = fromJust ('Map'.get feature r.words.[word+before].ResultWord.features)
		# (yes,no) = partition (\r -> fromJust ('Map'.get feature r.words.[word+before].ResultWord.features) == val) rs
		= [[r:yes]:groups g no]
