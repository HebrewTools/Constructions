module server

import StdDebug
import StdEnv

import Data.Encoding.GenBinary
import Data.Func
import qualified Data.Map as Map
import Data.Tuple
import System.FilePath
import System.Time
import System._Unsafe
from Text import class Text, instance Text String
import qualified Text
import Text.HTML
import Text.Language

import iTasks
import iTasks.Extensions.DateTime
import iTasks.Internal.TaskState
import iTasks.UI.Editor.Common

import ABC.Interpreter.JavaScript

import TextFabric.Import

import Constructions

import Bible

gEq{|DataSet|} _ _ = abort "gEq{|DataSet|}\n"
JSONEncode{|DataSet|} _ _ = abort "JSONEncode{|DataSet|}\n"
JSONDecode{|DataSet|} _ _ = abort "JSONDecode{|DataSet|}\n"
gText{|DataSet|} _ _ = abort "gText{|DataSet|}\n"
gEditor{|DataSet|} = abort "gEditor{|DataSet|}\n"

derive class iTask GroupStart
derive gEq Pattern, Group, GroupFeature, Result, ResultWord, Reference, Book
derive JSONEncode Pattern, Group, GroupFeature, Result, ResultWord, Reference, Book
derive JSONDecode Pattern, Group, GroupFeature, Result, ResultWord, Reference, Book
derive gBinaryDecode GroupFeature, Result, ResultWord, Reference, Book, Map, GroupStart
derive gText Pattern, Group, GroupFeature, Result, ResultWord, Reference, Book

gEditor{|Result|} = abort "gEditor{|Result|}\n"

gEditor{|GroupFeature|} = bijectEditorValue toInt fromInt (chooseWithDropdown choices)
where
	choices =
		[ "Lexeme"
		, "Part of speech"
		, "Phrase-dependent part of speech"
		, "Lexical set"
		, "Gender"
		, "Number"
		, "Person"
		, "State"
		, "Verbal stem"
		, "Verbal tense"
		, "Pronominal suffix"
		]

	toInt Lexeme             =  0
	toInt PartOfSpeech       =  1
	toInt PhraseDependentPoS =  2
	toInt LexicalSet         =  3
	toInt Gender             =  4
	toInt Number             =  5
	toInt Person             =  6
	toInt State              =  7
	toInt VerbalStem         =  8
	toInt VerbalTense        =  9
	toInt PronominalSuffix   = 10

	fromInt  0 = Lexeme
	fromInt  1 = PartOfSpeech
	fromInt  2 = PhraseDependentPoS
	fromInt  3 = LexicalSet
	fromInt  4 = Gender
	fromInt  5 = Number
	fromInt  6 = Person
	fromInt  7 = State
	fromInt  8 = VerbalStem
	fromInt  9 = VerbalTense
	fromInt 10 = PronominalSuffix
	fromInt  _ = Lexeme

FIELD_WIDTH :== widthAttr (ExactSize 80)

gEditor{|Group|} = bijectEditorValue
	(\g -> (g.Group.word, g.Group.feature))
	(\(w,f) -> {word=w, feature=f})
	(container2
		(Label "Word" @>> FIELD_WIDTH @>> integerField)
		(Label "Feature" @>> widthAttr (ExactSize 160) @>> gEditor{|*|}))

gEditor{|Pattern|} = bijectEditorValue
	(\p -> (p.lexeme, fst p.context_size, snd p.context_size, p.skip_article, ((), p.groups)))
	(\(l,b,a,sa,(_,gs)) -> {lexeme=l, groups=gs, context_size=(b,a), skip_article=sa})
	(container5
		(Label "Lexeme"       @>> FIELD_WIDTH @>> minlengthAttr 1 @>> textField)
		(Label "Words before" @>> FIELD_WIDTH @>> minAttr 0 @>> maxAttr 10 @>> integerField)
		(Label "Words after"  @>> FIELD_WIDTH @>> minAttr 0 @>> maxAttr 10 @>> integerField)
		(Label "Skip definite article" @>> checkBox)
		(container2
			(viewConstantValue "Group the results on:" textView)
			gEditor{|*|}))

resultsEditor :: Editor (Pattern, ([GroupStart], [Result]))
resultsEditor = editorWithClientSideInit jsInit (comapEditorValue format htmlView)
where
	format :: !(!Pattern, !(![GroupStart], ![Result])) -> HtmlTag
	format (pattern, (groups, results)) = TableTag [ClassAttr "results"]
		[ TheadTag []
			[ TrTag [] $
				[ThTag [] [Text (toString word+++"."+++toString feature)] \\ {word,feature} <- pattern.groups] ++
				[ThTag [] [Text "Reference"]] ++
				[ThTag [ClassAttr "word-header"] [Text (toString i)] \\ i <- [after,after-1..0-before]]
			]
		, TbodyTag [] (makeTableRows 0 groups results)
		]
	where
		(before,after) = pattern.context_size

		makeTableRows _ _ [] = []
		makeTableRows i groups all_results=:[r:rs]
			| isEmpty groups || (hd groups).result_index <> i =
				[ TrTag [] $
					[ TdTag [] [] \\ {word,feature} <- pattern.groups
					] ++
					[ TdTag [] [Text (formatReference r.reference)]
					: reverse [TdTag [] (wordToHtml w) \\ w <-: r.words]
					]
				: makeTableRows (i+1) groups rs
				]
			| otherwise = let [g:gs] = groups in
				[ TrTag [ClassAttr "header"] $
					[ TdTag [] $ if (g.group_index==j)
						[DivTag [ClassAttr "group"] [Text ('Text'.concat
							[ toString word
							, "."
							, toString feature
							, ": "
							, g.GroupStart.value
							])]]
						[]
					\\ {word,feature} <- pattern.groups
					& j <- [0..]
					] ++
					[ TdTag
						[ColspanAttr (toString (size r.words+1))]
						[Text (pluralisen English (n_results g.group_index) "result")]
					]
				: makeTableRows i gs all_results
				]
		where
			n_results gi = case dropWhile (\g -> g.group_index > gi) (tl groups) of
				[]    -> length rs+1
				[g:_] -> g.result_index-i

		formatReference {book,chapter,verse} = 'Text'.concat
			[ englishName book
			, " "
			, toString chapter
			, ":"
			, toString verse
			]

		wordToHtml {hebrew,features} =
			[ DivTag [ClassAttr "hebrew"] [Text hebrew]
			:
				[ DivTag [ClassAttr ("ft ft-"+++toString ft)] [Text val]
				\\ (ft, val) <- 'Map'.toList features
				]
			]

	jsInit :: !JSVal !*JSWorld -> *JSWorld
	jsInit me w
		# (cb,w) = jsWrapFun (jsInit` me) me w
		= (me .# "initDOMEl" .= cb) w
	jsInit` me _ w
		# w = (me .# "domEl" .# "style" .# "boxSizing" .= "border-box") w
		# w = (me .# "domEl" .# "style" .# "width" .= "100%") w
		# w = (me .# "domEl" .# "innerHTML" .= me .# "attributes" .# "value") w
		# (cb,w) = jsWrapFun (jsInit`` me) me w
		= addJSFromUrl "/js/construction-table.js" (Just cb) w
	jsInit`` me _ w
		= (jsGlobal "initConstructionTable" .$! me .# "domEl") w

editorWithClientSideInit :: !(JSVal *JSWorld -> *JSWorld) !(Editor a) -> Editor a
editorWithClientSideInit init editor=:{Editor | genUI} =
	{ Editor
	| editor
	& genUI = withClientSideInit init (unsafeCoerce genUI) // TODO: this should be possible without unsafeCoerce
	}

Start w = doTasks
	[ onStartup stopSearchBackendWhenInactive
	, onRequest "/" main
	]
	w

//* The `Bool` indicates whether a search is running.
search_backend :: SimpleSDSLens (Maybe (TaskId, Bool))
search_backend =: sdsFocus "search_backend" memoryShare

search_stdin :: SimpleSDSLens [String]
search_stdin =: mapReadWrite
	( fromMaybe []
	, \s _ -> Just (Just s)
	)
	Nothing
	(sdsFocus "search_stdin" memoryShare)

search_stdouterr :: SimpleSDSLens ([String], [String])
search_stdouterr =: mapReadWrite
	( fromMaybe ([], [])
	, \s _ -> Just (Just s)
	)
	Nothing
	(sdsFocus "search_stdouterr" memoryShare)

maybeStartSearchBackend :: Task ()
maybeStartSearchBackend =
	get search_backend >>- \backend ->
	if (isNothing backend) startSearchBackend (return ())

startSearchBackend :: Task ()
startSearchBackend =
	get applicationDirectory >>- \dir ->
	appendTopLevelTask 'Map'.newMap True (
		externalProcess
			{tv_sec=1,tv_nsec=0}
			(dir </> "search") [] (Just dir)
			externalProcessGraceful Nothing
			search_stdin search_stdouterr >-|
		set Nothing search_backend
	) >>- \id ->
	set (Just (id, False)) search_backend @! ()

stopSearchBackend :: Task ()
stopSearchBackend =
	get search_backend >>- \mbBackend -> case mbBackend of
		Nothing ->
			return ()
		Just (id,_) ->
			set Nothing search_backend >-|
			removeTask id topLevelTasks

// TODO: using stdout for the communication is rather slow...
search :: !Pattern -> Task ([GroupStart], [Result])
search pattern = ApplyLayout replaceWithLoader @>> (
	wait (\b -> isNothing b || not (snd (fromJust b))) search_backend >-|
	maybeStartSearchBackend >-|
	upd (\(Just (id,_)) -> Just (id, True)) search_backend >-|
	set ([], []) search_stdouterr >-|
	set [toString (toJSON pattern) +++ "\n"] search_stdin >-|
	(
		ApplyLayout hideUI @>>
		wait isNothing search_backend >-|
		throw "search backend crashed" @! ()
	) ||-
	foreverStIf
		(\s
			| size s < IF_INT_64_OR_32 8 4 -> True
			# len = decode (s % (0, IF_INT_64_OR_32 7 3))
			| isNothing len
				-> False
				-> size s < fromJust len)
		""
		(\buffer ->
			wait (not o isEmpty) stdout >>- \out ->
			set ([], []) search_stdouterr @!
			'Text'.concat [buffer:out]) >>- \out ->
	case decode (out % (0, IF_INT_64_OR_32 7 3)) of
		Nothing ->
			stopSearchBackend >-|
			throw "illegal response from search backend"
		Just len
			# rest = out % (IF_INT_64_OR_32 8 4 + len, size out - 1)
			  out = out % (IF_INT_64_OR_32 8 4, IF_INT_64_OR_32 7 3 + len)
			->
				set [rest] stdout >-|
				if (len > 750000) (throw "too many results (try to reduce the context size)")
				case decode out of
					Just results ->
						upd (\(Just (id,_)) -> Just (id, False)) search_backend >-|
						return results
					Nothing ->
						stopSearchBackend >-|
						throw "illegal response from search backend"
	)
where
	stdout = mapReadWrite (fst, \out (_,err) -> Just (out,err)) Nothing search_stdouterr

	replaceWithLoader = sequenceLayouts [removeSubUIs (SelectByDepth 1), setUIType UILoader]

derive gDefault TaskListFilter, TaskId
derive gEq TaskMeta, TaskChange, InstanceType
derive JSONEncode TaskChange
derive JSONDecode TaskChange
derive gText TaskMeta, TaskChange, InstanceType
derive gEditor TaskMeta, TaskChange, InstanceType

sessions :: SDSLens () (TaskId,[TaskMeta]) [TaskMeta]
sessions =: sdsFocus (TaskId 0 0, TaskId 0 0, gDefault{|*|}, onlySessions) taskListMetaData
where
	onlySessions =
		{ includeSessions   = True
		, includeDetached   = False
		, includeStartup    = False
		, includeTaskReduct = False
		, includeTaskIO     = False
		}

stopSearchBackendWhenInactive :: Task ()
stopSearchBackendWhenInactive = forever $
	waitForTimer False 5 >-|
	get sessions >>- \(_,sessions) ->
	if (isEmpty sessions) stopSearchBackend (return ())

main :: Task ()
main =
	maybeStartSearchBackend >-| // To start loading the features in the background already
	withShared Nothing \pattern ->
	Title "Construction searcher" @>>
	ApplyLayout (arrangeWithSideBar 0 LeftSide True) @>>
	ScrollContent @>>
	(
		(ScrollContent @>>
			foreverSt {lexeme="", groups=[], context_size=(0,0), skip_article=False}
			(\p ->
				viewInformation [] explanation ||-
				updateInformation [] p >>*
				[OnAction ActionOk (hasValue \p -> set (Just p) pattern @! p)]))
	-&&-
		onChange pattern \pattern -> case pattern of
			Just pattern -> case check_pattern pattern of
				Nothing ->
					catchAll (
						search pattern >>- \results ->
						if (isEmpty (snd results))
							(Hint "Warning:" @>> viewInformation [] "No results." @! ())
							(viewInformation [ViewUsing (tuple pattern) resultsEditor] results @! ())
					)
						(\e -> viewInformation [] ("Error: "+++e+++".") @! ())
				Just err ->
					Hint "Error:" @>>
					viewInformation [] err @! ()
			Nothing ->
				viewInformation [] "Enter a pattern and press 'Ok'." @! ()
	) @!
	()
where
	explanation = DetailsTag []
		[ SummaryTag [] [Text "Explanation"]
		, PTag []
			[ Text "This tool allows you to search for constructions in which a lexeme occurs in the Hebrew Bible."
			]
		, PTag []
			[ Text "Enter a lexeme in "
			, ATag [HrefAttr "https://annotation.github.io/text-fabric/writing/hebrew.html", TargetAttr "_blank"] [Text "ETCBC transcription"]
			, Text " (e.g.: "
			, TtTag [] [Text "PNH/"]
			, Text " for the noun פנה) and indicate how many words of context (before and after the result) you want to see."
			]
		, PTag []
			[ Text "The results can be grouped on features of the word itself and the words in the context. "
			, Text "For example: grouping on word 1 by Lexeme gives a list grouped by the lexeme of the following word; "
			, Text "grouping on word -1 by State gives a list grouped by the state (absolute or construct) of the previous word."
			]
		]

onChange :: !(sds () r w) !(r -> Task a) -> Task a | iTask r & iTask a & Registrable sds & TC w
onChange sds f = whileUnchanged sds \v -> f v @? toUnstable
where
	toUnstable v = case v of
		Value v _
			-> Value v False
			-> v
