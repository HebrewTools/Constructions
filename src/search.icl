module search

import StdDebug
import StdEnv
import StdMaybe

import Data.Encoding.GenBinary
import Data.Error
import Data.Map.GenJSON
import System.CommandLine
import System.Environment
import System.FilePath
import Text.GenJSON

import TextFabric.Import

import Bible

import Constructions

derive JSONEncode Pattern, Group, GroupFeature
derive JSONDecode Pattern, Group, GroupFeature

derive gBinaryEncodingSize GroupFeature, Result, ResultWord, Reference, Book, Map
derive gBinaryEncode GroupFeature, Result, ResultWord, Reference, Book, Map

Start w
	# (home,w) = getEnvironmentVariable "HOME" w
	  home = fromMaybe "/" home
	# (data,w) = import_tf trace_n required_features (home </> "text-fabric-data/etcbc/bhsa/tf/c") w
	| isError data
		= abort (fromError data+++"\n")
	# data = fromOk data
	# (io,w) = stdio w
	# (ok,io) = loop data io
	# (_,w) = fclose io w
	= setReturnCode (if ok 0 1) w

loop :: !DataSet !*File -> (!Bool, !*File)
loop data io
	# (line,io) = freadline io
	# pattern = fromJSON (fromString line)
	| isNothing pattern
		= (False, io)
	# results = search (fromJust pattern) data
	# results = encode results
	# io = io <<< encode (size results) <<< results <<< "\n"
	# (_,io) = fflush io
	= loop data io
