module Parsing.Parser where
import Parsing.FolParser (happyFolParser, E(Failed, Ok) )
import Parsing.FolToken ( alexScanTokens )


parser = happyFolParser . alexScanTokens


