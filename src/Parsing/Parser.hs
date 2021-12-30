module Parsing.Parser where
import Parsing.FolParser (happyFolParser, E(Failed, Ok) )
import Parsing.FolToken ( alexScanTokens )


parser i  = case (happyFolParser . alexScanTokens) i of
  Ok for -> for
  Failed s -> error "no parse"


