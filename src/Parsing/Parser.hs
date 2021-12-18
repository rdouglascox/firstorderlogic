module Parsing.Parser (parser) where

import Parsing.FolParser ( happyFolParser )
import Parsing.FolToken ( alexScanTokens )

parser = happyFolParser . alexScanTokens