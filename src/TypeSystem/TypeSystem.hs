module TypeSystem.TypeSystem where

import Parser.Parser
import Parser.TypeDefs
import TypeSystem.TypeDefs

typeCheck :: IAST -> Either ParserError IAST
typeCheck = return
