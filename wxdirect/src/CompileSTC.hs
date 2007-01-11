module CompileSTC ( compileSTC ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Data.Char
import Data.List
import Control.Monad

import Types

compileSTC :: Bool       -- ^ Verbose
           -> FilePath   -- ^ Outputdir
           -> [FilePath] -- ^ Input files (stc.h)
           -> IO ()
compileSTC verbose outputDir inputs = do
  dfs <- mapM parseH inputs
  let (ds,fs) = unzip dfs
      d = concat ds --currently unused
      f = concat fs
      h_target = outputDir ++ "include/stc_gen.h"
      cpp_target = outputDir ++ "src/stc_gen.cpp"
  putStrLn $ "generating: " ++ h_target
  writeFile h_target $ (glue "\n\n" $ map headerfunc f) ++ "\n"
  putStrLn $ "generating: " ++ cpp_target
  writeFile cpp_target $ (glue "\n" $ map cppfunc f) ++ "\n"
  when verbose $
       putStrLn $ "Wrote type macros and c wrappers for " ++ show (length f) ++ " functions."

parseH fname = do putStrLn ("parsing: " ++ fname)
		  input <- liftM lines $ readFile fname
		  let (defs, cpp) = partitionDefines $ input
		  case parse plines fname (unlines cpp) of
		    Left err -> print err >> return ([],[])
		    Right funcs -> return (defs,filter convertable funcs)

-- returns a list of defenitions, see Types.Def
-- and a new list of lines without #define's
partitionDefines :: [String] -> ([Def], [String])
partitionDefines lns = (defs, cpp)
    where (rdefs, cpp) = partition ("#define wxSTC_" `isPrefixOf`) lns
	  defs = map (toDef . (drop $ length "#define ")) rdefs
	  toDef x = let (name,value) = break (==' ') x
                    in Def name (read value) DefInt

plines = whiteSpace >> many1 pfunc

pfunc :: Parser (String, String, [(String, String)])
pfunc = do ret <- identifier
	   stars <- option "" $ symbol "*"
	   func <- identifier
	   args <- parens $ commaSep $ arg
	   symbol ";"
	   return (ret ++ stars,func,args)
    where arg = do option "" $ try $ symbol "const"
		   t <- identifier
		   stars <- option "" $ symbol "*"
		   option "" $ symbol "&"
		   name <- identifier
		   return (t ++ stars, name)

{-----------------------------------------------------------------------------------------
   The lexer
-----------------------------------------------------------------------------------------}

lexer :: P.TokenParser ()
lexer
  = P.makeTokenParser $ javaStyle

whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
symbol        = P.symbol lexer
parens        = P.parens lexer
semi          = P.semi lexer
comma         = P.comma lexer
commaSep      = P.commaSep lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer


{-----------------------------------------------------------------------------------------
   code gen
-----------------------------------------------------------------------------------------}

convertable (t,f,a) = elem t ["int", "bool", "void"]

glue str strs = concat $ intersperse str strs

-- CPP function generator (Creates functions that can be exported as C functions)

cppfunc x = macro x ++ arguments x ++ "\n" ++ body x 

macro (ret, func, args) = "EWXWEXPORT(" ++ ret ++ ", wxStyledTextCtrl_" ++ func ++ ")"

arguments (ret, func, args) = "(" ++ glue ", " params ++ ")"
	where 
	params = "void* _obj" : rest
	rest = map transType args

	transType ("int",      n) = "int "   ++ n
	transType ("bool",     n) = "bool "  ++ n
	transType ("void",     n) = "void "  ++ n
	-- transType ("wxString", n) = "char* " ++ n
	transType ("wxString", n) = "wxChar* " ++ n
	transType ("wxPoint",  n) = glue ", " ["int " ++ n ++ p | p <- ["_x","_y"]]
	transType ("wxColour", n) = glue ", " ["int " ++ n ++ c | c <- ["_r","_g","_b"]]
	transType (_,          n) = "void* " ++ n
		
body (ret, func, args) = "{\n" ++ "#ifdef wxUSE_STC\n  "
                         ++ maybeReturn ++ " ((wxStyledTextCtrl*) _obj)->"
                         ++ func ++ "(" ++ glue ", " params ++ ");\n"
                         ++ maybeElse ++ "#endif\n}"
	where 
	maybeReturn = if ret == "void" then "" else "return"
	maybeElse = if ret == "void" then "" else "#else\n  return NULL;\n"
	params = map transParam args
	transParam ("int",       n) = n
	transParam ("bool",      n) = n
	transParam ("void",      n) = n
	transParam ("wxString",  n) = n
	transParam ("wxPoint",   n) = "wxPoint(" ++ n ++ "_x," ++ n ++ "_y)"
	transParam ("wxColour",  n) = "wxColour(" ++ n ++ "_r," ++ n ++ "_g," ++ n ++ "_b)"
	transParam ("wxSTCDoc*", n) = n
	transParam (t,           n) = "*(" ++ t ++ "*) " ++ n
      
-- Generator for functions signatures with type macros

headerfunc (ret, func, args) = returnType ret ++ " wxStyledTextCtrl_" ++ func 
                               ++ "(" ++ glue ", " params ++ ");"
	where
	
	returnType "bool"  = "TBool"
	returnType "int"   = "int"
	returnType "void"  = "void"
	returnType _       = error "wtf?"

	params = "TSelf(wxStyledTextCtrl) _obj" : rest
	rest = map transType args
	
	transType ("bool",     n) = "TBool "   ++ n
	transType ("int",      n) = "int "     ++ n
	transType ("int*",     n) = "int* "    ++ n
	transType ("void",     n) = "void "    ++ n
	transType ("char*",    n) = "TString " ++ n
	-- transType ("wchar_t*", n) = "TString " ++ n
	-- transType ("wxChar*",  n) = "TString " ++ n
	transType ("wxString", n) = "TString " ++ n
	transType ("wxPoint",  n) = "TPoint(" ++ n ++ "_x," ++ n ++ "_y)"
	transType ("wxColour", n) = "TColorRGB(" ++ n ++ "_r," ++ n ++ "_g," ++ n ++ "_b)"
	transType (t,          n) | "*" `isSuffixOf` t = "TClass(" ++ init t ++ ") " ++ n
                            | otherwise = "TClass(" ++ t ++ ") " ++ n
