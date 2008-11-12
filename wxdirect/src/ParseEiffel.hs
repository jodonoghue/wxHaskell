-----------------------------------------------------------------------------------------
{-| Module      :  ParseEiffel
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Parse the wxc Eiffel definition file.
-}
-----------------------------------------------------------------------------------------
module ParseEiffel( parseEiffel ) where

import Data.Char( digitToInt )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Types

import System.Environment ( getEnv )

{-----------------------------------------------------------------------------------------
   Testing
-----------------------------------------------------------------------------------------}
test
  = do files <- getDefaultEiffelFiles
       defss <- mapM parseEiffel files
       let defs  = concat defss
           haskellDefs = map show defs
       writeFile "../../wxh/Graphics/UI/WXH/WxcDefs.hs" (unlines haskellDefs)

getDefaultEiffelFiles :: IO [FilePath]
getDefaultEiffelFiles
  = do wxwin <- getEnv "WXWIN" `catch` \err -> return ""
       return [wxwin ++ "/wxc/include/wxc_defs.e"
              ,wxwin ++ "/wxc/ewxw/eiffel/spec/r_2_4/wx_defs.e"]

{-----------------------------------------------------------------------------------------
   Parse Eiffel
-----------------------------------------------------------------------------------------}
parseEiffel :: FilePath -> IO [Def]
parseEiffel fname
  = do putStrLn ("parsing: " ++ fname)
       input  <- readFile fname
       defss  <- mapM (parseDef fname) (lines input)
       -- putStrLn ("ok.")
       return (concat defss)

parseDef :: FilePath -> String -> IO [Def]
parseDef fname line
  = case parse pdef fname line of
      Left err  -> do putStrLn ("ignore: parse error : " ++ line)
                      return []
      Right mbd -> case mbd of
                     Just d  -> return [d]
                     Nothing -> return []     -- empty line


{-----------------------------------------------------------------------------------------
   Parse a constant definition
-----------------------------------------------------------------------------------------}
-- parse a definition: return Nothing on an empty definition
pdef :: Parser (Maybe Def)
pdef
  = do whiteSpace
       x <- option Nothing (pconstDef <|> pignore)
       eof
       return x

pconstDef :: Parser (Maybe Def)
pconstDef
  = do name <- identifier
       symbol ":"
       tp   <- pdefType
       reserved "is"
       (do x    <- pdefValue
           return (Just (Def name x tp))
        <|>
           return Nothing)  -- external definition
  <?> "constant definition"


pignore
  =   do{ reserved "external"; stringLiteral; return Nothing }
  <|> do{ reserved "alias"; stringLiteral; return Nothing }
  <|> do{ reserved "end"; return Nothing }
  <|> do{ reserved "class"; identifier; return Nothing }
  <|> do{ reserved "feature"; symbol "{"; reserved "NONE"; symbol "}"; return Nothing }
  <?> ""


pdefType :: Parser DefType
pdefType
  =   do reserved "BIT"
         bits <- natural
         return DefMask
  <|> do reserved "INTEGER"
         return DefInt
  <?> "integer type"

pdefValue :: Parser Int
pdefValue
  = lexeme $
    do sign <- option id (do{ symbol "-"; return negate })
       ds   <- many1 digit
       base <- option 10 (do{char 'B'; return 2})
       return (sign (convertNum base ds))
  where
    convertNum :: Int -> String -> Int
    convertNum base digits
      = foldl convert 0 digits
      where
        convert x c  = base*x + digitToInt c


{-----------------------------------------------------------------------------------------
   The lexer
-----------------------------------------------------------------------------------------}
lexer :: P.TokenParser ()
lexer
  = P.makeTokenParser $
    emptyDef
    { commentStart = "/*"
    , commentEnd   = "*/"
    , commentLine  = "--"          -- ignore pre-processor stuff, but fail to recognise "//"
    , nestedComments = True
    , identStart   = letter <|> char '_'
    , identLetter  = alphaNum <|> oneOf "_'"
    , caseSensitive = True
    , reservedNames = ["is","feature","class","end","NONE","BIT","INTEGER","external","alias"]
    }

whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
symbol        = P.symbol lexer
parens        = P.parens lexer
semi          = P.semi lexer
comma         = P.comma lexer
commaSep      = P.commaSep lexer
identifier    = P.identifier lexer
natural       = P.natural lexer
reserved      = P.reserved lexer

stringLiteral
  = lexeme $
    do char '"'
       many stringChar
       char '"'
       return ()

stringChar
  =   noneOf "\"%\n\v"
  <|> do{ char '%'; anyChar }
