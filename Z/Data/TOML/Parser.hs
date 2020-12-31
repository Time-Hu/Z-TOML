module Z.Data.TOML.Parser where

import qualified Z.Data.JSON.Value as J 
import qualified Z.Data.Parser as P 
import Z.Data.Parser ( Parser, word8, skipWhile, integer, double', bytes )
import Z.Data.Text (Text, validate)
import Z.Data.TOML.Value 
import Data.Word ( Word8 )
import Control.Applicative ( Alternative((<|>)), many, some )
import Control.Applicative.Combinators
import Data.Functor ( ($>) )
import Z.Data.Vector
import Data.Scientific (fromFloatDigits)


keyVP :: Parser KeyV
keyVP = (,) <$> keyP <* ws <* word8 equal <* ws <*> valueP

keyVsP :: Parser KeyVs
keyVsP = many (ws *> keyVP <* wsComments)

tableKeyP :: Parser TableKey
tableKeyP = AKey <$> doubleSquare keyP
        <|> StdKey <$> square keyP

tomlP :: Parser [Toml]
tomlP = (:) <$> (TKeyV <$> keyVsP) <*> many tableP
      <|> many tableP

tableP :: Parser Toml 
tableP = Table <$> (ws *> tableKeyP <* wsComments) <*> keyVsP

valueP :: Parser Value
valueP = tStringP 
     <|> tIntegerP
     <|> tDoubleP
     <|> tArrayP
     <|> tInlineTableP


tStringP :: Parser Value 
tStringP = TString <$> (singleQTriple stringLitP
                   <|> singleQ stringLitP
                   <|> J.string)

tIntegerP :: Parser Value
tIntegerP = TInteger <$> integer   

tDoubleP :: Parser Value
tDoubleP = TDouble <$> double' 

tArrayP :: Parser Value 
tArrayP = TArray <$> arrayP valueP

tInlineTableP :: Parser Value
tInlineTableP = TInlineTable <$> curly (wsSurround keyVP `sepBy` word8 comma)



--curly (sepBy valueP comma)
-------------------------------------------------------------------
-- many :: Parser a -> Parser [a]
-- many p = (:) <$> p <*> some p <|> pure []

-- some :: Parser a -> Parser [a]
-- some p = (:) <$> p <*> many p 

comments :: Parser () 
comments = word8 hashtag <* skipWhile (not . isEol)

arrayP :: Parser a ->  Parser [a] 
arrayP p = square (comment p `sepEndBy` word8 comma)

stringMulP :: Parser Text
stringMulP = undefined 

stringBscP :: Parser Text
stringBscP = doubleQ J.string  

stringLitP :: Parser Text
stringLitP = do 
    x <- P.takeWhile (/= singleQuote)
    return . validate $ x 

keyP :: Parser Text 
keyP = ((<$>) (<>) ((<$>) (<>) ((validate <$> P.takeWhile isKeyChar)
                           <|> stringBscP)
                  <*> (comment (word8 dot) $> "."))
       <*> keyP)
    <|> validate <$> P.takeWhile isKeyChar
    <|> stringBscP

square :: Parser a -> Parser a
square p = word8 openSquare *> (p <* wsComments) <* word8 closeSquare 

doubleSquare :: Parser a -> Parser a
doubleSquare p = word8 openSquare *> word8 openSquare 
               *> (p <* wsComments) 
               <* word8 closeSquare <* word8 closeSquare

curly :: Parser a -> Parser a
curly p = word8 openCurly *> ws *> p <* ws <* word8 closeCurly 

singleQ :: Parser a -> Parser a 
singleQ p = word8 singleQuote *> p <* word8 singleQuote 

singleQTriple :: Parser a -> Parser a 
singleQTriple p = bytes "\'\'\'"
               *> p 
               <* bytes "\'\'\'"

doubleQ :: Parser a -> Parser a 
doubleQ p = word8 doubleQuote *> p <* word8 doubleQuote 

comment :: Parser a -> Parser a 
comment p = wsComments *> p <* wsComments

ws :: Parser ()
ws = many (word8 9 <|> word8 32) $> ()

wsSurround :: Parser a -> Parser a 
wsSurround = between ws ws

nl :: Parser ()
nl = word8 newline 
 <|> (word8 0x0D *> word8 newline)

wsComments :: Parser [()]
wsComments = many ((comments <|> nl) <|> (word8 9 <|> word8 32))

-- sepBy' :: Parser a -> Word8 -> Parser [a]
-- sepBy' p w = ((:) <$> p) <*> (ws *> word8 w *> ws *> (sepBy' p w
--                                       <|> pure [])
--                              <|>  pure [])

-- sepBy :: Parser a -> Word8 -> Parser [a]
-- sepBy p w = ((:) <$> p) <*> (ws *> word8 w *> ws *> sepBy p w
--                         <|>  pure [])

token :: Parser a -> Parser a 
token p = p <* ws
-------------------------------------------------------------------
isEol :: Word8 -> Bool 
isEol w = w < 20 && w /= 9

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9

isAlpha :: Word8  -> Bool 
isAlpha w = w - 97 <= 25 || w - 65 <= 25

isSpace :: Word8 -> Bool 
isSpace w = w == 9 || w == 32 

isKeyChar :: Word8 -> Bool 
isKeyChar w = isDigit w || isAlpha w || w == hyphen || w == underscore
-------------------------------------------------------------------
openSquare  :: Word8
openSquare  = 0x5B

closeSquare :: Word8
closeSquare = 0x5D

hashtag     :: Word8
hashtag     = 0x23

comma       :: Word8
comma       = 44

openCurly   :: Word8
openCurly   = 123

closeCurly  :: Word8
closeCurly  = 125

doubleQuote :: Word8
doubleQuote = 34

singleQuote :: Word8
singleQuote = 39

equal       :: Word8 
equal       = 61

underscore  :: Word8 
underscore  = 95

hyphen      :: Word8 
hyphen      = 45

dot :: Word8 
dot = 46

newline :: Word8
newline = 0x0A











---------------------------
convertValue :: Value -> J.Value 
convertValue (TString t) = J.String t 
convertValue (TInteger i) = J.Number $ fromInteger i 
convertValue (TDouble d) = J.Number $ fromFloatDigits d 
convertValue (TArray xs) = J.Array . pack $ convertValue <$> xs
convertValue (TInlineTable kvs) = J.Object . pack $ convertKeyV <$> kvs

convert :: Toml -> J.Value
convert (Table (AKey tk) kvs) = J.Array . pack $ J.Object . singleton . convertKeyV <$> kvs
convert (Table (StdKey tk) kvs) = J.Object . pack $ convertKeyV <$> kvs

convertKeyV :: (Text, Value) -> (Text, J.Value)
convertKeyV (x, y) = (x, convertValue y)