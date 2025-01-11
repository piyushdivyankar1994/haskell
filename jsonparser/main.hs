import Data.Char (isSpace)
import Data.List (isPrefixOf)

data Value = Bool Bool | String String | Int Int | Float Float | Array Array | Object Object | Null deriving (Show)

type Array = [Value]

type Pair = (String, Value)

type Object = [Pair]

data JSON = JObject Object | JArray Array deriving (Show)

parseArray :: String -> JSON
parseObject :: String -> JSON
parseJson :: String -> JSON
parseJson str
  | head str == '[' = parseArray $ tail str
  | head str == '{' = parseObject $ tail str

parseObject str = JObject [("abc", String str)]

helper :: JSON -> Array
helper (JArray a) = a

parseArray str = g (valueCur str) (valueRest str) []
  where
    g s r a
      | head r == ',' = g (valueCur $ tail r) (valueRest $ tail r) (parseValue s : a)
      | head r == ']' = JArray (parseValue s : a)
      | head r == '[' = JArray (Array (helper (parseArray $ tail r)) : a)
      | head r == '{' = parseObject $ tail r

parseValue :: String -> Value
parseValue s = g (strip s)
  where
    g vs
      | head vs == '"' = String (takeWhile (/= '"') $ tail vs)
      | head vs == 't' = Bool True
      | head vs == 'f' = Bool False
      | head vs == 'n' = Null

strip = f . f where f = reverse . dropWhile isSpace

valueCur = takeWhile (`notElem` ",[]{}")

valueRest = dropWhile (`notElem` ",[]{}")

-- takeWhile (\x -> ((x != ',') || (x != '[])))
myList = [Array [Bool True], Bool True]

main = do
  putStrLn "Enter Json"
  x <- getLine
  print (parseArray "[true, false, null, \"ab[c\"]")