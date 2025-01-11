encodeChar :: Char -> String
encodeChar ch =
  case ch of
    'A' -> "*-"
    'B' -> "-***"
    'C' -> "-*-*"
    'D' -> "-**"
    'E' -> "*"
    'F' -> "**-*"
    'G' -> "--*"
    'H' -> "****"
    'I' -> "**"
    'J' -> "*---"
    'K' -> "-*-"
    'L' -> "*-**"
    'M' -> "--"
    'N' -> "-*"
    'O' -> "---"
    'P' -> "*--*"
    'Q' -> "--*-"
    'R' -> "*-*"
    'S' -> "***"
    'T' -> "-"
    'U' -> "**-"
    'V' -> "***-"
    'W' -> "*--"
    'X' -> "-**-"
    'Y' -> "-*--"
    'Z' -> "--**"
    ' ' -> " "
    _ -> error "invalid character"

encode :: String -> String
encode (x : xs) = encodeChar x ++ " " ++ encode xs
encode [] = []

decodeChar :: String -> String
decodeChar str =
  case str of
    "*-" -> "A"
    "-***" -> "B"
    "-*-*" -> "C"
    "-**" -> "D"
    "*" -> "E"
    "**-*" -> "F"
    "--*" -> "G"
    "****" -> "H"
    "**" -> "I"
    "*---" -> "J"
    "-*-" -> "K"
    "*-**" -> "L"
    "--" -> "M"
    "-*" -> "N"
    "---" -> "O"
    "*--*" -> "P"
    "--*-" -> "Q"
    "*-*" -> "R"
    "***" -> "S"
    "-" -> "T"
    "**-" -> "U"
    "***-" -> "V"
    "*--" -> "W"
    "-**-" -> "X"
    "-*--" -> "Y"
    "--**" -> "Z"
    " " -> " "
    [] -> ""
    a -> error "\ninvalid character " a

morseDecode :: String -> String
morseDecode [] = []
morseDecode s = decodeChar (takeWhile (/= ' ') s) ++ morseDecode (drop 1 (dropWhile (/= ' ') s))

main = print $ morseDecode "**** * *-** *-** ---   *-- --- *-* *-** -** "