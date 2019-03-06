import System.Environment
import Data.Char
import Numeric
import Data.Digits -- FIXME: additional package, use cabal/stack

-- FIXME: error management missing

data FormatType = Hex | Bin | Str | Dec | Unrecognized deriving(Show)

instance Eq FormatType where
    Hex == Hex = True
    Bin == Bin = True
    Str == Str = True
    Dec == Dec = True
    _== _  = False

data NumFormat = NumFormat { format :: FormatType
                           , content :: String
                           } deriving (Show)

getNumFormat :: String -> NumFormat
getNumFormat(x:xs)
  | x == 'x'            = NumFormat { format = Hex, content = xs}
  | x == 'b'            = NumFormat { format = Bin, content = xs}
  | isDigit x           = NumFormat { format = Dec, content = x:xs}
  | isPunctuation x     = NumFormat { format = Str, content = x:xs}
  | otherwise           = NumFormat { format = Unrecognized, content = x:xs}

supportedFormatTypes :: [FormatType]
supportedFormatTypes = [Hex, Bin, Dec]

complementList :: (Eq a) => a -> [a] -> [a]
complementList elem ls = filter (elem /=) ls

toBase :: (ReadS Int) -> Int -> String -> String
toBase readFunc base num = showIntAtBase base intToDigit (fst . head . readFunc $ num) ""

hexToBase :: Int -> String -> String
hexToBase base hex = toBase readHex base hex

-- example of a conversion from hex to bin
hexToBin :: String -> String
hexToBin hex = hexToBase 2 hex

hexToDec :: String -> String
hexToDec hex = hexToBase 10 hex

fmtFromHexTo :: NumFormat -> FormatType -> NumFormat
fmtFromHexTo numFmt fmtHex = NumFormat { format = numFmt, content = hexToDec $ content $ fmtHex }

fmtFromHexToDec :: NumFormat -> NumFormat
fmtFromHexToDec fmtHex = NumFormat { format = Dec, content = hexToDec $ content $ fmtHex }

fmtFromHexToBin :: NumFormat -> NumFormat
fmtFromHexToBin fmtHex = NumFormat { format = Bin, content = hexToBin $ content $ fmtHex }

main :: IO ()
main = do
    args <- getArgs
    let formats = map getNumFormat args
    let toPrintFormatsTypes = complementList (format $ head $ formats) supportedFormatTypes
    -- print toPrintFormatsTypes
    -- print formats
    putStr "(Bin)\t\t(Dec)\n"
    putStr (hexToBin (content $ head $ formats))
    putStr "\t"
    putStr (hexToDec (content $ head $ formats))
    putStr "\n"
