import System.Environment
import Data.Char
-- import qualified Data.Set as S

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
supportedFormatTypes = [Hex, Bin, Str, Dec]

complementList :: (Eq a) => a -> [a] -> [a]
complementList elem ls = filter (elem /=) ls

main :: IO ()
main = do
    args <- getArgs
    let formats = map getNumFormat args
    let toPrintFormatsTypes = complementList (format $ head $ formats) supportedFormatTypes
    print toPrintFormatsTypes
    print formats
