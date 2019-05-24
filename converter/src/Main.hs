module Main where

import System.Environment
import Data.Char
import Numeric
import Data.Digits
import System.Console.ANSI
import Text.Layout.Table

import Data.Colour.SRGB (sRGB24)

-- FIXME: error management missing

data FormatType = Hex | Bin | Str | Dec | Unrecognized deriving(Show)

instance Eq FormatType where
    Hex == Hex = True
    Bin == Bin = True
    Str == Str = True
    Dec == Dec = True
    _ == _ = False

data NumFormat = NumFormat { format :: FormatType
                           , content :: String
                           }

instance Show NumFormat where
    show (NumFormat fmtType xs) = " (" ++ show fmtType ++ ")" ++ ": " ++ show xs ++ " "

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

decToBase :: Int -> String -> String
decToBase base dec = toBase readDec base dec

hexToBase :: Int -> String -> String
hexToBase base hex = toBase readHex base hex

-- example of a conversion from hex to bin
hexToBin :: String -> String
hexToBin hex = hexToBase 2 hex

hexToDec :: String -> String
hexToDec hex = hexToBase 10 hex

hexTo :: FormatType -> (String -> String)
hexTo Bin = hexToBin
hexTo Dec = hexToDec

decToBin :: String -> String
decToBin dec = decToBase 2 dec

decToHex :: String -> String
decToHex dec = decToBase 16 dec

decTo :: FormatType -> (String -> String)
decTo Bin = decToBin
decTo Hex = decToHex

undefTo _ = (\x -> "Conversion not implemented")

fromTo :: FormatType -> FormatType -> String -> String
fromTo Dec = decTo
fromTo Hex = hexTo
fromTo _ = undefTo

fmtFromTo :: FormatType -> FormatType -> NumFormat -> NumFormat
fmtFromTo fromType toType fromNumFmt =
    NumFormat { format = toType, content = (fromTo fromType toType) $ content $ fromNumFmt }

-- fmtFromDecTo :: FormatType -> NumFormat -> NumFormat
-- fmtFromDecTo numFmt fmtDec = fmtFromTo Dec numFmt fmtDec

-- fmtFromHexTo :: FormatType -> NumFormat -> NumFormat
-- fmtFromHexTo numFmt fmtHex = fmtFromTo Hex numFmt fmtHex

-- fmtFromHexToDec :: NumFormat -> NumFormat
-- fmtFromHexToDec fmtHex = fmtFromHexTo Dec fmtHex

-- fmtFromHexToBin :: NumFormat -> NumFormat
-- fmtFromHexToBin fmtHex = fmtFromHexTo Bin fmtHex

composePrintFmts :: [FormatType] -> NumFormat -> [NumFormat]
composePrintFmts (x:xs) fmt = fmtFromTo (format fmt) x fmt : composePrintFmts xs fmt
composePrintFmts [] fmt = []

main :: IO ()
main = do
    args <- getArgs
    let formats = map getNumFormat args
    let srcFmt = format $ head $ formats
    let srcContent = content $ head $ formats
    let toPrintFormatsTypes = complementList srcFmt supportedFormatTypes
    -- putStrLn $ (show (composePrintFmts toPrintFormatsTypes (NumFormat srcFmt srcContent)))
    setSGR [SetColor Foreground Vivid White]
    setSGR [SetColor Background Vivid Blue]
    -- setSGR [SetRGBColor Foreground $ sRGB24 0 0 0]

    putStr (show $ NumFormat srcFmt srcContent)
    setSGR [SetRGBColor Foreground $ sRGB24 0 0 0]
    -- setSGR [SetColor Foreground Vivid Yellow]
    setSGR [SetColor Background Vivid Black]
    setSGR [SetConsoleIntensity BoldIntensity]
    mapM_ putStr (map show (composePrintFmts toPrintFormatsTypes (NumFormat srcFmt srcContent)))
    putStrLn ""
    setSGR [Reset]

    {--
    putStrLn $ tableString [def , numCol]
                       unicodeRoundS
                       def
                       [ rowG $ map show $ composePrintFmts toPrintFormatsTypes (NumFormat srcFmt srcContent) ]

    putStrLn $ tableString [fixedLeftCol 10, column (fixed 10) center dotAlign def]
                        unicodeS
                        (titlesH ["Text", "Number"])
                        [ rowG ["A very long text", "0.42000000"]
                        , rowG ["Short text", "100200.5"]
                        ]
--}
