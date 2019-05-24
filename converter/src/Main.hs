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
supportedFormatTypes = [Hex, Bin, Dec, Str]

complementList :: (Eq a) => a -> [a] -> [a]
complementList elem ls = filter (elem /=) ls

toBase :: (ReadS Int) -> Int -> String -> String
toBase readFunc base num = showIntAtBase base intToDigit (fst . head . readFunc $ num) ""

decToBase :: Int -> String -> String
decToBase = toBase readDec

hexToBase :: Int -> String -> String
hexToBase = toBase readHex

hexTo :: FormatType -> (String -> String)
hexTo Bin = hexToBase 2
hexTo Dec = hexToBase 10

decTo :: FormatType -> (String -> String)
decTo Bin = decToBase 2
decTo Hex = decToBase 16

undefTo _ = (\x -> "Conversion not implemented")

fromTo :: FormatType -> FormatType -> String -> String
fromTo Dec = decTo
fromTo Hex = hexTo
fromTo _ = undefTo

fmtFromTo :: FormatType -> FormatType -> NumFormat -> NumFormat
fmtFromTo fromType toType fromNumFmt =
    NumFormat { format = toType, content = (fromTo fromType toType) $ content $ fromNumFmt }

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
