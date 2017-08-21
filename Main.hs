{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, Arrows #-} 
module Main where
import Text.XML.HXT.Core
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.HXT.XPath.Arrows
import Options.Applicative
import Data.List (intercalate)
import Data.Monoid

data Options = Options {
      parseHtml :: Bool
    , outputDelimiter :: String
    , nullOutput :: String
    , xpathBase :: String
    , childXPaths :: [String]
    } deriving Show

options :: Parser Options
options = Options 
    <$> flag yes no (long "xml" <> help "Parse as XML, rather than HTML.")
    <*> strOption (short 'F' <> metavar "OUTPUT-DELIM" <> help "Default \\t" <> value "\t")
    <*> strOption (short 'n' <> metavar "NULL-OUTPUT" <> help "Null value output string. Default \"\"" <> value "")
    <*> strArgument (metavar "BASE-XPATH")
    <*> many (strArgument (metavar "CHILD-XPATH"))

opts = info (helper <*> options)
            (header "xpathdsv" <> fullDesc 
            <> progDesc "Extract DSV data from HTML or XML with XPath"
            <> footer "See https://github.com/danchoi/xpathdsv for more information.")

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  let xpaths'' = mkXPaths nullOutput childXPaths
  r <- runX (readDocument [withValidate no, withWarnings no
                , withParseHTML parseHtml, withInputEncoding utf8] ""
            >>> extractDSV xpathBase xpaths''
            )
  mapM_ (putStrLn . intercalate outputDelimiter) $ concat r


mkXPaths :: ArrowXml a => String -> [String] -> a XmlTree [String]
mkXPaths n [] = mkXPaths n ["."]
mkXPaths nullOutput' xpaths =
    let arrows :: ArrowXml a => [a XmlTree String]
        arrows = map (\x -> (listA
                              (getXPathTrees x >>> getText' >>> cleanText)
                              >>^ concat >>> replaceNull nullOutput')
                            `orElse` constA nullOutput')
                      xpaths
    in listA (catA arrows)

getText' :: ArrowXml a => a XmlTree String
getText' = (isText >>> getText) 
           `orElse` (isAttr >>> getChildren >>> getText)
           `orElse` xshow this

cleanText :: ArrowXml a => a String String
cleanText = arr (T.unpack . T.concatMap escapeNewLines . T.strip . T.pack)

replaceNull nullOutput' = arr (\x -> if null x then nullOutput' else x)

escapeNewLines '\n' = " "
escapeNewLines '\r' = " "
escapeNewLines '\t' = " "
escapeNewLines x = T.singleton x



type Row = (String, [String]) -- output, one column minimum

extractDSV :: ArrowXml a => String -> a XmlTree [String] -> a XmlTree [[String]]
extractDSV xpath1 xpaths = 
    getXPathTrees xpath1 >>> listA xpaths
