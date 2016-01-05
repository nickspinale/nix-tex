module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

type MD5 = B.ByteString
type Arch = B.ByteString

data Package = Package String [String] CorePkg

data CorePkg = Bin { arch :: Arch
                   , md5  :: MD5
                   }
             | Texmf { relocated :: Bool
                     , run :: Maybe MD5
                     , src :: Maybe MD5
                     , doc :: Maybe MD5
                     }

-- key, value, and maybe files
data Entry = Entry String String (Maybe [String]) deriving 

entries :: Parser [Entry]
entries = do
    key <- many1 letter_ascii
    space
    value <- manyTill anyChar endOfLine
    -- let beFiles = (:) <$> fmap (Entry key value . Just) files <*> entries
    filesM <- fmap Just files <|> return Nothing
    return $ Entry key value filesM
  where
    files = many1 (space *> manyTill anyChar endOfLine)

keyIs :: String -> Entry -> Bool
keyIs key (Entry key' _ _) = key == key'

noFiles :: Entry -> Bool
noFiles (Entry _ _ Nothing) = True
noFiles _ = False

exactlyOne :: [a] -> Maybe a
exactlyOne [a] = Just a
exactlyOne  _  = Nothing

lookupOne :: String -> [Entry] -> Maybe String
lookupOne = ((.).(.)) exactlyOne lookupMany

lookupMany :: String -> [Entry] -> [String]
lookupMany key entries = [ value | Entry k value _, k == key ]

toPackage :: [Entry] -> Maybe Package
toPackage = Package <$> lookupOne "name"
                    <*> lookupMany "depend"
                    <*> (maybeBin <|> maybeTexmf)
  where
    maybeBin = Bin <$> lookupOne "arch"
                   <*> lookupOne "containermd5"
    maybeTexmf = Texmf <$> maybe (const True) false (lookupOne "relocated") 
                       <*> return (lookupOne "containermd5")
                       <*> return (lookupOne "srccontainermd5")
                       <*> return (lookupOne "doccontainermd5")

main :: IO ()
main = do
    putStrLn "tl: {"

    putStrLn "}"
