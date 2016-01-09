{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import           Data.Char (toLower)
import           Data.Maybe
import           Data.Monoid
import           System.IO

import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Pipes.Attoparsec as PA

data Package = Package String PkgBody deriving Show

data PkgBody = PkgBody { relocated :: Bool
                       , depend :: [String]
                       , containermd5 :: Maybe String
                       , srccontainermd5 :: Maybe String
                       , doccontainermd5 :: Maybe String
                       } deriving Show

-- key, value, and maybe files
data Entry = Entry String String [String] deriving Show

buildPackage :: Package -> Builder
buildPackage (Package name (PkgBody{..})) = mconcat $ map ((indent <>) . (<> char7 '\n')) lines
  where
    indent = string7 "  "
    equals = string7 " = "
    quote str = char7 '"' <> string7 str <> char7 '"'
    lines = [quote name <> equals <> char7 '{']
         ++ map ((indent <>) . (<> char7 ';')) rest
         ++ [string7 "};"]
    rest = [ string7 "relocated" <> equals <> string7 (map toLower $ show relocated)
           , string7 "depend" <> equals <> char7 '[' <> deps <> char7 ']'
           ] ++ md5 "containermd5" containermd5
             ++ md5 "srccontainermd5" srccontainermd5
             ++ md5 "doccontainermd5" doccontainermd5
    md5 str = maybeToList . fmap (\m -> string7 str <> equals <> quote m)
    deps = foldr (\l r -> quote (archify l) <> char7 ' '  <> r) mempty depend
    archify str = case splitAt (length str - 4) str of
        (left, "ARCH") -> left ++ "${arch}"
        _ -> str

entry :: Parser Entry
entry = Entry <$> (many1 (satisfy inValue) <* char ' ')
              <*> manyTill anyChar endOfLine
              <*> many' (char ' ' *> manyTill anyChar (char '\n'))
  where
    inValue = (||) <$> isDigit <*> ((||) <$> isAlpha_ascii <*> (==) '-')

exactlyOne :: [a] -> Maybe a
exactlyOne [a] = Just a
exactlyOne  _  = Nothing

lookupOne :: String -> [Entry] -> Maybe String
lookupOne = ((.).(.)) exactlyOne lookupMany

lookupMany :: String -> [Entry] -> [String]
lookupMany key entries = [ value | Entry k value _ <- entries, k == key ]

toPkgBody :: [Entry] -> PkgBody
toPkgBody = PkgBody <$> (maybe False (const True) <$> (lookupOne "relocated"))
                    <*> lookupMany "depend"
                    <*> lookupOne "containermd5"
                    <*> lookupOne "srccontainermd5"
                    <*> lookupOne "doccontainermd5"

toPackage :: [Entry] -> Maybe Package
toPackage ((Entry "name" name _):rest) = Just . Package name $ toPkgBody rest
toPackage _ = Nothing

main :: IO ()
main = do
    putStrLn "arch: {"
    maybe (return ()) (hPutStrLn stderr) <$> PP.evalStateT parser PB.stdin
    putStrLn "}"
  where
    parser :: PP.Parser B.ByteString IO (Maybe String)
    parser = do
        parsed <- PA.parse (many1 entry <* endOfLine)
        case parsed of
            Nothing -> return Nothing
            Just (Left (PA.ParsingError _ msg)) -> return (Just msg)
            Just (Right entries) -> case toPackage entries of
                Nothing -> return (Just "malformed package")
                Just pkg -> PP.lift (hPutBuilder stdout (buildPackage pkg)) >> parser

-- main :: IO ()
-- main = putStrLn "arch: {" >> getPackages' >> putStrLn "}"
--   where
--     getPackages' = getPackages (Partial (parse package))
--     getPackages (Fail _ _ reason) = putStrLn reason
--     getPackages (Partial f) = do
--         eof <- hIsEOF stdin
--         if eof
--          then return ()
--          else do
--             line <- B.hGetLine stdin
--             getPackages (f (line <> C.pack "\n"))
--     getPackages (Done i r) = do
--         hPutBuilder stdout (buildPackage r)
--         getPackages'

-- package :: Parser Package
-- package = do
--     entries <- many' entry
--     case toPackage entries of
--         Just package -> return package
--         Nothing -> fail "package had no name"

