{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Parser.HttpVersion (httpVersionParser) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, satisfy)
import Text.Parser (ByteStringWithChars)
import Network.HTTP.Types (HttpVersion (..))
import Text.Megaparsec.Char (char, string)
import Data.Char (isDigit)
import qualified Data.ByteString.Char8 as BC

type HTTPVersionParser = Parsec Void ByteStringWithChars HttpVersion

httpVersionParser :: HTTPVersionParser
httpVersionParser = string "HTTP/" *> many (satisfy isDigit) >>= (\major -> char '.' *> many (satisfy isDigit) >>= (\minor -> pure $ HttpVersion (maybe (error "Unexpected") fst . BC.readInt $ BC.pack major) (maybe (error "Unexpected") fst . BC.readInt $ BC.pack minor)))
