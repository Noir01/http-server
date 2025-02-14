{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Parser (ByteStringWithChars (..)) where

import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NE (toList)
import Data.Proxy (Proxy (..))
import Text.Megaparsec.Stream (ShareInput (..), Stream (..), Token, Tokens, VisualStream (..))

newtype ByteStringWithChars = BS BC.ByteString

instance Stream (ShareInput ByteStringWithChars) where
  type Token (ShareInput ByteStringWithChars) = Char
  type Tokens (ShareInput ByteStringWithChars) = BC.ByteString
  tokenToChunk Proxy = BC.singleton
  tokensToChunk Proxy = BC.pack
  chunkToTokens Proxy = BC.unpack
  chunkLength Proxy = BC.length
  chunkEmpty Proxy = BC.null
  take1_ (ShareInput (BS s)) = second (ShareInput . BS) <$> BC.uncons s
  takeN_ n (ShareInput (BS s))
    | n <= 0 = Just (BC.empty, ShareInput (BS s))
    | BC.null s = Nothing
    | otherwise = Just . second (ShareInput . BS) $ BC.splitAt n s
  takeWhile_ p (ShareInput (BS s)) = second (ShareInput . BS) $ BC.span p s

instance Stream ByteStringWithChars where
  type Token ByteStringWithChars = Token (ShareInput ByteStringWithChars)
  type Tokens ByteStringWithChars = Tokens (ShareInput ByteStringWithChars)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput ByteStringWithChars))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput ByteStringWithChars))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

instance VisualStream ByteStringWithChars where
  showTokens Proxy = NE.toList