{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.ICalendar.Parser
    ( parseICalendar
    , parseICalendarFile
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWS          (runRWS)
import qualified Data.ByteString.Lazy as B
import           Data.Monoid
import           Prelude
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T

import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Pos
import Text.Parsec.Prim            hiding (many, (<|>))
import Text.Parsec.Text.Lazy       ()
import Text.ICalendar.Parser.Common
import Text.ICalendar.Parser.Components
import Text.ICalendar.Parser.Content
import Text.ICalendar.Types


-- | Parse a ByteString containing iCalendar data.
--
-- Returns either an error, or a tuple of the result and a list of warnings.
parseICalendar :: Text
               -> Either String ([VCalendar], [String])
parseICalendar txt = do
    a <- either (Left . show) Right $ parse parseToContent "" txt
    when (null a) $ throwError "Missing content."
    let xs = map (runCP . parseVCalendar) a
    (x, w) <- ((flip.).) flip foldM ([], []) xs $ \(x, ws) (g, (pos, _), w) ->
             case g of
                  Left e -> Left $ show pos ++ ": " ++ e
                  Right y -> Right (y:x, w <> ws)
    return (x, w)

-- | Parse an iCalendar file.
parseICalendarFile :: FilePath
                   -> IO (Either String ([VCalendar], [String]))
parseICalendarFile filepath = parseICalendar . T.decodeUtf8 <$> B.readFile filepath

runCP :: ContentParser a
      -> (Either String a, (SourcePos, [Content]), [String])
runCP = ((flip .) . flip) runRWS () (undefined, undefined) . runExceptT
