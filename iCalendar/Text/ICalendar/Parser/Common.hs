{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Text.ICalendar.Parser.Common where

import           Control.Applicative
import           Control.Arrow                (second)
import           Control.Monad.Except
import           Control.Monad.RWS            (MonadState (get, put),
                                               MonadWriter (tell), RWS,
                                               modify)
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Char
import           Data.Default
import           Data.List                    (partition)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import           Data.Time                    (Day, LocalTime (LocalTime),
                                               TimeOfDay (), UTCTime (UTCTime))
import qualified Data.Time                    as Time
import qualified Network.URI                  as URI
import           Prelude                      hiding (mapM)

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import qualified Text.Parsec.Text.Lazy  as P
import qualified Text.Parsec            as P
import           Text.Parsec.Combinator hiding (optional)
import           Text.Parsec.Prim       hiding ((<|>))

import Text.ICalendar.Types

-- | Content lines, separated into components. 3.1.
data Content = ContentLine P.SourcePos (CI Text) [(CI Text, [Text])] Text
             | Component P.SourcePos (CI Text) [Content]
               deriving (Show, Eq, Ord)

type TextParser = P.Parser

type ContentParser = ExceptT String -- Fatal errors.
                            (RWS ()
                                 [String] -- Warnings.
                                 (P.SourcePos, [Content]))

-- | Parse text. 3.3.11
parseText' :: Text -> ContentParser ([Text], Text)
parseText' txt = do case runParser ((,) <$> texts <*> getInput) () "text" txt of
                         Left e -> throwError $ "parseText': " ++ show e
                         Right (x, r) -> return (x, r)
  where texts = sepBy1 text (P.char ',') <|> return [mempty]
        text = do x <- P.satisfy isTSafe'
                  case x of
                       '\\' -> do y <- P.anyChar
                                  case y of
                                       '\\' -> nxt '\\'
                                       ';' -> nxt ';'
                                       ',' -> nxt ','
                                       z | z `elem` ['n','N'] -> nxt '\n'
                                       _ -> fail $ "unexpected " ++ show x
                       y -> nxt y
        -- isTSafe + 0x22, 0x3A, and 0x5C is pattern matched against.
        isTSafe' c = let n = ord c
                      in n == 9 || (n >= 0x20 && n <= 0x2B)
                                || (n >= 0x2D && n <= 0x3A)
                                || (n >= 0x3C && n /= 0x7F)
        nxt c = liftM (T.cons c) $ text <|> return mempty

-- | Chech that there's no remainding text after the parser is done.
noRestText :: ([Text], Text) -> ContentParser [Text]
noRestText (x, "") = return x
noRestText (_, x) = throwError $ "noRestText: remainding text: " ++ show x

-- | Parse text, not allowing any remainding text.
parseText :: Text -> ContentParser [Text]
parseText = noRestText <=< parseText'

-- | Parse a DateTime value. 3.3.5
parseDateTime :: Maybe Text -- ^ Time Zone ID
              -> Text -> ContentParser DateTime
parseDateTime mTZ txt = do
    let str = T.unpack txt
        dayRes = parseDateStr str
        Just (day, rest') = dayRes
        t = take 1 rest'
        timeRes = parseTimeStr $ drop 1 rest'
        Just (time, isUTC) = timeRes
    when (isNothing (dayRes >> timeRes) || t /= "T") .
        throwError $ "parseDateTime: " ++ str
    when (isUTC && isJust mTZ) $
        tell ["parseDateTime: TZID on UTC timezone: " ++ str]
    return $ case (mTZ, isUTC) of
                  (Nothing, False) -> FloatingDateTime (LocalTime day time)
                  (Just tz, False) -> ZonedDateTime (LocalTime day time) tz
                  (_, True) -> UTCDateTime (UTCTime day
                                                  $ Time.timeOfDayToTime time)

-- | Parse a string to a Day. 3.3.4
parseDateStr :: String -> Maybe (Day, String)
parseDateStr = lastToMaybe . Time.readSTime True defaultTimeLocale "%Y%m%d"

-- | Parse a string to a TimeOfDay, and a bool if it's in UTC.
parseTimeStr :: String -> Maybe (TimeOfDay, Bool)
parseTimeStr s = do
    (t, r) <- lastToMaybe (Time.readSTime True defaultTimeLocale "%H%M%S" s)
    case r of
         "Z" -> return (t, True)
         "" -> return (t, False)
         _ -> fail ""

-- | Parse a Date value. 3.3.4
parseDate :: Text -> ContentParser Date
parseDate txt = do
    let str = T.unpack txt
    let dayRes = parseDateStr str
        Just (day, rest) = dayRes
    when (isNothing dayRes) .
        throwError $ "parseDate: " ++ str
    unless (null rest) $
        tell ["parseDate: extra content: " ++ rest]
    return $ Date day

-- {{{ Misc parsers

parseURI :: String -> ContentParser URI.URI
parseURI s = case URI.parseURI s of
                  Just x -> return x
                  Nothing -> throwError $  "Invalid URI: " ++ show s

-- | Convert a 'DateTime' to 'UTCTime', giving an appropriate error.
mustBeUTC :: DateTime -> ContentParser UTCTime
mustBeUTC (UTCDateTime x) = return x
mustBeUTC _ = throwError "DateTime-value must be UTC"

-- | Parse something simple with only a Text-field for the content, and
-- 'OtherParams'.
parseSimple :: (Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimple k (ContentLine _ _ o bs) = do c <- valueOnlyOne =<< parseText bs
                                          return $ k c (toO o)
parseSimple _ x = throwError $ "parseSimple: " ++ show x

-- | Parse something simple with only a CI Text-field for the content, and
-- 'OtherParams'.
parseSimpleI :: (CI Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimpleI k (ContentLine _ _ o txt) = return $ k (CI.mk txt) (toO o)
parseSimpleI _ x = throwError $ "parseSimpleI: " ++ show x

-- | Parse something simple with only a Int-field for the content, and
-- 'OtherParams'.
parseSimpleRead :: forall a b. Read a
                => (a -> OtherParams -> b) -> Content -> ContentParser b
parseSimpleRead k (ContentLine _ _ o txt) = do
    let r = maybeRead $ T.unpack txt :: Maybe a
    when (isNothing r) . throwError $ "parseSimpleRead: " ++ show txt
    return $ k (fromJust r) (toO o)
parseSimpleRead _ x = throwError $ "parseSimpleRead: " ++ show x

-- | Parse something b with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLang' :: ([Text] -> ContentParser b)
                 -> (b -> Maybe URI.URI -> Maybe Language -> OtherParams -> a)
                 -> Content -> ContentParser a
parseAltRepLang' m f (ContentLine _ _ o txt) = do
    t <- m =<< parseText txt
    uri <- mapM (parseURI <=< paramOnlyOne) $ T.unpack .: lookup "ALTREP" o
    lang <- mapM paramOnlyOne $ Language . CI.mk .: lookup "LANGUAGE" o
    let o' = filter (\(x, _) -> x `notElem` ["ALTREP", "LANGUAGE"]) o
    return $ f t uri lang (toO o')
parseAltRepLang' _ _ x = throwError $ "parseAltRepLang': " ++ show x

-- | Parse something 'Text' with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLang :: (Text -> Maybe URI.URI -> Maybe Language -> OtherParams -> a)
                -> Content -> ContentParser a
parseAltRepLang = parseAltRepLang' lenientTextOnlyOne
  where lenientTextOnlyOne :: [Text] -> ContentParser Text
        lenientTextOnlyOne [x] = return x
        lenientTextOnlyOne [] = throwError "Must have one value, not zero."
        lenientTextOnlyOne xs = do
            tell ["Illegal comma in value that only allows one TEXT, assuming literal comma was intended."]
            return $ T.intercalate "," xs

-- | Parse something '[Text]' with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLangN :: (Set Text -> Maybe URI.URI -> Maybe Language
                              -> OtherParams -> a)
                -> Content -> ContentParser a
parseAltRepLangN = parseAltRepLang' (return . S.fromList)

-- | Parse something simple with only a URI-field for the content, and
-- 'OtherParams'.
parseSimpleURI :: (URI.URI -> OtherParams -> a) -> Content -> ContentParser a
parseSimpleURI f (ContentLine _ _ o txt) = do
    uri <- parseURI $ T.unpack txt
    return . f uri $ toO o
parseSimpleURI _ x = throwError $ "parseSimpleURI: " ++ show x


-- | Parse something which has either a 'Date' or a 'DateTime' value, and
-- 'OtherParams'. Uses DateTime if there is no value parameter.
parseSimpleDateOrDateTime :: (DateTime -> OtherParams -> a)
                          -> (Date     -> OtherParams -> a)
                          -> Content
                          -> ContentParser a
parseSimpleDateOrDateTime dt d (ContentLine _ _ o txt) = do
    (typ, tzid, o') <- typTzIdO o
    case typ of
         "DATE-TIME" -> do x <- parseDateTime tzid txt
                           return . dt x $ toO o'
         "DATE" -> do x <- parseDate txt
                      return . d x $ toO o'
         _ -> throwError $ "Invalid type: " ++ show typ
parseSimpleDateOrDateTime _ _ x =
    throwError $ "parseSimpleDateOrDateTime: " ++ show x

-- | Parse something which has a set of either a 'Date' or a 'DateTime' value,
-- and 'OtherParams'. Uses DateTime if there is no value parameter.
parseSimpleDatesOrDateTimes :: (Set DateTime -> OtherParams -> a)
                            -> (Set Date     -> OtherParams -> a)
                            -> Content
                            -> ContentParser a
parseSimpleDatesOrDateTimes dt d (ContentLine _ _ o txt) = do
    (typ, tzid, o') <- typTzIdO o
    case typ of
         "DATE-TIME" -> do x <- S.fromList .: mapM (parseDateTime tzid) $
                                                T.split (== ',') txt
                           return . dt x $ toO o'
         "DATE" -> do x <- S.fromList .: mapM parseDate $ T.split (== ',') txt
                      return . d x $ toO o'
         _ -> throwError $ "Invalid type: " ++ show typ
parseSimpleDatesOrDateTimes _ _ x =
    throwError $ "parseSimpleDatesOrDateTimes: " ++ show x

typTzIdO :: [(CI Text, [Text])]
         -> ContentParser (Text, Maybe Text, [(CI Text, [Text])])
typTzIdO o = do
    typ <- paramOnlyOne . fromMaybe ["DATE-TIME"] $ lookup "VALUE" o
    tzid <- mapM paramOnlyOne $ if typ == "DATE-TIME" then lookup "TZID" o
                                                      else Nothing
    let f x = x /= "VALUE" && (typ /= "DATE-TIME" || x /= "TZID")
        o' = filter (f . fst) o
    return (typ, tzid, o')

-- | Parse something which has only a DateTime value, and 'OtherParams'.
parseSimpleDateTime :: (DateTime -> OtherParams -> a)
                    -> Content
                    -> ContentParser a
parseSimpleDateTime dt (ContentLine _ _ o txt) = do
    tzid <- mapM paramOnlyOne $ lookup "TZID" o
    let o' = filter ((/="TZID") . fst) o
    flip dt (toO o') <$> parseDateTime tzid txt
parseSimpleDateTime _ x = throwError $ "parseSimpleDateTime: " ++ show x

parseSimpleUTC :: (UTCTime -> OtherParams -> a)
               -> Content
               -> ContentParser a
parseSimpleUTC dt (ContentLine _ _ o txt) =
    flip dt (toO o) <$> (mustBeUTC =<< parseDateTime Nothing txt)
parseSimpleUTC _ x = throwError $ "parseSimpleUTC: " ++ show x

-- | Convert a property dictionary to 'OtherParams'.
toO :: [(CI Text, [Text])] -> OtherParams
toO = OtherParams . S.fromList . map (uncurry OtherParam)

-- | Get the remaining properties.
otherProperties :: ContentParser (Set OtherProperty)
otherProperties = do opts <- snd <$> get
                     modify (second $ const [])
                     S.fromList <$> mapM lineToOtherProp opts
  where lineToOtherProp (ContentLine _ n opts txt) =
            return (OtherProperty n txt $ toO opts)
        lineToOtherProp c@Component {} =
            down c . throwError $ "Unconsumed component: " ++ show c

neg :: TextParser (Int -> Int)
neg = maybe id (\x -> if x == '-' then negate else id)
            <$> optional (P.oneOf "+-")

digits :: TextParser Int
digits = foldl1 ((+).(*10)) . map digitToInt <$> many1 P.digit

digitsN :: TextParser [Int]
digitsN = sepBy1 digits (P.char ',')

-- }}}

-- | Set the parser context.
down :: Content -> ContentParser a -> ContentParser a
down (Component p _ x) = down' (p, x)
down x@(ContentLine p _ _ _) = down' (p, [x])

-- | Set the parser context.
down' :: (P.SourcePos, [Content]) -> ContentParser a -> ContentParser a
down' x m = get >>= \old -> put x >> m <* put old

-- | Many optional components named ...
optCompN :: Ord a
         => CI Text -> (Content -> ContentParser a) -> ContentParser (Set a)
optCompN s f = optN f . partition (`isComponentNamed` s) =<< snd <$> get

-- | One required line named ...
reqLine1 :: CI Text -> (Content -> ContentParser a) -> ContentParser a
reqLine1 s f = req1 s f . partition (`isLineNamed` s) =<< snd <$> get

-- | One optional line named ...
optLine1 :: Default b
         => CI Text -> (Content -> ContentParser b) -> ContentParser b
optLine1 s f = opt1 f . partition (`isLineNamed` s) =<< snd <$> get

-- | Many optional lines named ...
optLineN :: Ord b
         => CI Text -> (Content -> ContentParser b) -> ContentParser (Set b)
optLineN s f = optN f . partition (`isLineNamed` s) =<< snd <$> get

-- | Many lines named ..., at least one required.
reqLineN :: Ord b
         => CI Text -> (Content -> ContentParser b) -> ContentParser (Set b)
reqLineN s f = reqN s f . partition (`isLineNamed` s) =<< snd <$> get

-- | One required ...
req1 :: CI Text -> (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser b
req1 _ f ([x], xs) = modify (second $ const xs) >> down x (f x)
req1 s _ ([], _) = throwError $ "Missing content: " ++ show s
req1 _ f (x:xs, xs') = do modify (second $ const xs')
                          tell (map (("Extra content: " ++) . show) xs)
                          down x $ f x

-- | One optional ...
opt1 :: Default b
     => (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser b
opt1 f ([x], xs) = modify (second $ const xs) >> down x (f x)
opt1 _ ([], _) = return def
opt1 f (x:xs, xs') = do modify (second $ const xs')
                        tell (map (("Extra content: " ++) . show) xs)
                        down x $ f x

-- | Many optional ...
optN :: Ord b
     => (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser (Set b)
optN f (xs, xs') = do modify (second $ const xs')
                      S.fromList <$> mapM (\x -> down x (f x)) xs

-- | Many ..., at least one required.
reqN :: Ord b
     => CI Text -- ^ What, needed for the error.
     -> (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser (Set b)
reqN w f (xs, xs') = do modify (second $ const xs')
                        o <- S.fromList <$> mapM (\x -> down x (f x)) xs
                        when (S.size o < 1) .
                            throwError $ "At least one required: " ++ show w
                        return o

-- | Only allow one parameter value.
paramOnlyOne :: [a] -> ContentParser a
paramOnlyOne [x] = return x
paramOnlyOne _ = throwError "Only one parameter value allowed."

valueOnlyOne :: [a] -> ContentParser a
valueOnlyOne [x] = return x
valueOnlyOne [] = throwError "Must have one value, not zero."
valueOnlyOne _ = throwError "Only one value allowed."

-- | Line predicate.
isLineNamed :: Content -> CI Text -> Bool
isLineNamed (ContentLine _ n _ _) n' | n == n' = True
isLineNamed _ _ = False

-- | Component name predicate.
isComponentNamed :: Content -> CI Text -> Bool
isComponentNamed (Component _ n _) n' | n == n' = True
isComponentNamed _ _ = False

isComponent :: Content -> Bool
isComponent Component {} = True
isComponent _ = False

-- Util

maybeRead :: Read a => String -> Maybe a
maybeRead = fst .: lastToMaybe . reads

lastToMaybe :: [a] -> Maybe a
lastToMaybe x = if null x then Nothing else Just $ last x

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap fmap fmap

infixl 4 .:
