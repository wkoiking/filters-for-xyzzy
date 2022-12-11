module ShowCalendar where

import Text.ICalendar.Parser
import Text.ICalendar.Types
import Data.Time
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad
import Data.Maybe
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as S
import Data.List (find, intercalate, isPrefixOf)
import Control.Applicative ((<|>))
import Data.Char (isSpace)

-- safe
import Safe

showCalendar :: FilePath -> IO ()
showCalendar filepath = do
    eCalender <- parseICalendarFile filepath
    case eCalender of
        Right (calendars, _errs) -> do
            mapM_ (putStrLn . showVCalender) calendars
--             putStrLn $ show $ length calendars
--             mapM_ putStrLn errs
        Left err -> putStrLn $ "Error: " ++ err

showVCalender :: VCalendar -> String
showVCalender calendar = unlinesEx $ map showVEvent $ M.elems $ vcEvents calendar

unlinesEx :: [String] -> String
unlinesEx = intercalate "\n" . filter (not . null) . map removeTrailingLF
 where removeTrailingLF :: String -> String
       removeTrailingLF str = reverse $ dropWhile (== '\n') $ reverse $ dropWhile (== '\n') str

showVEvent :: VEvent -> String
showVEvent VEvent{..} = unlinesEx $ catMaybes
    [ Just $ unwords [startTimeStr, summaryStr]
    , mLocationStr
    , mUrlStr
    ]
 where summaryStr = maybe "???" showSummary veSummary
       startTimeStr = maybe "mm/dd ??? HH:MM" showDTStart veDTStart
       mLocationStr = liftM (("    " ++)) $ showLocation =<< veLocation
       mUrlStr = liftM (("    " ++) . T.unpack) $ mUrlStrTeams <|> mUrlStrZoom
       mUrlStrTeams = liftM otherValue $ find ((== CI.mk "X-MICROSOFT-SKYPETEAMSMEETINGURL") . otherName) $ S.toList veOther
       mUrlStrZoom = veDescription >>= showZoomUrlFromDescription

showZoomUrlFromDescription :: Description -> Maybe Text
showZoomUrlFromDescription Description{..} = headMay $ mapMaybe takeURL $ T.lines descriptionValue
 where takeURL :: Text -> Maybe Text
       takeURL txt = find ("https://zoom" `T.isPrefixOf`) $ T.tails txt

showLocation :: Location -> Maybe String
showLocation Location{..}
    | "Microsoft Teams" `T.isInfixOf` locationValue = Nothing
    | "Online meeting" `T.isInfixOf` locationValue = Nothing
    | otherwise = Just $ T.unpack locationValue

showSummary :: Summary -> String
showSummary Summary{..} = removeFW $ T.unpack summaryValue

showDTStart :: DTStart -> String
showDTStart (DTStartDateTime dateTime _) = showDateTime dateTime
showDTStart (DTStartDate (Date day) _) = showDay day

showDateTime :: DateTime -> String
showDateTime (FloatingDateTime localTime) = showLocalTime localTime ++ " LT"
showDateTime (ZonedDateTime localTime timeZone) = unwords $ catMaybes
    [ Just $ showLocalTime localTime
    , replaceTokyoStandardTime $ T.unpack timeZone
    ]
showDateTime (UTCDateTime utcTime) = showLocalTime utcTime

showLocalTime :: FormatTime t => t -> String
showLocalTime = formatTime defaultTimeLocale "%m/%d %a %H:%M"

showUTCTime :: UTCTime -> String
showUTCTime t = formatTime defaultTimeLocale "%m/%d %a %H:%M" $ utcToLocalTime japanTimeZone t

replaceTokyoStandardTime :: String -> Maybe String
replaceTokyoStandardTime "Tokyo Standard Time" = Nothing
replaceTokyoStandardTime str = Just str

removeFW :: String -> String
removeFW str
    | "FW:" `isPrefixOf` str = dropWhile isSpace $ drop 3 str
    | otherwise = str

showDay :: FormatTime t => t -> String
showDay = formatTime defaultTimeLocale "%m/%d"

japanTimeZone :: TimeZone
japanTimeZone = TimeZone
    { timeZoneMinutes = 60 * 9
    , timeZoneSummerOnly = False
    , timeZoneName = "JST"
    }
