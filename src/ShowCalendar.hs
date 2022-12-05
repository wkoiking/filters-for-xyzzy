module ShowCalendar where

import Text.ICalendar.Parser
import Text.ICalendar.Types
import Data.Time
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Default
import Control.Monad
import Data.Maybe
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as S
import Data.List (find, intercalate)

showCalendar :: FilePath -> IO ()
showCalendar filepath = do
    eCalender <- parseICalendarFile filepath
    case eCalender of
        Right (calendars, errs) -> do
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
       mLocationStr = liftM (("    " ++) . showLocation) veLocation
       mUrlStr = liftM (("    " ++) . T.unpack . otherValue) $ find ((== CI.mk "X-MICROSOFT-SKYPETEAMSMEETINGURL") . otherName) $ S.toList veOther

showLocation :: Location -> String
showLocation Location{..} = T.unpack locationValue 

showSummary :: Summary -> String
showSummary Summary{..} = T.unpack summaryValue

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

showDay :: FormatTime t => t -> String
showDay = formatTime defaultTimeLocale "%m/%d"

japanTimeZone :: TimeZone
japanTimeZone = TimeZone
    { timeZoneMinutes = 60 * 9
    , timeZoneSummerOnly = False
    , timeZoneName = "JST"
    }
