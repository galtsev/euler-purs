module E19 where

import Prelude

type Year = Int
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

data Date = Date Year Month Int

start:: Date
start = Date 1901 Jan 2

isSunday:: Date -> Boolean
isSunday (Date _ _ c) = mod c 7 == 0

isLeap:: Year -> Boolean
isLeap y | mod y 400 == 0 = true
isLeap y | mod y 100 == 0 = false
isLeap y | mod y 4 == 0 = true
isLeap _ = false

incDate:: Date -> Date
incDate (Date y Jan c) = Date y Feb (c+31)
incDate (Date y Feb c) = Date y Mar (c + if isLeap y then 29 else 28)
incDate (Date y Mar c) = Date y Apr (c+31)
incDate (Date y Apr c) = Date y May (c+30)
incDate (Date y May c) = Date y Jun (c+31)
incDate (Date y Jun c) = Date y Jul (c+30)
incDate (Date y Jul c) = Date y Aug (c+31)
incDate (Date y Aug c) = Date y Sep (c+31)
incDate (Date y Sep c) = Date y Oct (c+30)
incDate (Date y Oct c) = Date y Nov (c+31)
incDate (Date y Nov c) = Date y Dec (c+30)
incDate (Date y Dec c) = Date (y+1) Jan (c+31)

collect:: Year -> Date -> Int -> Int
collect lastYear d@(Date y _ c) cnt | y<lastYear = collect lastYear (incDate d) (if isSunday d then cnt+1 else cnt)
collect _ _ cnt = cnt

collect2:: Year -> Date -> Int
collect2 lastYear d = cc d 0
    where
        cc dt@(Date y _ c) cnt | y<lastYear = cc (incDate dt) (if isSunday dt then cnt+1 else cnt)
        cc _ cnt = cnt

e19:: Unit -> Int
e19 unit = collect 2001 start 0

e19':: Unit -> Int
e19' unit = collect2 2001 start